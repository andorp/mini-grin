{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, ConstraintKinds #-}
module Tutorial.Chapter01.Exercise02 where

import Data.Int
import Data.Word
import Data.Maybe
import Grin.Exp
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.RWS.Strict (RWST(..))

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import Grin.Interpreter.Store (Store)
import qualified Grin.Interpreter.Store as Store
import qualified Grin.Value as Grin
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

{-
Motivation:
To give a language meaning one could write an interpreter.

Using the interpreter we can define the operational semantics
of a language. The interpreter can also be regarded as a state
transition system over a given abstract domain.

This approach is called the DEFINITIONAL INTERPRETER.

Our domain consist of
  * an Environment, which associates variables with values, AND
  -- NOTE: consider renamind Store -> (Abstract) Heap [to avoid confusion about store operation]
  * a Store (Heap) which represents the memory of the machine. The
    store associates heap locations (Addresses) with values.

Exercise: Read the Grin.Interpreter.Env module
Exercise: Read the Grin.Interpreter.Store module

During the interpretation
 * The environment associates variables with values
   which can be of three types:
    * Primitive (SValue)
    * Node
    * Unit
   Values of type Unit can be created by an Update operation,
   or an effectful external operation.
 * The store can only hold Node values (similar to C-style structs)

Note:
GRIN programs are in Static Single Assignment form,
which means rebinding a variable is illegal.


-- NOTE: extract this to somewhere in the beginning
GRIN stands for Graph Reduction Intermediate Notation, and is
a compiler back end for functional languages. As its name
suggests, GRIN can be used to express graph reduction semantics
and hence can be used to compile functional languages.

In GRIN, a node in the graph is represented as a C-stlye struct.
The Heap can only contain Node values and nothing else.
These Node values stored on the Heap are the nodes of the functional program's
graph. The reduction of this graph is done through the primitive
heap operations of GRIN (store, fetch, update).

Exercise: Read the definition of Value, Node and SValue types below. These types represent
values in a running interpreter.
-}

data Value       -- A runtime value can be:
  = Prim SValue  -- A primitive value as simple value
  | Node Node    -- A node value which represents a node in the graph
  | Unit         -- The UNIT value, which represents no information at all. Like () in Haskell.
  deriving (Eq, Show)

data Node = N { tag :: Grin.Tag, args :: [SValue] }
  deriving (Eq, Show)

data SValue
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SChar   Char
  | SLoc    Address
  deriving (Eq, Ord, Show)

-- | For simplicity's sake, we will represent addresses using Ints.
type Address = Int

{-
The structure of the interpreter can be represented as a Monad Transformer, which
operates on a given 'm' Monad, which can run arbitrary IO computations.

The Env represents the frame, which holds values for variables. The MonadReader abstraction
fits well with the frame abstraction.

-- NOTE: name of operations first
The Store associates Addresses with Node values, during the execution of the program,
contents of the heap location associated with the given address can change.
 * A new address is allocated using the Store operation, which creates a new heap
   location saves the value of which was given to the Store operation via a variable
   which value must be looked up from the Env.
   The store operation returns the newly created address.
 * The content of a given address can be retrieved using the Fetch operation. The
   parameter of the operation is a variable which holds an address value.
 * The content of an address can be overwritten using the Update operation.
-}

type InterpretExternal = Map.Map Grin.Name ([Value] -> IO Value)

-- NOTE: more descriptive name
data Functions = Functions
  { functions :: Map.Map Grin.Name Exp
  , externals :: InterpretExternal
  }


-- NOTE: Reader env can change
-- NOTE: IO is neeed to call externals
newtype Definitional m a =
    Definitional
      (RWST -- Reader Writer State Monad Transformer
        (Functions, Env Value) -- Reader on the Functions and the variable value mapping
        ()                     -- Empty writer: No logging happens at all
        (Store Address Node)   -- State is the Store which can be change during the interpretation
        m
        a
      )
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Functions, Env Value)
    , MonadState (Store Address Node)
    , MonadFail
    )

type DC m = (Monad m, MonadIO m, MonadFail m)

{-
During the execution of a GRIN program, the interpeter
needs a context to interpret function calls.

Firstly, it needs to know all the functions defined in the GRIN program.

Furthermore, it needs to know how to call external functions (system/OS functions).
This is accomplished by `externalCall`. This function will be used to interpret
external function calls (in SApp). Given an external function's name, and the
actual arguments to the call, it calls the corresponding system/OS function.
-}



-- The interpreter function gets how to interpret the external functions,
-- a program to interpret, and returns a computed value.
--
-- It collects the function definitions from the program,
-- loads the body of the main function and starts to evaluate that expression.
interpreter :: InterpretExternal -> Program -> IO Value
interpreter iext prog =
    fst <$> runInterpreter (eval (grinMain prog))
  where
    runInterpreter :: (Monad m, MonadIO m, MonadFail m) => Definitional m a -> m (a, Store Address Node)
    runInterpreter (Definitional r) = do
      let funs = Functions (programToDefs prog) iext
      (a,store,()) <- runRWST r (funs, Env.empty) Store.empty
      pure (a,store)

-- | Turns a Simple Value from a syntax to a simple value of the semantics.
simpleValue :: Grin.SimpleValue -> SValue
simpleValue = \case
  Grin.SInt64  s -> SInt64  s
  Grin.SWord64 s -> SWord64 s
  Grin.SFloat  s -> SFloat  s
  Grin.SBool   s -> SBool   s
  Grin.SChar   s -> SChar   s

-- | Looks up a name from the active frame/environment and returns its value.
valueOf :: (DC m) => Grin.Name -> Definitional m Value
valueOf name = asks ((`Env.lookup` name) . snd)

-- | Looks up a name from the active frame/environment and returns its value, expecting a simple value.
svalueOf :: (DC m) => Grin.Name -> Definitional m SValue
svalueOf name = do
  (Prim sv) <- valueOf name
  pure sv

-- | Creates a new location that can be used in the Store operation.
alloc :: (DC m) => Definitional m Address
alloc = gets Store.size



eval :: (DC m) => Exp -> Definitional m Value
eval = \case
  SPure (Grin.Val l) -> value l
  SPure (Grin.Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    ext <- isExternal fn
    (if ext then external else funCall eval) fn vs

  SFetch n -> do
    p <- askEnv
    let v = Env.lookup p n
    findStore v

  SUpdate nl nn -> do
    p <- askEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    extStore vl vn
    unit

  ECase n alts -> do
    p <- askEnv
    v <- pure $ Env.lookup p n
    -- Select the alternative and continue the evaluation
    evalCase eval v alts

  EBind (SStore n) (BVar l) rhs -> do
    p <- askEnv
    let v = Env.lookup p n
    a  <- allocStore l
    extStore a v
    let p' = Env.insert [(l, a)] p
    localEnv p' $ eval rhs

  EBind lhs (BVar n) rhs -> do
    v <- eval lhs
    p <- askEnv
    let p' = Env.insert [(n, v)] p
    localEnv p' (eval rhs)

  EBind lhs (BNodePat t@(Grin.Tag{}) vs) rhs -> do
    v   <- eval lhs
    p   <- askEnv
    p'  <- flip Env.insert p <$> bindPattern v (t,vs)
    localEnv p' (eval rhs)

  EBind lhs BUnit rhs -> do
    void $ eval lhs
    eval rhs

  Alt _pat body -> do
    eval body

  overGenerative -> error $ show overGenerative

{-
TODO:
Explain terminilogy
-}

-- | How to turn a source defined value to a runtime value
value :: (DC m) => Grin.Value -> Definitional m Value
value = \case
  Grin.VPrim sval -> pure $ Prim $ simpleValue sval
  -- Exercise: Node can refer to names, lookup the names from
  -- the environment and create a runtime Node value from
  -- the value that was defined in the source.
  Grin.VNode vnode -> undefined vnode

-- | Convert a runtime value to an address value
val2addr :: (DC m) => Value -> Definitional m Address
val2addr v = do
  (Prim (SLoc addr)) <- pure v
  pure addr

-- | Convert an address value to a runtime value.
addr2val :: (DC m) => Address -> Definitional m Value
addr2val addr = pure (Prim (SLoc addr))

-- | Convert a heap value, which is a node to a runtime value
heapVal2val :: (DC m) => Node -> Definitional m Value
heapVal2val node = pure (Node node)

-- | Convert a runtime value to a Node value
val2heapVal :: (DC m) => Value -> Definitional m Node
val2heapVal val = do
  (Node node) <- pure val
  pure node

-- | Creates the Unit value, which is only created when the Update operation runs.
unit :: (DC m) => Definitional m Value
unit = pure Unit

-- | Creates a list of Name and runtime value pairs which extends the environment for the
-- right hand side of the bind.
bindPattern :: (DC m) => Value -> (Grin.Tag, [Grin.Name]) -> Definitional m [(Grin.Name, Value)]
bindPattern val tags =
  -- Exercise: The val should be a Node value, if the tag of the node matches, with the given
  -- tag, than the args argument from the Node value must be paired with the names in the
  -- given pattern.
  -- TODO: Add reference to the examples.
  undefined

-- | Return the environment, which associates  names with values
askEnv :: (DC m) => Definitional m (Env Value)
askEnv = asks snd

-- | Sets the environment to the one given, this is for binds, function calls,
-- and alternatives.
localEnv :: (DC m) => Env (Value) -> Definitional m Value -> Definitional m Value
localEnv env = local (set _2 env)

-- | Lookup a function by its name. It should return the Def constructor which contains
-- the parameters and the body of the function.
lookupFun :: (DC m) => Grin.Name -> Definitional m Exp
lookupFun funName = fromJust <$> view (_1 . to functions . at funName)

-- | Checks if the given name refers to an external function.
isExternal :: (DC m) => Grin.Name -> Definitional m Bool
isExternal extName =
  -- Exercise: Use MonadReader to retrieve the Context and lookup the
  -- the extName in the context
  undefined

-- | Run the given external with the parameters
external :: (DC m) => Grin.Name -> [Value] -> Definitional m Value
external =
  -- Exercise: Use the MonadReader to retrieve the Context and lookup
  -- the function and apply the parameters to it
  undefined

funCall
  :: (DC m)
  => (Exp -> Definitional m Value)
  -> Grin.Name -> [Value] -> Definitional m Value
funCall ev funName values =
  -- Exercise:
  -- Lookup the function by the given name
  -- Retrieve its (Def params body)
  -- Create an empty env and bind the function parameters to the given values
  -- Run the eval function on the created new local env and body
  undefined

evalCase
  :: (DC m)
  => (Exp -> Definitional m Value)
  -> Value -> [Alt] -> Definitional m Value
evalCase ev =
  -- Exercise:
  -- Find the first Alt that matches the given value.
  -- If the Alt has a Node pattern, the Val must be a Node
  -- In that case bind the values to the names defined in the Alt pattern
  -- create a new local environment and evaluate the body of the alt in it.
  undefined

-- | Looks up the current store. This is needed for abstractions that
-- are not part of the interpreter.
getStore :: (DC m) => Definitional m (Store Address Node)
getStore = get

-- | Updates the store with the given functions. This is needed for abstractions that
-- are not part of the interpreter.
updateStore :: (DC m) => (Store Address Node -> Store Address Node) -> Definitional m ()
updateStore = modify

-- | Creates a location for a given name. This is particular for the GRIN store structure,
-- where the Store operation must be part of a Bind, thus there will be always a name to
-- bind to, which should hold the address of the created location.
--
-- In this Definitional interpreter for every Store operation that the interpreter evaluates
-- it must creates a new location.
allocStore :: (DC m) => Grin.Name -> Definitional m Value
allocStore _name = do
  addr <- alloc
  pure $ Prim $ SLoc addr

-- | Loads the content from the store addressed by the given value
findStore :: (DC m) => Value -> Definitional m Value
findStore addr = do
  s <- getStore
  a <- undefined addr
  heapVal2val $ Store.lookup a s

-- | Extends the store with the given value.
extStore :: (DC m) => Value -> Value -> Definitional m ()
extStore addr val = do
  a <- undefined addr
  n <- val2heapVal val
  updateStore (Store.insert a n)

-- * Helpers

grinMain :: Program -> Exp
grinMain = \case
  (Program _ defs) -> head $ flip mapMaybe defs $ \case
                        (Def n _ b) -> if n == "main" then Just b else Nothing
                        _           -> Nothing
  _                -> error "grinMain"

programToDefs :: Program -> Map.Map Grin.Name Exp
programToDefs = \case
  (Program _ defs) -> Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty

-- * The externals that the interpreter can understand

knownExternals :: Map.Map Grin.Name ([Value] -> IO Value)
knownExternals = Map.fromList
  [ ("prim_int_eq",  \[Prim (SInt64 a), Prim (SInt64 b)] -> pure $ Prim (SBool (a == b)))
  , ("prim_int_gt",  \[Prim (SInt64 a), Prim (SInt64 b)] -> pure $ Prim (SBool (a > b)))
  , ("prim_int_add", \[Prim (SInt64 a), Prim (SInt64 b)] -> pure $ Prim (SInt64 (a + b)))
  , ("prim_int_sub", \[Prim (SInt64 a), Prim (SInt64 b)] -> pure $ Prim (SInt64 (a - b)))
  , ("prim_int_mul", \[Prim (SInt64 a), Prim (SInt64 b)] -> pure $ Prim (SInt64 (a * b)))
  , ("prim_int_print", \[(Prim (SInt64 a))] -> Unit <$ print a)
  ]
