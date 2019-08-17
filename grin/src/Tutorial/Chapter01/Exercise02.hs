{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
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

{-
Motivation:
To give a language a meaning one could write an interpreter.

Using the interpreter we can define the operational semantics
of a language, showing how a language describe the process of
state changes of an abstract domain.

This approach is called the DEFINITIONAL INTERPRETER.

Our domain consist of
  * an Environment, which associates variables with values, AND
  * a Store (Heap) which represents the memory of a machine. The
    store associates heap locations (Addresses) with values.

Exercise: Read the Grin.Interpreter.Env module
Exercise: Read the Grin.Interpreter.Store module

Note:
In GRIN variables are Static Single Assignment,
which means rebind of a variable should be illegal.

During the interpretation
 * The environment associated variables with values
   of Primitive types (SValue), Node types and Unit which can be created
   of an Update operation or a result of an effectful external operation.
 * The store can only hold Node values, which are structured values.
   (In the GRIN terminology a Node represents a Graph-Node in the memory. Using this
   approach functional language programs can be represented in memory
   as a graph, which needs to be reduced somehow. Thus only Node values
   can be stored on the HEAP)

Exercise: Read the definition of Value, Node and SValue types. These types do represent
values in a running interpreter.
-}

data Value       -- A runtime value can be:
  = Prim SValue  -- A primitive value aks simple value
  | Node Node    -- A node value which represent a node in the graph
  | Unit         -- The UNIT value, which represents no information at all. Like () in Haskell.
  deriving (Eq, Show)

data Node = N { tag :: Grin.Tag, fields :: [SValue] }
  deriving (Eq, Show)

data SValue
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SChar   Char
  | SLoc    Address
  deriving (Eq, Ord, Show)

{-
During the run of the interpeter it needs a context to operate with.
This context gives the ability to the interpreter to call (via the SApp)
the external functions, which mainly represent system/OS functions (externalCall)

Also it needs to know all the defined functions of a program to be
able to load the functions (defined by Def) when they are called
via the SApp primitive.
-}

data Context = Context
  { externalCall :: Grin.Name -> [Value] -> IO Value
  , functions    :: Map.Map Grin.Name Exp
  }


{-
The structure of the interpreter can be represented as a Monad Transformer, which
operated on a given 'm' Monad, which can run arbitrary IO computations.

The Env represents the frame, which holds values for variables. The MonadReader abstraction
fits well with the frame abstraction.

The Store associates Addresses with Node values, during the program run the content
of an address may change.
 * A new address is allocated using the Store operation, which creates a new store
   location saves the value of which was given to the Store operation via a variable
   which value must be looked up from the Env.
   The store operation returns the newly created address.
 * The content of a given address can be retrieved using the Fetch operation. The
   parameter of the operation is a variable which holds an address value.
 * The content of an address can be overwritten using the Update operation.
-}

type Address = Int

newtype Interpreter m a = Interpreter (RWST (Env Value) () (Store Address Node) m a)
  deriving  ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadReader (Env Value)
            , MonadState (Store Address Node)
            , MonadFail
            )

type InterpretExternal = Grin.Name -> [Value] -> IO Value

-- The interpreter function gets how to interpret the external functions,
-- a program to interpret and returns a computed value.
--
-- It collects the function definitions from the program
-- Loads the body of the main function and starts to evaluate that expression.
interpreter :: InterpretExternal -> Program -> IO Value
interpreter ietx prog =
    fst <$> runInterpreter (eval (Context ietx (programToDefs prog)) (grinMain prog))
  where
    runInterpreter
      :: (Monad m, MonadIO m)
      => Interpreter m a -> m (a, Store Address Node)
    runInterpreter (Interpreter r) = do
      (a,store,()) <- runRWST r Env.empty Store.empty
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
valueOf :: Grin.Name -> Interpreter IO Value
valueOf name = asks (`Env.lookup` name)

-- | Looks up a name from the active frame/environment and returns its value, expecting a simple value.
svalueOf :: Grin.Name -> Interpreter IO SValue
svalueOf name = do
  (Prim sv) <- valueOf name
  pure sv

-- | Creates a new location that can be used in the Store operation.
alloc :: Interpreter IO Address
alloc = gets Store.size

-- Exercise:
-- Read and understand the defined parts, fill out the missing definitions.
eval :: Context -> Exp -> Interpreter IO Value
eval ctx = \case

  -- Convert a node literal value to the grin interpreter value
  SPure (Grin.Lit l) -> case l of
    Grin.LVal  sv               -> pure $ Prim $ simpleValue sv
    Grin.LNode (Grin.Node t ps) -> Node . N t <$> mapM svalueOf ps

  -- Lookup a variable in the environment and return its value
  SPure (Grin.Var n) -> valueOf n

  -- Create a memory location on the heap and store the value which the variable v has.
  SStore n -> do
    (Node node) <- valueOf n
    addr <- alloc
    modify (Store.insert addr node)
    pure $ Prim $ SLoc addr

  -- Exercise:
  -- Fetch a value from the heap, addressed by
  -- the memory location stored in the variable h.
  SFetch h -> error "TODO"

  -- Exercise:
  -- Update the value of the memory location h
  -- with the stored value n.
  SUpdate h n -> error "TODO"

  -- Evaluate the left hand side, ignore the value,
  -- and evaluate the right hand side.
  EBind lhs BUnit rhs -> do
    void $ eval ctx lhs
    eval ctx rhs

  -- Exercise:
  -- Evaluate the left hand side, bind its value to the variable x
  -- extending the environment, then evaluate the right hand side
  -- Corresponds to the bind: lhs >>= \pattern -> rhs
  EBind lhs (BVar x) rhs -> error "TODO"

  -- Exercise:
  -- Evaluate the left hand side, bind its value to the pattern
  -- if the node value match the given pattern, otherwise the behaviour
  -- is undefined.
  EBind lhs (BNodePat t xs) rhs -> error "TODO"

  -- Exercise:
  -- Evaluate the variable x and select the the matching alternatives to the
  -- to the value, similar how the pattern matching happened in the EBind,
  -- use your intuition.
  ECase x alts -> do
    v <- valueOf x
    let selectedAlt = (error "TODO") v alts
    eval ctx selectedAlt

  -- Ignore the pattern in the Alt and evaluate the body
  Alt _apat body -> eval ctx body

  -- Call a function. Lookup the function which can be external or internal
  -- bind the values to the function parameters, or pass them to the
  -- interpreter of the externals
  SApp fn callParams -> do
    callValues <- mapM valueOf callParams
    case Map.lookup fn (functions ctx) of
      Nothing -> liftIO $ externalCalls fn callValues
      Just (Def _ funParamNames body) -> do
        -- Creates a new local environment which contains only the function parameters
        local (const $ Env.insert (funParamNames `zip` callValues) Env.empty)
              (eval ctx body)
      overGenerative -> error $ show overGenerative

  overGenerative -> error $ show overGenerative



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

externalCalls :: Grin.Name -> [Value] -> IO Value
externalCalls ext args = case ( ext, args) of
  ("prim_int_eq",  [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SBool (a == b))
  ("prim_int_gt",  [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SBool (a > b))
  ("prim_int_add", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a + b))
  ("prim_int_sub", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a - b))
  ("prim_int_mul", [Prim (SInt64 a), Prim (SInt64 b)]) -> pure $ Prim (SInt64 (a * b))
  ("prim_int_print", [val@(Prim (SInt64 a))]) -> Unit <$ print a
  other -> error ("non-existing external, or bad args " <> show other)
