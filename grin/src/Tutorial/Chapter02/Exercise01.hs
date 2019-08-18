{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Tutorial.Chapter02.Exercise01 where

import Control.Monad (void)
import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Grin.Exp
import Grin.Interpreter.Store
import Grin.Value hiding (Val)

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import qualified Grin.Value as Grin

{-
Motivation:
The talk is based on the Abstracting Definitional Interpreters [1].

In this paper they claim that the same structure for the definitional
interpreter can be reused to create abstract interpreters which
are some form of inference. We will see that type inference
can be encoded using abstract interpretation.

The main motivation here is to understand how the abstraction of
the definitional interpreter can be achieved.

The paper uses open recursion technique to make possible to inject
different aspects of the recursive calls.

This approach is also used in the `semantic` framework by github [2].

[1] https://plum-umd.github.io/abstracting-definitional-interpreters/
[2] https://github.com/github/semantic


Exercise:
Find the difference between the interpreter from the previous exercise.
Change the types in the Interpreter to make compile eval interpreter.
-}

eval  :: (MonadIO m, Interpreter m)
    => (Exp -> m (Val m)) -> Exp -> m (Val m)
eval ev0 = \case
  SPure (Lit l) -> literal l
  SPure (Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    op <- isOperation fn
    (if op then operation else funCall ev0) fn vs

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
    evalCase ev0 v alts

  EBind (SStore n) (BVar l) rhs -> do
    p <- askEnv
    let v = Env.lookup p n
    ac <- name2NewStoreInfo l
    a  <- allocStore ac
    extStore a v
    let p' = Env.insert [(l, a)] p
    localEnv p' (ev0 rhs)

  EBind lhs (BVar n) rhs -> do
    v <- ev0 lhs
    p <- askEnv
    let p' = Env.insert [(n, v)] p
    localEnv p' (ev0 rhs)

  EBind lhs (BNodePat t@(Tag{}) vs) rhs -> do
    v   <- ev0 lhs
    p   <- askEnv
    p'  <- flip Env.insert p <$> bindPattern v (t,vs)
    localEnv p' (ev0 rhs)

  EBind lhs BUnit rhs -> do
    void $ ev0 lhs
    ev0 rhs

  Alt _pat body -> do
    ev0 body

  overGenerative -> error $ show overGenerative


-- * Type class

type Todo m = m ()

-- Change the types in this typeclass to make compile the ev interpreter above
class (Monad m, MonadFail m) => Interpreter m where
  type Val          m :: * -- ^ Values that can be placed in variables
  -- NOTE: HeapVal is the type of values that can be passed to store, fetch, update
  type HeapVal      m :: * -- ^ Values that can be stored on the heap
  -- NOTE: StoreVal is the abstract value that can be stored on the heap (eg.: set of possible types)
  type StoreVal     m :: * -- ^ A collection of values that can come from the HeapVal type
  type Addr         m :: * -- ^ A type to represent Addresses
  -- NOTE: sure
  type NewStoreInfo m :: * -- ^ When creating a new store location this information helps
                           --   to distinguis between different stores.

  -- Exercise: Try to figure out the types of the by yourself based on the interpreter
  -- defined in the previous exercise and 'eval' function in this module.,
  -- using the following method:
  --
  -- Replace the type of a function with `Todo m` and follow the type error.
  -- The more you can figure out in the given time the better.
  --
  -- The solutions are at the end of this module.

  -- Conversions, but m type is needed for type inference
  literal     :: Literal_ m     -- ^ Convert a literal value to an value of the interpretation
  val2addr    :: Val2Addr m     -- ^ Extract the location information from a value, hint :: Val -> Addr
  addr2val    :: Addr2Val m     -- ^ Wrap a location information inside a value
  heapVal2val :: HeapVal2Val m  -- ^ Convert a value that is stored in the heap to a value that
                                --   is stored in registers
  val2heapVal :: Val2HeapVal m  -- ^ Convert a value to the value that can be stored on the heap.
  unit        :: Unit m         -- ^ Some operations require unit values, this should provide it.
  bindPattern :: BindPattern m  -- ^ Bind the a value to pattern, creating a list of pairs (Name, Val)

  -- Externals, functions
  lookupFun         :: LookupFun m          -- ^ Looks up a function by its name and returns its Exp
  isOperation       :: IsOperation m        -- ^ Check if the given name is an external operation
  operation         :: Operation m          -- ^ Call the external operation
  name2NewStoreInfo :: Name2NewStoreInfo m  -- ^ Convert a given name for the information that is needed
                                            --   by the store allocation

  -- Control-flow
  evalCase  :: EvalCase m -- ^ Gets an evaluator, a variable and a set of alternative cases, selecting the
                          --   relevant ones and execute them.
  funCall   :: FunCall m  -- ^ Gets an evaluated, a name of a function with a list of values as values
                          --   for the parameters and evaluates the body of the function

  -- Env
  askEnv        :: AskEnv m   -- ^ Returns the active environment
  localEnv      :: LocalEnv m -- ^ Sets the given environment as a local one

  -- Store
  getStore      :: GetStore m     -- ^ Returns the Store (Heap)
  putStore      :: PutStore m     -- ^ Sets the Store (Heap)
  updateStore   :: UpdateStore m  -- ^ Update the Store using the the given function
  nextLocStore  :: NextLocStore m -- ^ Combines the new store info the given store, and return a possible new address
  allocStore    :: AllocStore m   -- ^ Creates a new location using the New
  findStore     :: FindStore m    -- ^ Retrieve a value from the Store addressed by the parameter
  extStore      :: ExtStore m     -- ^ Change the value of the given location























































































































type Literal_ m  = Grin.Literal -> m (Val m)
type Val2Addr m = Val m -> m (Addr m)
type Addr2Val m = Addr m -> m (Val m)
type HeapVal2Val m = HeapVal m -> m (Val m)
type Val2HeapVal m = Val m -> m (HeapVal m)
type Unit m = m (Val m)
type BindPattern m = Val m -> (Tag, [Name]) -> m [(Name, Val m)]
type AskEnv m = m (Env (Val m))
type LocalEnv m = Env (Val m) -> m (Val m) -> m (Val m)
type LookupFun m = Name -> m Exp
type IsOperation m = Name -> m Bool
type Operation m = Name -> [Val m] -> m (Val m)
type Name2NewStoreInfo m = Name -> m (NewStoreInfo m)
type EvalCase m = (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
type FunCall m = (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)
type GetStore m = m (Store (Addr m) (StoreVal m))
type PutStore m = (Store (Addr m) (StoreVal m)) -> m ()
type UpdateStore m = (Store (Addr m) (StoreVal m) -> Store (Addr m) (StoreVal m)) -> m ()
type NextLocStore m = NewStoreInfo m -> Store (Addr m) (StoreVal m) -> m (Addr m)
type AllocStore m = NewStoreInfo m -> m (Val m)
type FindStore m = Val m -> m (Val m)
type ExtStore m = Val m -> Val m -> m ()
