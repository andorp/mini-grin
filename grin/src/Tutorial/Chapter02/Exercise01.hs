{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Tutorial.Chapter02.Exercise01 where

import Control.Monad (void)
import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Grin.Exp
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Grin.Value hiding (Val)

import qualified Grin.Value as Grin

{-
Exercise:
Find the difference between the interpreter from the previous exercise.
Change the types in the Interpreter to make compile eval interpreter.
-}





eval  :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
    => (Exp -> m (Val m)) -> Exp -> m (Val m)
eval ev0 = \case
  SPure (Lit l) -> literal l
  SPure (Var n) -> do
    p <- askEnv
    pure $ lookupEnv p n

  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (lookupEnv p) ps
    op <- isOperation fn
    (if op then operation else funCall ev0) fn vs

  SFetch n -> do
    p <- askEnv
    let v = lookupEnv p n
    findStore v

  SUpdate nl nn -> do
    p <- askEnv
    let vl = lookupEnv p nl
    let vn = lookupEnv p nn
    extStore vl vn
    unit

  ECase n alts -> do
    p <- askEnv
    v <- pure $ lookupEnv p n
    -- Select the alternative and continue the evaluation
    evalCase ev0 v alts

  EBind (SStore n) (BVar l) rhs -> do
    p <- askEnv
    let v = lookupEnv p n
    ac <- name2NewStoreInfo l
    a  <- allocStore ac
    extStore a v
    let p' = extendEnv p [(l, a)]
    localEnv p' (ev0 rhs)

  EBind lhs (BVar n) rhs -> do
    v <- ev0 lhs
    p <- askEnv
    let p' = extendEnv p [(n, v)]
    localEnv p' (ev0 rhs)

  EBind lhs (BNodePat t@(Tag{}) vs) rhs -> do
    v   <- ev0 lhs
    p   <- askEnv
    p'  <- extendEnv p <$> bindPattern v (t,vs)
    localEnv p' (ev0 rhs)

  EBind lhs BUnit rhs -> do
    void $ ev0 lhs
    ev0 rhs

  Alt _pat body -> do
    ev0 body

  overGenerative -> error $ show overGenerative














-- * Type class

-- Change the types in this typeclass to make compile the ev interpreter above
class (Monad m, MonadFail m) => Interpreter m where
  type Val          m :: * -- ^ Values that can be placed in registers/variables
  type HeapVal      m :: * -- ^ Values that can be stored on the heap
  type StoreVal     m :: * -- ^ Heap where heap values can be stored (like malloc)
  type Addr         m :: * -- ^ A type to represent Addresses
  type NewStoreInfo m :: * -- ^ When creating a new store location this information helps
                           --   to distinguis between different stores.

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
