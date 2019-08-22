{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Grin.Interpreter.Base
  ( module Grin.Interpreter.Base
  ) where

import Control.Monad (void)
import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Data.Function (fix)
import Grin.Exp
import Grin.Value hiding (Val)

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import qualified Grin.Value as Grin


-- * Interpreter

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => Exp -> m v
eval = fix baseEval

-- Open recursion and monadic interpreter.
baseEval :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
         => (Exp -> m (Val m)) -> Exp -> m (Val m)
baseEval ev0 = \case
  SPure (Grin.Val v) -> value v
  SPure (Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  SApp fn ps -> do
    p  <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    ex <- isExternal fn
    (if ex then external else funCall ev0) fn vs

  SFetch n -> do
    p <- askEnv
    let v = Env.lookup p n
    fetchStore v

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
    a  <- allocStore l
    extStore a v
    let p' = Env.insert l a p
    localEnv p' (ev0 rhs)

  EBind lhs (BVar n) rhs -> do
    v <- ev0 lhs
    p <- askEnv
    let p' = Env.insert n v p
    localEnv p' (ev0 rhs)

  EBind lhs (BNodePat t@(Tag{}) vs) rhs -> do
    v   <- ev0 lhs
    p   <- askEnv
    p'  <- flip Env.inserts p <$> bindPattern v (t,vs)
    localEnv p' (ev0 rhs)

  EBind lhs BUnit rhs -> do
    void $ ev0 lhs
    ev0 rhs

  Alt _pat body -> do
    ev0 body

  overGenerative -> error $ show overGenerative

-- Type class

class (Monad m, MonadFail m) => Interpreter m where
  type Val     m :: * -- Values that can be placed in registers/variables
  type HeapVal m :: * -- Values for the Store, Fetch, Update parameters
  type Addr    m :: * -- A type to represent an Address

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Value   -> m (Val m)  -- Value of the given literal
  val2addr    :: Val m        -> m (Addr m) --
  addr2val    :: Addr m       -> m (Val m)
  heapVal2val :: HeapVal m    -> m (Val m)
  val2heapVal :: Val m        -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv        :: m (Env (Val m))
  -- | Set the local environment
  localEnv      :: Env (Val m) -> m (Val m) -> m (Val m)
  lookupFun     :: Name -> m Exp
  isExternal    :: Name -> m Bool
  external      :: Name -> [Val m] -> m (Val m)

  -- Control-flow
  evalCase      :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall       :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  allocStore    :: Name -> m (Val m)
  fetchStore    :: Val m -> m (Val m)      -- TODO: Change this to Addr m??
  extStore      :: Val m -> Val m -> m ()  --
