{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Base
  ( module Grin.Interpreter.Env
  , module Grin.Interpreter.Store
  , module Grin.Interpreter.Base
  ) where

import Control.Monad (void)
import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Grin.Exp
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Grin.Value hiding (Val)

import qualified Data.Map.Strict as Map
import qualified Grin.Value as Grin


-- * Interpreter

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => Exp -> m v
eval = fix ev

-- Open recursion and monadic interpreter.
ev  :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
    => (Exp -> m (Val m)) -> Exp -> m (Val m)
ev ev0 = \case
  SPure n@(CNode{}) -> value n
  SPure l@(Lit{})   -> value l
  SPure u@Unit      -> value u
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

-- Type class

class (Monad m, MonadFail m) => Interpreter m where
  type Val          m :: * -- ^ Values that can be placed in registers/variables
  type HeapVal      m :: * -- ^ Values that can be send to
  type StoreVal     m :: * -- ^ Content of an actual location
  type Addr         m :: * -- ^ A type to represent Addresses
  type NewStoreInfo m :: * -- ^ When creating a new store location this information helps
                           --   to distinguis between different path of the interpretation.

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Val   -> m (Val m)  -- Value of the given literal
  val2addr    :: Val m      -> m (Addr m) --
  addr2val    :: Addr m     -> m (Val m)
  heapVal2val :: HeapVal m  -> m (Val m)
  val2heapVal :: Val m      -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv        :: m (Env (Val m))
  -- | Set the local environment
  localEnv      :: Env (Val m) -> m (Val m) -> m (Val m)
  lookupFun     :: Name -> m Exp
  isOperation   :: Name -> m Bool
  operation     :: Name -> [Val m] -> m (Val m)
  name2NewStoreInfo :: Name -> m (NewStoreInfo m)

  -- Control-flow
  evalCase      :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall       :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  getStore      :: m (Store (Addr m) (StoreVal m))
  putStore      :: (Store (Addr m) (StoreVal m)) -> m ()
  updateStore   :: (Store (Addr m) (StoreVal m) -> Store (Addr m) (StoreVal m)) -> m ()
  nextLocStore  :: NewStoreInfo m -> Store (Addr m) (StoreVal m) -> m (Addr m)
  allocStore    :: NewStoreInfo m -> m (Val m)
  findStore     :: Val m -> m (Val m)
  extStore      :: Val m -> Val m -> m ()

-- * Helper

grinMain :: Program -> Exp
grinMain = \case
  (Program _ defs) -> head $ flip mapMaybe defs $ \case
                        (Def n _ b) -> if n == "main" then Just b else Nothing
                        _           -> Nothing
  _                -> error "grinMain"

programToDefs :: Program -> Map.Map Name Exp
programToDefs = \case
  (Program _ defs) -> Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty
