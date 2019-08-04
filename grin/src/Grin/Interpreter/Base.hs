{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Base where

import Data.Function (fix)
import qualified Data.Map.Strict as Map
import Control.Monad.Fail
import Grin.Exp hiding (Val)
import Grin.Pretty
import qualified Grin.Exp as Grin
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Data.List (foldl')
import Debug.Trace (traceShowId)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


-- * Env

newtype Env v = Env (Map.Map Name v)
  deriving (Eq, Show, Ord, Functor)

emptyEnv :: Env v
emptyEnv = Env mempty

lookupEnv :: (Env v) -> Name -> v
lookupEnv (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

extendEnv :: Env v -> [(Name, v)] -> Env v
extendEnv (Env m) vs = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

instance (Semigroup v) => Semigroup (Env v) where
  Env m1 <> Env m2 = Env (Map.unionWith (<>) m1 m2)

instance (Semigroup v) => Monoid (Env v) where
  mempty = Env mempty

instance (Pretty v) => Pretty (Env v) where
  pretty (Env m) = prettyKeyValue (Map.toList m)

-- * Store

newtype Store a v = Store (Map.Map a v)
  deriving (Eq, Ord, Show)

emptyStore :: (Ord a) => Store a v
emptyStore = Store mempty

storeFind :: (Ord a) => Store a v -> a -> v
storeFind (Store m) a = fromMaybe (error "Store; missing") $ Map.lookup a m

storeExt :: (Ord a) => a -> v -> Store a v -> Store a v
storeExt a v (Store m) = Store (Map.insert a v m)

instance (Ord a, Semigroup v) => Semigroup (Store a v) where
  (Store ma) <> (Store mb) = Store (Map.unionWith (<>) ma mb)

instance (Ord a, Monoid v) => Monoid (Store a v) where
  mempty = Store mempty

instance (Pretty a, Pretty v) => Pretty (Store a v) where
  pretty (Store m) = prettyKeyValue (Map.toList m)

-- * Interpreter

class (Monad m, MonadFail m) => Interpreter m where
  type Val      m :: *
  type HeapVal  m :: *
  type StoreVal m :: *
  type Addr     m :: *
  type StoreCtx m :: *

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Val -> m (Val m)
  val2addr    :: Val m -> m (Addr m)
  addr2val    :: Addr m -> m (Val m)
  heapVal2val :: HeapVal m -> m (Val m)
  val2heapVal :: Val m -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv      :: m (Env (Val m))
  -- | Set the local environment
  localEnv    :: Env (Val m) -> m (Val m) -> m (Val m)
  lookupFun   :: Name -> m Exp
  isOperation :: Name -> m Bool
  operation   :: Name -> [Val m] -> m (Val m)
  name2AllocCtx :: Name -> m (StoreCtx m)

  -- Control-flow
  evalCase    :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall     :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  getStore     :: m (Store (Addr m) (StoreVal m))
  putStore     :: (Store (Addr m) (StoreVal m)) -> m ()
  updateStore  :: (Store (Addr m) (StoreVal m) -> Store (Addr m) (StoreVal m)) -> m ()
  nextLocStore :: StoreCtx m -> Store (Addr m) (StoreVal m) -> m (Addr m)
  allocStore   :: StoreCtx m -> m (Val m)
  findStore    :: Val m -> m (Val m)
  extStore     :: Val m -> Val m -> m ()

grinMain :: Program -> Exp
grinMain (Program _ defs) = gmain
  where
    gmain = head $ mapMaybe (\(Def n _ b) -> if n == "main" then Just b else Nothing) defs

debug :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => (Exp -> m (Val m)) -> Exp -> m (Val m)
debug ev e = do
  p <- askEnv
  liftIO $ case e of
    SApp fn ps -> print (fn, ps)
    _          -> pure ()
  ev e

-- Open recursion and monadic interpreter.
ev  :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
    => (Exp -> m (Val m)) -> Exp -> m (Val m)
ev ev = \case
  SPure n@(ConstTagNode{})  -> value n
  SPure l@(Lit{})           -> value l
  SPure v@(Var n)           -> do
    p <- askEnv
    pure $ lookupEnv p n

  SApp fn ps -> do
    p <- askEnv
    vs <- pure $ map (lookupEnv p) ps
    op <- isOperation fn
    (if op then operation else funCall ev) fn vs

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
    evalCase ev v alts

  EBind (SStore n) (Var l) rhs -> do
    p <- askEnv
    let v = lookupEnv p n
    ac <- name2AllocCtx l
    a  <- allocStore ac
    extStore a v
    let p' = extendEnv p [(l, a)]
    localEnv p' (ev rhs)

  EBind lhs (Var n) rhs -> do
    v <- ev lhs
    p <- askEnv
    let p' = extendEnv p [(n, v)]
    localEnv p' (ev rhs)

  EBind lhs c@(ConstTagNode t@(Tag{}) vs) rhs -> do
    v   <- ev lhs
    p   <- askEnv
    p'  <- extendEnv p <$> bindPattern v (t,vs)
    localEnv p' (ev rhs)

  EBind lhs Unit rhs -> do
    _ <- ev lhs
    ev rhs

  Alt p body -> do
    ev body

  other -> error $ show ("ev", other)

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => Exp -> m v
eval e = fix (debug . ev) e

programToDefs :: Program -> Map.Map Name Exp
programToDefs (Program _ defs) = Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
