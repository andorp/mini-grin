{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract.Interpreter where

import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.RWS.Strict hiding (ask, local, get)
import Data.Maybe (fromMaybe, fromJust)
import Grin.Exp (Exp(..), CPat(..), Alt)
import Grin.Value (Name, Tag)
import Grin.Interpreter.Base (Interpreter(..))
import Grin.Pretty hiding (SChar)
import Lens.Micro.Platform
import Prelude hiding (fail)

import Grin.Interpreter.Store (Store(..))
import Grin.Interpreter.Env (Env(..))
import qualified Grin.Interpreter.Store as Store
import qualified Grin.Interpreter.Env as Env
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.Value as Grin

import Grin.Interpreter.Abstract.Base



getCacheOut :: (Monad m) => AbstractT m Cache
getCacheOut = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (_, outC) <- get
  sk (outC,as,()) fk

putCacheOut :: (Monad m) => Cache -> AbstractT m ()
putCacheOut outC1 = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (te, outC0) <- get
  Control.Monad.State.put (te <> cache2TypeEnv outC1, outC0 <> outC1)
  sk ((),as,()) fk

updateCacheOut :: (Monad m) => (Cache -> Cache) -> AbstractT m ()
updateCacheOut f = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  Control.Monad.State.state (\(te,c) -> ((), (te <> cache2TypeEnv (f c), c <> f c)))
  sk ((),as,()) fk

-- | Joins the given Env T to the end result
collectEnv :: (Monad m) => Env T -> AbstractT m ()
collectEnv env = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (te, outC) <- get
  Control.Monad.State.put (te <> mempty { _variable = (Set.singleton <$> env) }, outC)
  sk ((),as,()) fk

-- | Joins the type of the funcion to the end result
collectFunctionType :: (Monad m) => (Name,[T],T) -> AbstractT m ()
collectFunctionType (fn,ps,r) = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (te, outC) <- get
  Control.Monad.State.put
    (te <> mempty
      { _function = Map.singleton fn (FunctionT (Set.singleton r,Set.singleton <$> ps))
      }
    , outC)
  sk ((),as,()) fk

askCacheIn :: (Monad m) => AbstractT m Cache
askCacheIn = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (_, inC) <- ask
  sk (inC,as,()) fk

localCacheIn :: (Monad m) => Cache -> AbstractT m a -> AbstractT m a
localCacheIn inC m = AbstractT $ RWST $ \ae as -> LogicT $ \sk fk -> do
  local (\(te, _) -> (te <> cache2TypeEnv inC, inC)) $ unLogicT (runRWST (abstractT m) ae as) sk fk

cache2TypeEnv :: Cache -> TypeEnv
cache2TypeEnv (Cache m) = mempty { _heap = (mconcat $ Set.toList $ Set.map snd $ Set.unions $ Map.elems m) }

exp2CExp :: Exp -> Maybe CExp
exp2CExp = \case
  -- Simple Exp
  SApp    f ps  -> Just $ CApp f ps
  _             -> Nothing

instance (Monad m, MonadIO m, MonadFail m) => Interpreter (AbstractT m) where
  type Val     (AbstractT m) = T
  type HeapVal (AbstractT m) = Node
  type Addr    (AbstractT m) = Loc

  value :: Grin.Value -> AbstractT m T
  value = \case
    (Grin.VNode (Grin.Node tag ps)) -> do
      p  <- askEnv
      ts <- pure $ map (Env.lookup p) ps
      pure $ NT $ Node tag $ map (\case
        ST t -> t
        other -> error $ unwords ["value", show other] -- TODO: Include type error
        ) ts
    (Grin.VPrim l) -> pure $ typeOfSimpleValue l

  val2addr :: T -> AbstractT m Loc
  val2addr = \case
    ST (ST_Loc l) -> pure l
    other         -> error $ unwords ["val2addr", show other]

  addr2val :: Loc -> AbstractT m T
  addr2val l = pure $ ST $ ST_Loc l

  val2heapVal :: T -> AbstractT m Node
  val2heapVal = \case
    NT n -> pure n
    other -> error $ unwords ["val2heapVal", show other]

  heapVal2val :: Node -> AbstractT m T
  heapVal2val = pure . NT

  unit :: AbstractT m T
  unit = pure UT

  bindPattern :: T -> (Tag, [Name]) -> AbstractT m [(Name, T)]
  bindPattern t (tag,ps) = case t of
    NT (Node t1 ps1)
      | t1 == tag -> pure (ps `zip` (ST <$> ps1))
      | otherwise -> mzero
    other -> error $ unwords ["bindPattern", show other]

  -- non-pure
  askEnv :: AbstractT m (Env T)
  askEnv = _absEnv <$> ask

  localEnv :: Env T -> AbstractT m T -> AbstractT m T
  localEnv env m = do
    collectEnv env
    local (absEnv .~ env) m

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ unwords ["lookupFun", Grin.nameString fn]) . Map.lookup fn . _absFun) <$> ask

  isExternal :: Name -> AbstractT m Bool
  isExternal n = (Map.member n . _absExt) <$> ask

  external :: Name -> [T] -> AbstractT m T
  external n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absExt) <$> ask
    when (ps /= ts) $ error $ unwords ["external", Grin.nameString n, show ps, show ts]
    collectFunctionType (n,ts,r)
    pure r

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase ev0 v alts = do
    selectedAlts <- filterM isMatching alts
    forMonadPlus selectedAlts extendAlt
    where
      isMatching (Alt DefaultPat      _) = pure True
      isMatching (Alt (LitPat l)      _) = (v ==) <$> value (Grin.VPrim l)
      isMatching (Alt (NodePat t _ps) _) = case v of
        NT (Node t0 _) -> pure $ t == t0
        _nonNodeType   -> pure False
      isMatching overGenerative = error $ show overGenerative

      extendAlt alt@(Alt DefaultPat     _body) = ev0 alt
      extendAlt alt@(Alt (LitPat _)     _body) = ev0 alt
      extendAlt alt@(Alt (NodePat _ ns) _body) = case v of
        NT (Node _t0 vs) -> do
          p <- askEnv
          localEnv (Env.inserts (ns `zip` (ST <$> vs)) p) $ ev0 alt
        nonNodeType -> error $ show nonNodeType
      extendAlt overGenerative = error $ show overGenerative

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev0 fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = Env.inserts (fps `zip` vs) Env.empty
    v <- localEnv p' (ev0 body)
    collectFunctionType (fn,vs,v)
    pure v

  allocStore :: Name -> AbstractT m T
  allocStore name = pure $ ST $ ST_Loc $ Loc name

  fetchStore :: T -> AbstractT m T
  fetchStore v = do
    s <- _absStr <$> Control.Monad.State.get
    a <- val2addr v
    forMonadPlus (Set.toList $ Store.lookup a s) heapVal2val

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem Nothing  = Just (Set.singleton n)
        changeElem (Just m) = Just (Set.insert n m)
    AbstractT $ (modify (over absStr (\(Store m) -> Store (Map.alter changeElem a m))))

-- * Fixpoint finding algorithm

putStore :: Store Loc (Set Node) -> AbstractT m ()
putStore o = (absStr .= o)

getStore :: AbstractT m (Store Loc (Set Node))
getStore = _absStr <$> Control.Monad.State.get

evalCache
  :: (Monad m, MonadFail m, MonadIO m)
  => ((Exp -> AbstractT m T) -> (Exp -> AbstractT m T)) -> (Exp -> AbstractT m T) -> Exp -> AbstractT m T
evalCache ev0 ev1 e = do
  case (exp2CExp e) of
    Nothing -> do
      ev0 ev1 e
    Just ce -> do
      s <- getStore
      let c = Config { cfgExp = ce, cfgStore = s }
      outc <- getCacheOut
      if inCache c outc
        then do
          forMonadPlus (getCache c outc) $ \(v,o0) -> do
            putStore o0
            pure v
        else do
          inc <- askCacheIn
          let vo0 = if inCache c inc then (getCache c inc) else []
          putCacheOut (insertCache c vo0 outc)
          v <- ev0 ev1 e
          o' <- getStore
          updateCacheOut (insertCache c [(v,o')])
          pure v


mlfp :: (MonadIO m, Monad m) => (Cache -> AbstractT m Cache) -> AbstractT m Cache
mlfp f = loop mempty where
  loop x = do
    x' <- f x
    if (x' == x)
      then pure x
      else loop x'

fixCache :: (MonadFail m, MonadIO m) => (t -> AbstractT m a) -> t -> AbstractT m ()
fixCache eval0 e = do
  o <- getStore
  void $ mlfp $ \cin -> do
    putCacheOut mempty
    putStore o
    void $ localCacheIn cin $ eval0 e
    getCacheOut
