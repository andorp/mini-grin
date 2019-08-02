{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract where

import Data.Function (fix)
import Prelude hiding (fail)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), forM_, msum, filterM)
import Data.Maybe (isNothing)
import Grin.Exp hiding (Val)
import Grin.Pretty
import Grin.Examples
import qualified Grin.Exp as Grin (Val)
import Grin.Interpreter.Base
import Data.Maybe (fromMaybe, fromJust)
import Lens.Micro.Platform
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.State hiding (MonadState(..), get)
import Control.Monad.Trans.Reader hiding (MonadReader(..), local, ask)
import Control.Monad.Trans.RWS hiding (ask, local, get)
-- import Control.Monad.Trans.List
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Infix ((<$$>))
import Control.Monad.Logic hiding (fail)
import Text.PrettyPrint.ANSI.Leijen hiding (SChar, (<$>), (<$$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Data.List as List ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set; import Data.Set (Set)


data ST
  = SInt64
  | SWord64
  | SFloat
  | SBool
  | SString
  | SChar
  | SLoc
  deriving (Eq, Ord, Show)

instance Pretty ST where
  pretty = \case
    SLoc -> {-encloseSep lbrace rbrace comma $ map (cyan . int)-} cyan $ text $ show SLoc
    ty -> red $ text $ show ty

data Node = Node Tag [ST]
  deriving (Eq, Ord, Show)

data T
  = ST ST
  | NT Node
  | UT
  deriving (Eq, Ord, Show)

instance Pretty Node where
  pretty (Node tag args) = pretty tag <> list (map pretty args)

instance Pretty T where
  pretty = \case
    ST ty -> pretty ty
    NT n  -> pretty n

forMonadPlus :: (MonadPlus m) => [a] -> (a -> m b) -> m b
forMonadPlus xs k = msum (map k xs)

data H = H -- Heap
  deriving (Eq, Ord, Show)

instance Pretty H where
  pretty = text . show

data AbsEnv
  = AbsEnv
  { _absOps :: Map.Map Name (T, [T])
  , _absEnv :: Env T            -- Names in scope
  , _absFun :: Map.Map Name Exp -- How to record the type of function parameters? As now the interpeter calls the
                                -- function during the evaluation, and the current parameters are recorded in the
                                -- environment? Maybe a state will be needed instead of the Reader monad?
                                -- Or the lookupFun and the SApp needs a different implementation? OR the SApp
                                -- should be implemented as the ecase?
  }

makeLenses ''AbsEnv

data TypeEnv = TypeEnv
  { _location :: AbsStore
  }

instance Pretty TypeEnv where
  pretty TypeEnv{..} = vsep
    [ yellow (text "Location") PP.<$$> indent 4 (pretty _location)
    ]

type AbsStore = Store H (Set Node)

data AbsState
  = AbsState
  { _absStr :: AbsStore
  } deriving (Show)

makeLenses ''AbsState

newtype AbstractT m a = AbstractT
  { abstractT :: RWST AbsEnv () AbsState (LogicT (RWST Cache () Cache m)) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState AbsState
    , MonadReader AbsEnv
    , Alternative
    , MonadPlus
    )

instance MonadFail (AbstractT m) where
  fail _ = mzero

runAbstractT
  :: (Monad m, MonadFail m, MonadIO m)
  => Program -> [(Name, (T, [T]))]
  -> AbstractT m a -> m ((a, AbsState, ()), Cache, ())
runAbstractT prog ops m =
  runRWST
    (observeT
      (runRWST
        (abstractT m)
        (AbsEnv (Map.fromList ops) emptyEnv (programToDefs prog))
        (AbsState emptyStore)))
    mempty
    mempty

getCacheOut :: (Monad m) => AbstractT m Cache
getCacheOut = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  outC <- get
  sk (outC,as,()) fk

putCacheOut :: (Monad m) => Cache -> AbstractT m ()
putCacheOut outC = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  Control.Monad.State.put outC
  sk ((),as,()) fk

updateCacheOut :: (Monad m) => (Cache -> Cache) -> AbstractT m ()
updateCacheOut f = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  Control.Monad.State.state (\s -> ((), f s))
  sk ((),as,()) fk

askCacheIn :: (Monad m) => AbstractT m Cache
askCacheIn = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  inC <- ask
  sk (inC,as,()) fk

localCacheIn :: (Monad m) => Cache -> AbstractT m a -> AbstractT m a
localCacheIn inC m = AbstractT $ RWST $ \ae as -> LogicT $ \sk fk -> do
  local (const inC) $ unLogicT (runRWST (abstractT m) ae as) sk fk

data Cache = Cache (Map.Map Config (Set.Set (T, AbsStore)))
  deriving (Eq, Show)

cache2TypeEnv :: Cache -> TypeEnv
cache2TypeEnv (Cache m) = TypeEnv $ mconcat $ Set.toList $ Set.map snd $ Set.unions $ Map.elems m

data Config = Config
  { cfgEnv    :: Env T
  , cfgStore  :: AbsStore
  , cfgExp    :: CExp
  } deriving (Eq, Show, Ord)

data CExp
  = CApp     Name [Name]
  | CStore   Name -- Variable should hold only nodes
  | CFetch   Name -- Variable should hold only locations
  | CUpdate  Name Name -- The variables in order should hold only location and node
  deriving (Eq, Show, Ord)

exp2CExp :: Exp -> Maybe CExp
exp2CExp = \case
  -- Simple Exp
  SApp    f ps  -> Just $ CApp f ps
  SStore  n     -> Just $ CStore n
  SFetch  l     -> Just $ CFetch l
  SUpdate l v   -> Just $ CUpdate l v
  _             -> Nothing

instance Semigroup Cache where
  (Cache ma) <> (Cache mb) = Cache (Map.unionWith (Set.union) ma mb)

instance Monoid Cache where
  mempty = Cache mempty

cacheSize :: Cache -> [Int]
cacheSize (Cache m) = Map.elems $ fmap Set.size m

inCache :: Config -> Cache -> Bool
inCache c (Cache m) = Map.member c m

getCache :: Config -> Cache -> [(T, AbsStore)]
getCache c (Cache m) = maybe [] Set.toList $ Map.lookup c m

insertCache :: Config -> [(T, AbsStore)] -> Cache -> Cache
insertCache c vos (Cache m) = Cache (Map.unionWith (<>) m (Map.singleton c (Set.fromList vos)))

typeOfLit :: Lit -> T
typeOfLit = \case
  LInt64  _ -> ST SInt64
  LWord64 _ -> ST SWord64
  LFloat  _ -> ST SFloat
  LBool   _ -> ST SBool
  LString _ -> ST SString
  LChar   _ -> ST SChar

instance (Monad m, MonadIO m, MonadFail m) => Interpreter (AbstractT m) where
  type Val     (AbstractT m) = T
  type HeapVal (AbstractT m) = Node
  type StoreVal (AbstractT m) = Set Node
  type Addr    (AbstractT m) = H

  value :: Grin.Val -> AbstractT m T
  value = \case
    (ConstTagNode tag ps) -> do
      p  <- askEnv
      ts <- pure $ map (lookupEnv p) ps
      pure $ NT $ Node tag $ map (\case
        ST t -> t
        other -> error $ show ("value", other) -- TODO: Include type error
        ) ts
    (Lit l) -> pure $ typeOfLit l
    Unit    -> pure $ UT

  val2addr :: T -> AbstractT m H
  val2addr = \case
    ST SLoc -> pure H
    other   -> error $ show ("val2addr", other)

  addr2val :: H -> AbstractT m T
  addr2val _ = pure $ ST SLoc

  val2heapVal :: T -> AbstractT m Node
  val2heapVal = \case
    NT n -> pure n
    other -> error $ show ("val2heapVal", other)

  heapVal2val :: Node -> AbstractT m T
  heapVal2val = pure . NT

  unit :: AbstractT m T
  unit = pure UT

  bindPattern :: T -> (Tag, [Name]) -> AbstractT m [(Name, T)]
  bindPattern t (tag,ps) = case t of
    NT (Node t1 ps1)
      | t1 == tag -> pure (ps `zip` (ST <$> ps1))
      | otherwise -> mzero
    other -> error $ show ("bindPattern", other)

  -- non-pure
  askEnv :: AbstractT m (Env T)
  askEnv = _absEnv <$> ask

  localEnv :: Env T -> AbstractT m T -> AbstractT m T
  localEnv env = local (absEnv .~ env)

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ show ("lookupFun", fn)) . Map.lookup fn . _absFun) <$> ask

  isOperation :: Name -> AbstractT m Bool
  isOperation n = (Map.member n . _absOps) <$> ask

  operation :: Name -> [T] -> AbstractT m T
  operation n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ show ("operation", n, ps, ts)
    pure r

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase ev v alts = do
    selectedAlts <- filterM isMatching alts
    forMonadPlus selectedAlts extendAlt
    where
      isMatching (Alt DefaultPat     _) = pure True
      isMatching (Alt (LitPat l)     _) = (v ==) <$> value (Lit l)
      isMatching (Alt (NodePat t ps) _) = case v of
        NT (Node t0 _) -> pure $ t == t0
        other          -> pure False

      extendAlt (Alt DefaultPat     body) = ev body
      extendAlt (Alt (LitPat _)     body) = ev body
      extendAlt (Alt (NodePat _ ns) body) = case v of
        NT (Node t0 vs) -> do
          p <- askEnv
          localEnv (extendEnv p (ns `zip` (ST <$> vs))) $ ev body
        other -> error $ show ("extendAlt", other)

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = extendEnv emptyEnv (fps `zip` vs)
    localEnv p' (ev body)

  getStore :: AbstractT m AbsStore
  getStore = _absStr <$> Control.Monad.State.get

  putStore :: AbsStore -> AbstractT m ()
  putStore = (absStr .=)

  updateStore :: (AbsStore -> AbsStore) -> AbstractT m ()
  updateStore = (absStr %=)

  nextLocStore :: AbsStore -> AbstractT m H
  nextLocStore _ = pure H

  allocStore :: AbstractT m T
  allocStore = pure $ ST SLoc

  findStore :: T -> AbstractT m T
  findStore v = do
    s <- getStore
    a <- val2addr v
    forMonadPlus (Set.toList $ storeFind s a) heapVal2val

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem x = (fmap (Set.insert n) x) `mplus` (Just (Set.singleton n))
    updateStore (\(Store m) -> Store (Map.alter changeElem a m))

evalCache
  :: (Monad m, MonadFail m, MonadIO m)
  => ((Exp -> AbstractT m T) -> (Exp -> AbstractT m T)) -> (Exp -> AbstractT m T) -> Exp -> AbstractT m T
evalCache ev0 ev e = do
  p   <- askEnv
  o   <- getStore
  case (exp2CExp e) of
    n@Nothing -> do
      ev0 ev e
    Just ce -> do
      let c = Config { cfgEnv = p, cfgStore = o, cfgExp = ce }
      outc <- getCacheOut
      if inCache c outc
        then do
          forMonadPlus (getCache c outc) $ \(v,o) -> do
            putStore o
            pure v
        else do
          inc <- askCacheIn
          let vo0 = if inCache c inc then (getCache c inc) else []
          putCacheOut (insertCache c vo0 outc)
          v <- ev0 ev e
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
fixCache eval e = do
  p <- askEnv
  o <- getStore
  dp <- mlfp $ \cin -> do
          putCacheOut mempty
          putStore o
          localCacheIn cin $ eval e
          r <- getCacheOut
          pure r
  pure ()

evalAbstractOne :: (Monad m, MonadFail m, MonadIO m) => Program -> m Cache
evalAbstractOne prog = do
  let ops = [ ("prim_int_add", prim_int_add)
            , ("prim_int_sub", prim_int_sub)
            , ("prim_int_mul", prim_int_mul)
            , ("prim_int_print", prim_int_print)
            , ("prim_int_eq", prim_int_eq)
            , ("prim_int_gt", prim_int_gt)
            ]
  let opsMap = Map.fromList ops
  forM_ exts $ \ext -> do
    when (isNothing (Map.lookup (eName ext) opsMap)) $
      fail $ "Missing external: " ++ show (eName ext)
  (\(_,c,_) -> c) <$> runAbstractT prog ops (fixCache (fix (evalCache ev)) (grinMain prog))
  where
    exts = externals prog
    prim_int_add    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_sub    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_mul    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_eq     = (ST SBool,  [ST SInt64, ST SInt64])
    prim_int_gt     = (ST SBool,  [ST SInt64, ST SInt64])
    prim_int_print  = (UT, [ST SInt64])


runAdd :: IO ()
runAdd = do
  cache <- evalAbstractOne add
  print $ PP $ cache2TypeEnv cache

runFact :: IO ()
runFact = do
  cache <- evalAbstractOne fact
  print $ PP $ cache2TypeEnv cache

runSum :: IO ()
runSum = do
  cache <- evalAbstractOne sumSimple
  print cache
  print $ PP $ cache2TypeEnv cache

