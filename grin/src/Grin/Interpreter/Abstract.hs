{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract where

import Data.Function (fix)
import Prelude hiding (fail)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), forM_, msum, filterM)
import Data.Maybe (isNothing)
import Grin.Exp hiding (Val, Loc)
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
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Infix ((<$$>))
import Control.Monad.Logic hiding (fail)
import Text.PrettyPrint.ANSI.Leijen hiding (SChar, (<$>), (<$$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.PrettyPrint.ANSI.Leijen.Internal as PP

import qualified Data.List as List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)


data ST
  = SInt64
  | SWord64
  | SFloat
  | SBool
  | SString
  | SChar
  | SLoc Loc
  deriving (Eq, Ord, Show)

instance Pretty ST where
  pretty = \case
    l@(SLoc h) -> {-encloseSep lbrace rbrace comma $ map (cyan . int)-} cyan $ pretty h
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
    UT    -> text "Unit"

forMonadPlus :: (MonadPlus m) => [a] -> (a -> m b) -> m b
forMonadPlus xs k = msum (map k xs)

data Loc = Loc Name -- Heap
  deriving (Eq, Ord, Show)

instance Pretty Loc where
  pretty (Loc l) = text "L" <> pretty l

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

newtype FunctionT = FunctionT (Set T, [Set T])

instance Semigroup FunctionT where
  (FunctionT (r1,ps1)) <> (FunctionT (r2,ps2)) = FunctionT ((r1 <> r2),(zipWith (<>) ps1 ps2)) -- TODO: Check

instance Monoid FunctionT where
  mempty = FunctionT mempty

prettyFunctionT :: (Name, FunctionT) -> Doc
prettyFunctionT (name, (FunctionT (ret, args))) = pretty name <> align (encloseSep (text " :: ") PP.empty (text " -> ") (map pretty $ args ++ [ret]))

data TypeEnv = TypeEnv
  { _location :: AbsStore
  , _variable :: Env (Set T)
  , _function :: Map.Map Name FunctionT
  }

instance Semigroup TypeEnv where
  TypeEnv l1 v1 f1 <> TypeEnv l2 v2 f2 = TypeEnv (l1 <> l2) (v1 <> v2) (Map.unionWith (<>) f1 f2)

instance Monoid TypeEnv where
  mempty = TypeEnv mempty mempty mempty

instance Pretty TypeEnv where
  pretty TypeEnv{..} = vsep
    [ yellow (text "Location") PP.<$$> indent 4 (pretty _location)
    , yellow (text "Variable") PP.<$$> indent 4 (pretty _variable)
    , yellow (text "Function") PP.<$$> indent 4 (vsep $ map prettyFunctionT $ Map.toList _function)
    ]

type AbsStore = Store Loc (Set Node)

data AbsState
  = AbsState
  { _absStr :: AbsStore
  } deriving (Show, Eq)

makeLenses ''AbsState

newtype AbstractT m a = AbstractT
  { abstractT :: RWST AbsEnv () AbsState (LogicT (RWST (TypeEnv,Cache) () (TypeEnv, Cache) m)) a
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
  -> AbstractT m a -> m ([(a, AbsState, ())], (TypeEnv,Cache), ())
runAbstractT prog ops m =
  runRWST
    (observeAllT
      (runRWST
        (abstractT m)
        (AbsEnv (Map.fromList ops) emptyEnv (programToDefs prog))
        (AbsState emptyStore)))
    mempty
    mempty

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

appendEnvOut :: (Monad m) => Env T -> AbstractT m ()
appendEnvOut env = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
  (te, outC) <- get
  Control.Monad.State.put (te <> mempty { _variable = (Set.singleton <$> env) }, outC)
  sk ((),as,()) fk

appendFunOut :: (Monad m) => (Name,[T],T) -> AbstractT m ()
appendFunOut (fn,ps,r) = AbstractT $ RWST $ \_ae as -> LogicT $ \sk fk -> do
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

data Cache = Cache (Map.Map Config (Set.Set (T, AbsStore)))
  deriving (Eq, Show)

cache2TypeEnv :: Cache -> TypeEnv
cache2TypeEnv (Cache m) = mempty { _location = (mconcat $ Set.toList $ Set.map snd $ Set.unions $ Map.elems m) }

data Config = Config
  { cfgEnv    :: Env T
  , cfgStore  :: AbsStore
  , cfgExp    :: CExp
  } deriving (Eq, Show, Ord)

data CExp
  = CApp     Name [Name]
  deriving (Eq, Show, Ord)

exp2CExp :: Exp -> Maybe CExp
exp2CExp = \case
  -- Simple Exp
  SApp    f ps  -> Just $ CApp f ps
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
  type Val      (AbstractT m) = T
  type HeapVal  (AbstractT m) = Node
  type StoreVal (AbstractT m) = Set Node
  type Addr     (AbstractT m) = Loc
  type StoreCtx (AbstractT m) = Name

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

  val2addr :: T -> AbstractT m Loc
  val2addr = \case
    ST (SLoc l) -> pure l
    other       -> error $ show ("val2addr", other)

  addr2val :: Loc -> AbstractT m T
  addr2val l = pure $ ST $ SLoc l

  val2heapVal :: T -> AbstractT m Node
  val2heapVal = \case
    NT n -> pure n
    other -> error $ show ("val2heapVal", other)

  heapVal2val :: Node -> AbstractT m T
  heapVal2val = pure . NT

  name2AllocCtx :: Name -> AbstractT m Name
  name2AllocCtx = pure

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
  localEnv env m = do
    appendEnvOut env
    local (absEnv .~ env) m

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ show ("lookupFun", fn)) . Map.lookup fn . _absFun) <$> ask

  isOperation :: Name -> AbstractT m Bool
  isOperation n = (Map.member n . _absOps) <$> ask

  operation :: Name -> [T] -> AbstractT m T
  operation n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ show ("operation", n, ps, ts)
    appendFunOut (n,ts,r)
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

      extendAlt alt@(Alt DefaultPat     body) = ev alt
      extendAlt alt@(Alt (LitPat _)     body) = ev alt
      extendAlt alt@(Alt (NodePat _ ns) body) = case v of
        NT (Node t0 vs) -> do
          p <- askEnv
          localEnv (extendEnv p (ns `zip` (ST <$> vs))) $ ev alt
        other -> error $ show ("extendAlt", other)

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = extendEnv emptyEnv (fps `zip` vs)
    v <- localEnv p' (ev body)
    appendFunOut (fn,vs,v)
    pure v

  getStore :: AbstractT m AbsStore
  getStore = _absStr <$> Control.Monad.State.get

  putStore :: AbsStore -> AbstractT m ()
  putStore = (absStr .=)

  updateStore :: (AbsStore -> AbsStore) -> AbstractT m ()
  updateStore = (absStr %=)

  nextLocStore :: Name -> AbsStore -> AbstractT m Loc
  nextLocStore n _ = pure $ Loc n

  allocStore :: Name -> AbstractT m T
  allocStore ctx = do
    s <- getStore
    l <- nextLocStore ctx s
    pure $ ST $ SLoc l

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

evalAbstractOne :: (Monad m, MonadFail m, MonadIO m) => Program -> m (TypeEnv, Cache)
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
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache ev)) (grinMain prog))
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
  (typeEnv, cache) <- evalAbstractOne add
  print $ PP typeEnv

runFact :: IO ()
runFact = do
  (typeEnv, cache) <- evalAbstractOne fact
  print $ PP typeEnv

runSum :: IO ()
runSum = do
  (typeEnv, cache) <- evalAbstractOne sumSimple
  print $ PP typeEnv
