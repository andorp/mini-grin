{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), forM_, msum, filterM)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.RWS.Strict hiding (ask, local, get)
import Data.Function (fix)
import Data.Maybe (fromMaybe, fromJust)
import Data.Maybe (isNothing)
import Grin.Exp
import Grin.TypeEnv hiding (TypeEnv(..), Loc)
import Grin.Value (Name, Tag)
import Grin.Interpreter.Base
import Grin.Interpreter.Store
import Grin.Pretty hiding (SChar)
import Lens.Micro.Platform
import Prelude hiding (fail)
import Grin.GExpToExp (gexpToExp)

import Grin.Interpreter.Store (Store)
import qualified Grin.Interpreter.Store as Store
import Grin.Interpreter.Env (Env(..))
import qualified Grin.Interpreter.Env as Env
import qualified Data.List as List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.TypeEnv as Grin
import qualified Grin.Value as Grin
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Grin.Examples as Examples


data ST
  = ST_Int64
  | ST_Word64
  | ST_Float
  | ST_Bool
  | ST_Char
  | ST_Loc Loc
  deriving (Eq, Ord, Show)

instance Pretty ST where
  pretty = \case
    (ST_Loc h) -> cyan $ pretty h
    ty         -> red $ text $ show ty

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

-- The abstract location is generated by the variable
-- which was associated with the store. (l <- store _)
data Loc = Loc Name
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
  deriving (Eq)

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
  } deriving (Eq)

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
        (AbsEnv (Map.fromList ops) Env.empty (programToDefs prog))
        (AbsState Store.empty)))
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

typeOfSimpleValue :: Grin.SimpleValue -> T
typeOfSimpleValue = \case
  Grin.SInt64  _ -> ST ST_Int64
  Grin.SWord64 _ -> ST ST_Word64
  Grin.SFloat  _ -> ST ST_Float
  Grin.SBool   _ -> ST ST_Bool
  Grin.SChar   _ -> ST ST_Char

-- Chapter 2: Fill out the missing definitions that the 3 test pass
instance (Monad m, MonadIO m, MonadFail m) => Interpreter (AbstractT m) where
  type Val          (AbstractT m) = T
  type HeapVal      (AbstractT m) = Node
  type StoreVal     (AbstractT m) = Set Node
  type Addr         (AbstractT m) = Loc

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
    appendEnvOut env
    local (absEnv .~ env) m

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ unwords ["lookupFun", Grin.nameString fn]) . Map.lookup fn . _absFun) <$> ask

  isExternal :: Name -> AbstractT m Bool
  isExternal n = (Map.member n . _absOps) <$> ask

  external :: Name -> [T] -> AbstractT m T
  external n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ unwords ["external", Grin.nameString n, show ps, show ts]
    appendFunOut (n,ts,r)
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
          localEnv (Env.insert (ns `zip` (ST <$> vs)) p) $ ev0 alt
        nonNodeType -> error $ show nonNodeType
      extendAlt overGenerative = error $ show overGenerative

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev0 fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = Env.insert (fps `zip` vs) Env.empty
    v <- localEnv p' (ev0 body)
    appendFunOut (fn,vs,v)
    pure v

  getStore :: AbstractT m AbsStore
  getStore = _absStr <$> Control.Monad.State.get

  updateStore :: (AbsStore -> AbsStore) -> AbstractT m ()
  updateStore = (absStr %=)

  allocStore :: Name -> AbstractT m T
  allocStore ctx = do
    s <- getStore
    let l = Loc ctx
    pure $ ST $ ST_Loc l

  findStore :: T -> AbstractT m T
  findStore v = do
    s <- getStore
    a <- val2addr v
    forMonadPlus (Set.toList $ Store.lookup a s) heapVal2val

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem x = (fmap (Set.insert n) x) `mplus` (Just (Set.singleton n))
    updateStore (\(Store m) -> Store (Map.alter changeElem a m))

putStore :: Interpreter m => Store (Addr m) (StoreVal m) -> m ()
putStore o = updateStore (const o)

evalCache
  :: (Monad m, MonadFail m, MonadIO m)
  => ((Exp -> AbstractT m T) -> (Exp -> AbstractT m T)) -> (Exp -> AbstractT m T) -> Exp -> AbstractT m T
evalCache ev0 ev1 e = do
  p   <- askEnv
  o   <- getStore
  case (exp2CExp e) of
    Nothing -> do
      ev0 ev1 e
    Just ce -> do
      let c = Config { cfgEnv = p, cfgStore = o, cfgExp = ce }
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

typeInference :: (Monad m, MonadFail m, MonadIO m) => Program -> m Grin.TypeEnv
typeInference = fmap (calcTypeEnv . fst) . evalAbstract

evalAbstract :: (Monad m, MonadFail m, MonadIO m) => Program -> m (TypeEnv, Cache)
evalAbstract prog = do
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
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache baseEval)) (grinMain prog))
  where
    exts = externals prog
    prim_int_add    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_sub    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_mul    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_eq     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_gt     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_print  = (UT, [ST ST_Int64])

-- * Tests

tests :: IO ()
tests = do
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.add))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.fact))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.sumSimple))

-- * Convert Abstract.TypeEnv to Grin.TypeEnv

tToType :: Map.Map Loc Int -> T -> Type
tToType ml = \case
  UT   -> T_SimpleType $ T_Unit
  ST s -> T_SimpleType $ stToSimpleType ml s
  NT (Node t ps) -> T_NodeSet $ Map.singleton t (stToSimpleType ml <$> ps)

stToSimpleType :: Map.Map Loc Int -> ST -> SimpleType
stToSimpleType ml = \case
  ST_Int64  -> T_Int64
  ST_Word64 -> T_Word64
  ST_Float  -> T_Float
  ST_Bool   -> T_Bool
  ST_Char   -> T_Char
  ST_Loc l  -> T_Location [ml Map.! l]

locsToLocation :: Store Loc (Set Node) -> (Map.Map Loc Int, Map.Map Int NodeSet)
locsToLocation (Store m0) = (locToHeap, storeToHeap m0)
  where
    locToHeap = Map.fromList $ zip (Map.keys m0) [0..]

    storeToHeap :: Map.Map Loc (Set Node) -> Map.Map Int NodeSet
    storeToHeap = Map.map (Set.foldl' (flip insertNode) mempty) . Map.mapKeys (locToHeap Map.!)

    insertNode :: Node -> NodeSet -> NodeSet
    insertNode (Node t ps) = flip Map.alter t $ \case
      Nothing  -> Just (stToSimpleType locToHeap <$> ps)
      Just ps0 -> Just $ zipWith (\p0 p1 -> if p0 /= p1 then error $ show (p0,p1) else p0) ps0 (stToSimpleType locToHeap <$> ps)

applyIfBoth :: (Applicative f, Alternative f) => (a -> a -> a) -> f a -> f a -> f a
applyIfBoth f a b = (f <$> a <*> b) <|> a <|> b

unifyNodeSet :: NodeSet -> NodeSet -> Maybe NodeSet
unifyNodeSet ns0 ns1 = sequence $ Map.unionWith unifyParams (Map.map Just ns0) (Map.map Just ns1)
  where
    unifyParams :: Maybe [SimpleType] -> Maybe [SimpleType] -> Maybe [SimpleType]
    unifyParams ms0 ms1 = msum
      [ do s0 <- ms0
           s1 <- ms1
           zipWithM unifySimpleType s0 s1
      , ms0
      , ms1
      ]

unifySimpleType :: SimpleType -> SimpleType -> Maybe SimpleType
unifySimpleType t1 t2 = case (t1,t2) of
  (T_Location l1, T_Location l2)  -> Just $ T_Location (List.nub $ l1 ++ l2)
  (st1, st2) | st1 == st2         -> Just st2
             | otherwise          -> Nothing

unifyType :: Type -> Type -> Maybe Type
unifyType t1 t2 = case (t1,t2) of
  (T_NodeSet n1,     T_NodeSet n2)     -> T_NodeSet    <$> unifyNodeSet n1 n2
  (T_SimpleType st1, T_SimpleType st2) -> T_SimpleType <$> unifySimpleType st1 st2
  _ -> Nothing

funcToFunctions :: Map.Map Loc Int -> Map.Map Name FunctionT -> Map.Map Name (Type, [Type])
funcToFunctions ml = Map.map
  (\(FunctionT (r,ps))
    -> ( fromJust $ unifyTypes $ Set.map (tToType ml) r
       , fromJust $ sequence $ fmap (unifyTypes . Set.map (tToType ml)) ps
       )
  )

varToVariable :: Map.Map Loc Int -> Env (Set T) -> Map.Map Name Type
varToVariable ml (Env m) = Map.map (fromJust . unifyTypes . Set.map (tToType ml)) m

unifyTypes :: Set Type -> Maybe Type
unifyTypes = unifyTypes' . Set.toList where
  unifyTypes' []     = Nothing
  unifyTypes' (t:ts) = foldM unifyType t ts

calcTypeEnv :: TypeEnv -> Grin.TypeEnv
calcTypeEnv TypeEnv{..} = Grin.TypeEnv
  { Grin._location = locationNodeSet
  , Grin._variable = varToVariable locToHeap _variable
  , Grin._function = funcToFunctions locToHeap _function
  }
  where
    (locToHeap, locationNodeSet) = locsToLocation _location
