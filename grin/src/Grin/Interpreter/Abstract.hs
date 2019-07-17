{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
module Grin.Interpreter.Abstract where

import Prelude hiding (fail)
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), forM_, msum)
import Data.Maybe (isNothing)
import Grin.Exp hiding (Val)
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
import Control.Monad.Trans.List
import Control.Monad.IO.Class (MonadIO(..))

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

data Node = Node Tag [ST]
  deriving (Eq, Ord, Show)

data T
  = ST ST
  | NT (Set Node)
  | UT
  | TV
  deriving (Eq, Ord, Show)

instance Semigroup T where
  TV <> v = v
  v <> TV = v
  (ST s1) <> (ST s2)
    | s1 == s2 = ST s1
    | otherwise = error $ "Semigroup T:" ++ show (s1, s2)
  (NT n1) <> (NT n2) = NT (n1 <> n2)
  t1 <> t2 = error $ "Semigroup T:" ++ show (t1, t2)

instance Monoid T where
  mempty  = TV
  mappend = (<>)

data H = H -- Heap
  deriving (Eq, Ord, Show)

data AbsEnv
  = AbsEnv
  { _absOps :: Map.Map Name (T, [T])
  , _absEnv :: Set Name         -- Names in scope
  , _absFun :: Map.Map Name Exp -- How to record the type of function parameters? As now the interpeter calls the
                                -- function during the evaluation, and the current parameters are recorded in the
                                -- environment? Maybe a state will be needed instead of the Reader monad?
                                -- Or the lookupFun and the SApp needs a different implementation? OR the SApp
                                -- should be implemented as the ecase?
  }

makeLenses ''AbsEnv

data AbsState
  = AbsState
  { _absStr :: Store H (Set Node)
  , _absVar :: Map.Map Name T
  } deriving (Show)

makeLenses ''AbsState

newtype AbstractT m a = AbstractT
  { abstractT :: ListT (StateT AbsState (ReaderT AbsEnv m)) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadState AbsState
    , MonadReader AbsEnv
    , Alternative
    , MonadPlus
    )

runAbstractT
  :: (Monad m, MonadFail m, MonadIO m)
  => Program -> [(Name, (T, [T]))]
  -> AbstractT m a -> m ([a], AbsState)
runAbstractT prog ops m =
  flip runReaderT (AbsEnv (Map.fromList ops) mempty (programToDefs prog)) $
  flip runStateT (AbsState emptyStore mempty) $
  runListT $
  abstractT $ m

typeOfLit :: Lit -> T
typeOfLit = \case
  LInt64  _ -> ST SInt64
  LWord64 _ -> ST SWord64
  LFloat  _ -> ST SFloat
  LBool   _ -> ST SBool
  LString _ -> ST SString
  LChar   _ -> ST SChar

instance (Monad m, MonadFail m) => Interpreter (AbstractT m) where
  type Val     (AbstractT m) = T
  type HeapVal (AbstractT m) = Set Node
  type Addr    (AbstractT m) = H

  value :: Grin.Val -> AbstractT m T
  value = \case
    (ConstTagNode tag ps) -> do
      p  <- askEnv
      ts <- pure $ map (lookupEnv p) ps
      pure $ NT $ Set.singleton $ Node tag $ map (\case
        ST t -> t
        other -> error $ show ("value", other) -- TODO: Include type error
        ) ts
    (Lit l) -> pure $ typeOfLit l
    Unit    -> pure $ UT

  val2addr :: T -> AbstractT m H
  val2addr = \case
    TV      -> pure H
    ST SLoc -> pure H
    other   -> error $ show ("val2addr", other)

  addr2val :: H -> AbstractT m T
  addr2val _ = pure $ ST SLoc

  val2heapVal :: T -> AbstractT m (Set Node)
  val2heapVal = \case
    NT n -> pure n
    other -> error $ show ("val2heapVal", other)

  heapVal2val :: (Set Node) -> AbstractT m T
  heapVal2val = pure . NT

  unit :: AbstractT m T
  unit = pure UT

  bindPattern :: T -> (Tag, [Name]) -> AbstractT m [(Name, T)]
  bindPattern t (tag,ps) = case t of
    NT nodes -> do
      [Node t1 ps1] <- pure $ Set.toList $ Set.filter (\(Node t0 ps0) -> t0 == tag) nodes
      pure $ ps `zip` (ST <$> ps1)
    other -> error $ show ("bindPattern", other)

  -- non-pure
  askEnv :: AbstractT m (Env T)
  askEnv = do
    ns <- _absEnv <$> ask
    (Env . flip Map.restrictKeys ns . _absVar) <$> get

  localEnv :: Env T -> AbstractT m T -> AbstractT m T
  localEnv (Env ns) m = do
    absVar %= (Map.unionWith (<>) ns)
    local (absEnv .~ (Map.keysSet ns)) m

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ show ("lookupFun", fn)) . Map.lookup fn . _absFun) <$> ask

  isOperation :: Name -> AbstractT m Bool
  isOperation n = (Map.member n . _absOps) <$> ask

  operation :: Name -> [T] -> AbstractT m T
  operation n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ show ("operation", ps, ts)
    pure r

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase ev _v alts = do
    msum $ map extendAlt alts
    where
      extendAlt (Alt DefaultPat     body) = ev body
      extendAlt (Alt (LitPat _)     body) = ev body
      extendAlt (Alt (NodePat _ ns) body) = do
        p <- askEnv
        let p' = extendEnv p (map (flip (,) TV) ns)
        localEnv p' $ ev body

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = extendEnv emptyEnv (fps `zip` vs)
    localEnv p' (ev body)

  getStore :: AbstractT m (Store H (Set Node))
  getStore = _absStr <$> Control.Monad.State.get

  updateStore :: (Store H (Set Node) -> Store H (Set Node)) -> AbstractT m ()
  updateStore = (absStr %=)

  nextLocStore :: Store H (Set Node) -> AbstractT m H
  nextLocStore _ = pure H

  allocStore :: AbstractT m T
  allocStore = pure $ ST SLoc

  findStore :: T -> AbstractT m T
  findStore v = do
    s <- getStore
    a <- val2addr v
    heapVal2val $ storeFind s a

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem x = (fmap (Set.union n) x) `mplus` (Just n)
    updateStore (\(Store m) -> Store (Map.alter changeElem a m))

evalAbstractOne :: (Monad m, MonadFail m, MonadIO m) => Program -> m AbsState
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
  -- runDefinitionalT prog ops (eval (grinMain prog))
  snd <$> runAbstractT prog ops (eval (grinMain prog))
  where
    exts = externals prog
    prim_int_add    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_sub    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_mul    = (ST SInt64, [ST SInt64, ST SInt64])
    prim_int_eq     = (ST SBool,  [ST SInt64, ST SInt64])
    prim_int_gt     = (ST SBool,  [ST SInt64, ST SInt64])
    prim_int_print  = (UT, [ST SInt64, ST SInt64])


runAdd :: IO ()
runAdd = do
  print =<< evalAbstractOne add

runFact :: IO ()
runFact = do
  print =<< evalAbstractOne fact

runSum :: IO ()
runSum = do
  print =<< evalAbstractOne sumSimple

