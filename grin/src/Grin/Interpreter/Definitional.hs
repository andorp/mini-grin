{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Definitional where

import Prelude hiding (fail)
import Control.Monad (forM_, when)
import Control.Monad.Fail
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Reader hiding (ask, local)
import Control.Monad.Trans.State hiding (state, get)
import Data.Maybe (fromJust, mapMaybe, fromMaybe, isNothing)
import Grin.Exp hiding (Loc, Val)
import qualified Grin.Exp as Grin (Val)
import Grin.Interpreter.Base
import Lens.Micro.Platform
import qualified Data.Map as Map
import Data.Text (Text, isPrefixOf, unpack)
import Data.Word
import Data.Int


-- * Definitional Interpreter

data SVal
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SString Text
  | SChar   Char
  | SLoc    Loc
  deriving (Eq, Ord, Show)

lit2sval :: Lit -> SVal
lit2sval = \case
  LInt64  i -> SInt64 i
  LWord64 w -> SWord64 w
  LFloat  f -> SFloat f
  LBool   b -> SBool b
  LString s -> SString s
  LChar   c -> SChar c

data Node = Node Tag [SVal]
  deriving (Eq, Ord, Show)

newtype Loc = Loc Int
  deriving (Eq, Ord, Show)

data DVal
  = DNode Node
  | DVal  SVal
  | DUnit
  deriving (Eq, Ord, Show)

data DefEnv m v = DefEnv
  { _defFuns :: Map.Map Name Exp
  , _defOps  :: Map.Map Name ([v] -> m v)
  , _defEnv  :: Env v
  }

makeLenses ''DefEnv

newtype DefinitionalT m a = DefinitionalT
  { definitionalT :: StateT (Store Loc Node) (ReaderT (DefEnv m DVal) m) a
  }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader (DefEnv m DVal), MonadState (Store Loc Node))

runDefinitionalT :: (Monad m) => Exp -> [(Name, [DVal] -> m DVal)] -> DefinitionalT m a -> m a
runDefinitionalT prog ops n = runReaderT (evalStateT (definitionalT n) emptyStore) definitional
  where
    definitional =
      DefEnv
        (programToDefs prog)
        (Map.fromList ops)
        emptyEnv

instance (Applicative m, Monad m, MonadFail m) => Interpreter (DefinitionalT m) where
  type Val      (DefinitionalT m) = DVal
  type HeapVal  (DefinitionalT m) = Node
  type StoreVal (DefinitionalT m) = Node
  type Addr     (DefinitionalT m) = Loc

  value :: Grin.Val -> DefinitionalT m DVal
  value = \case
    (ConstTagNode t0 ps) -> do
      p  <- askEnv
      vs <- pure $ map (lookupEnv p) ps
      pure $ DNode $ Node t0 $ map (\case
        DVal v -> v
        other -> error $ show ("value", other)
        ) vs
    (Lit l) -> pure $ DVal $ lit2sval l
    Unit    -> pure $ DUnit

  val2addr :: DVal -> DefinitionalT m Loc
  val2addr = \case
    (DVal (SLoc l)) -> pure l
    other           -> error $ show ("val2addr", other)

  addr2val :: Loc -> DefinitionalT m DVal
  addr2val = pure . DVal . SLoc

  heapVal2val :: Node -> DefinitionalT m DVal
  heapVal2val = pure . DNode

  val2heapVal :: DVal -> DefinitionalT m Node
  val2heapVal = \case
    DNode n -> pure n
    other   -> error $ show ("val2HeapVal", other)

  unit :: DefinitionalT m DVal
  unit = pure DUnit

  bindPattern :: DVal -> (Tag, [Name]) -> DefinitionalT m [(Name, DVal)]
  bindPattern (DNode (Node t0 vs)) (t1, ps)
    | t0 == t1  = pure (ps `zip` (DVal <$> vs))
    | otherwise = error "bindPattern"

  askEnv :: (DefinitionalT m) (Env DVal)
  askEnv = _defEnv <$> ask

  localEnv :: Env DVal -> (DefinitionalT m) DVal -> (DefinitionalT m) DVal
  localEnv e = local (defEnv .~ e)

  lookupFun :: Name -> (DefinitionalT m) Exp
  lookupFun funName = (fromMaybe (error $ "Missing:" ++ show funName) . Map.lookup funName . _defFuns) <$> ask

  isOperation :: Name -> (DefinitionalT m) Bool
  isOperation funName = (Map.member funName . _defOps) <$> ask

  operation :: Name -> [DVal] -> (DefinitionalT m) DVal
  operation funName params = DefinitionalT $ do
    op <- lift ((fromJust . Map.lookup funName . _defOps) <$> ask)
    lift (lift (op params))

  evalCase :: (Exp -> (DefinitionalT m) DVal) -> DVal -> [Alt] -> (DefinitionalT m) DVal
  evalCase ev v alts = evalBranch v $ head $ filter (\(Alt p b) -> match v p) alts
    where
      match :: DVal -> CPat -> Bool
      match DUnit                 p               = error $ show ("matching failure:", DUnit, p)
      match (DVal (SLoc l))       p               = error $ show ("matching failure:", l, p)
      match (DNode (Node t0 ps))  (NodePat t1 vs) = t0 == t1
      match (DVal l0)             (LitPat l1)     = l0 == (lit2sval l1)
      match (DNode{})             DefaultPat      = True
      match (DVal{})              DefaultPat      = True
      match _                     _               = False

      evalBranch :: DVal -> Alt -> (DefinitionalT m) DVal
      evalBranch (DNode (Node t0 vs)) (Alt (NodePat t1 nps) body) = do
        p <- askEnv
        let p' = extendEnv p (nps `zip` (DVal <$> vs))
        localEnv p' (ev body)
      evalBranch _                    (Alt _               body) = ev body

  funCall :: (Exp -> DefinitionalT m DVal) -> Name -> [DVal] -> DefinitionalT m DVal
  funCall ev fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = extendEnv emptyEnv (fps `zip` vs)
    localEnv p' (ev body)

  getStore :: DefinitionalT m (Store Loc Node)
  getStore = DefinitionalT get

  putStore :: Store Loc Node -> DefinitionalT m ()
  putStore s = state (\_ -> ((), s))

  updateStore :: (Store Loc Node -> Store Loc Node) -> DefinitionalT m ()
  updateStore f = state (\s -> ((), f s))

  nextLocStore :: Store Loc Node -> DefinitionalT m Loc
  nextLocStore (Store s) = pure $ Loc $ Map.size s

  allocStore :: DefinitionalT m DVal
  allocStore = do
    s <- getStore
    a <- nextLocStore s
    addr2val a

  findStore :: DVal -> DefinitionalT m DVal
  findStore l = do
    s <- getStore
    a <- val2addr l
    heapVal2val $ storeFind s a

  extStore :: DVal -> DVal -> DefinitionalT m ()
  extStore l n = do
    a <- val2addr l
    v <- val2heapVal n
    updateStore (storeExt a v)

evalDefinitional :: (Monad m, MonadFail m, MonadIO m) => Program -> m DVal
evalDefinitional prog = do
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
  runDefinitionalT prog ops (eval (grinMain prog))
  where
    exts = externals prog
    prim_int_add    [(DVal (SInt64 a)),(DVal (SInt64 b))] = pure (DVal (SInt64 (a + b)))
    prim_int_sub    [(DVal (SInt64 a)),(DVal (SInt64 b))] = pure (DVal (SInt64 (a - b)))
    prim_int_mul    [(DVal (SInt64 a)),(DVal (SInt64 b))] = pure (DVal (SInt64 (a * b)))
    prim_int_eq     [(DVal (SInt64 a)),(DVal (SInt64 b))] = pure (DVal (SBool (a == b)))
    prim_int_gt     [(DVal (SInt64 a)),(DVal (SInt64 b))] = pure (DVal (SBool (a > b)))
    prim_int_print  [(DVal (SInt64 i))] = liftIO $ print i >> pure DUnit

-- * Test runs

runAdd :: IO ()
runAdd = do
  print =<< evalDefinitional add

runFact :: IO ()
runFact = do
  print =<< evalDefinitional fact

runSum :: IO ()
runSum = do
  print =<< evalDefinitional sumSimple
