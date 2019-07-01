{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
module Grin.Interpreter.Definitional where

import Control.Monad.Fail
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Trans.Reader
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Grin.Exp
import Grin.Interpreter.Base
import Lens.Micro.Platform
import qualified Data.Map as Map


-- * NoHeap interpreter


data Node = Node Tag [Lit]
  deriving (Eq, Ord, Show)

data DVal
  = DNode Node
  | DVal  Lit
  | DUnit
  | DLoc  Int
  deriving (Eq, Ord, Show)


data NoHeapEnv m v = NoHeapEnv
  { _noHeapFuns :: Map.Map Name Exp
  , _noHeapOps  :: Map.Map Name ([v] -> m v)
  , _noHeapEnv  :: Env v
  }

makeLenses ''NoHeapEnv

newtype NoHeapT m a = NoHeapT { noHeapT :: ReaderT (NoHeapEnv m DVal) m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runNoHeapT :: (Monad m) => Exp -> [(Name, [DVal] -> m DVal)] -> NoHeapT m a -> m a
runNoHeapT prog ops n = runReaderT (noHeapT n) noHeapEnv
  where
    (Program _ defs) = prog
    noHeapEnv = NoHeapEnv (Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)) (Map.fromList ops) emptyEnv


instance (Applicative m, Monad m, MonadFail m) => Interpreter (NoHeapT m) where
  type IntpVal (NoHeapT m) = DVal
  value :: Val -> (NoHeapT m) DVal
  value = \case
    (ConstTagNode t0 ps) -> do
      p  <- askEnv
      vs <- pure $ map (lookupEnv p) ps
      pure $ DNode $ Node t0 $ map (\(DVal v) -> v) vs
    (Lit n) -> pure $ DVal n
    Unit    -> pure $ DUnit

  askEnv :: (NoHeapT m) (Env DVal)
  askEnv = NoHeapT (_noHeapEnv <$> ask)

  localEnv :: Env DVal -> (NoHeapT m) DVal -> (NoHeapT m) DVal
  localEnv e = NoHeapT . local (noHeapEnv %~ (const e)) . noHeapT

  lookupFun :: Name -> (NoHeapT m) Exp
  lookupFun funName = NoHeapT ((fromMaybe (error $ "Missing:" ++ show funName) . Map.lookup funName . _noHeapFuns) <$> ask)

  isOperation :: Name -> (NoHeapT m) Bool
  isOperation funName = NoHeapT ((Map.member funName . _noHeapOps) <$> ask)

  operation :: Name -> [DVal] -> (NoHeapT m) DVal
  operation funName params = NoHeapT $ do
    op <- (fromJust . Map.lookup funName . _noHeapOps) <$> ask
    lift (op params)

  ecase :: (Exp -> (NoHeapT m) DVal) -> DVal -> [Alt] -> (NoHeapT m) DVal
  ecase ev v alts = evalBranch v $ head $ filter (\(Alt p b) -> match v p) alts
    where
      match :: DVal -> CPat -> Bool
      match (DNode (Node t0 ps))  (NodePat t1 vs) = t0 == t1
      match (DVal l0)             (LitPat l1)     = l0 == l1
      match (DNode{})             DefaultPat      = True
      match (DVal{})              DefaultPat      = True
      match DUnit                 p               = error $ show ("matching failure:", DUnit, p)
      match (DLoc l)              p               = error $ show ("matching failure:", DLoc l, p)
      match _                     _               = False

      evalBranch :: DVal -> Alt -> (NoHeapT m) DVal
      evalBranch (DNode (Node t0 vs)) (Alt (NodePat t1 nps) body) = do
        p <- askEnv
        let p' = extendEnv p (nps `zip` (DVal <$> vs))
        localEnv p' (ev body)
      evalBranch _                    (Alt _               body) = ev body

evalNoHeap :: (Monad m, MonadFail m, MonadIO m) => Program -> m DVal
evalNoHeap prog = do
  let ops = [ ("prim_int_add", prim_int_add)
            , ("prim_int_sub", prim_int_sub)
            , ("prim_int_mul", prim_int_mul)
            , ("prim_int_print", prim_int_print)
            , ("prim_int_eq", prim_int_eq)
            ]
  runNoHeapT prog ops (eval (grinMain prog))
  where
    prim_int_add    [(DVal (LInt64 a)),(DVal (LInt64 b))] = pure (DVal (LInt64 (a + b)))
    prim_int_sub    [(DVal (LInt64 a)),(DVal (LInt64 b))] = pure (DVal (LInt64 (a - b)))
    prim_int_mul    [(DVal (LInt64 a)),(DVal (LInt64 b))] = pure (DVal (LInt64 (a * b)))
    prim_int_eq     [(DVal (LInt64 a)),(DVal (LInt64 b))] = pure (DVal (LBool (a == b)))
    prim_int_print  [(DVal (LInt64 i))] = liftIO $ print i >> pure DUnit

-- * Test runs

runSum :: IO ()
runSum = do
  print =<< evalNoHeap sump

runFact :: IO ()
runFact = do
  print =<< evalNoHeap fact
