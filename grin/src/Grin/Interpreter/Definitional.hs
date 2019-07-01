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

data NoHeapEnv m v = NoHeapEnv
  { _noHeapFuns :: Map.Map Name Exp
  , _noHeapOps  :: Map.Map Name ([v] -> m v)
  , _noHeapEnv  :: Env v
  }

makeLenses ''NoHeapEnv

newtype NoHeapT m a = NoHeapT { noHeapT :: ReaderT (NoHeapEnv m NHVal) m a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

runNoHeapT :: (Monad m) => Exp -> [(Name, [NHVal] -> m NHVal)] -> NoHeapT m a -> m a
runNoHeapT prog ops n = runReaderT (noHeapT n) noHeapEnv
  where
    (Program _ defs) = prog
    noHeapEnv = NoHeapEnv (Map.fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)) (Map.fromList ops) emptyEnv

data NHVal
  = NHNode Tag [Lit]
  | NHVal  Lit
  | NHUnit
  | NHLoc  Int
  deriving (Eq, Ord, Show)

instance (Applicative m, Monad m, MonadFail m) => Interpreter (NoHeapT m) where
  type IntpVal (NoHeapT m) = NHVal
  value :: Val -> (NoHeapT m) NHVal
  value = \case
    (ConstTagNode t0 ps) -> do
      p  <- askEnv
      vs <- pure $ map (lookupEnv p) ps
      pure $ NHNode t0 $ map (\(NHVal v) -> v) vs
    (Lit n) -> pure $ NHVal n
    Unit    -> pure $ NHUnit

  askEnv :: (NoHeapT m) (Env NHVal)
  askEnv = NoHeapT (_noHeapEnv <$> ask)

  localEnv :: Env NHVal -> (NoHeapT m) NHVal -> (NoHeapT m) NHVal
  localEnv e = NoHeapT . local (noHeapEnv %~ (const e)) . noHeapT

  lookupFun :: Name -> (NoHeapT m) Exp
  lookupFun funName = NoHeapT ((fromMaybe (error $ "Missing:" ++ show funName) . Map.lookup funName . _noHeapFuns) <$> ask)

  isOperation :: Name -> (NoHeapT m) Bool
  isOperation funName = NoHeapT ((Map.member funName . _noHeapOps) <$> ask)

  operation :: Name -> [NHVal] -> (NoHeapT m) NHVal
  operation funName params = NoHeapT $ do
    op <- (fromJust . Map.lookup funName . _noHeapOps) <$> ask
    lift (op params)

  ecase :: (Exp -> (NoHeapT m) NHVal) -> NHVal -> [Alt] -> (NoHeapT m) NHVal
  ecase ev v alts = evalBranch v $ head $ filter (\(Alt p b) -> match v p) alts
    where
      match :: NHVal -> CPat -> Bool
      match (NHNode t0 ps) (NodePat t1 vs) = t0 == t1
      match (NHVal l0)     (LitPat l1)     = l0 == l1
      match (NHNode{})     DefaultPat      = True
      match (NHVal{})      DefaultPat      = True
      match NHUnit         p               = error $ show ("matching failure:", NHUnit, p)
      match (NHLoc l)      p               = error $ show ("matching failure:", NHLoc l, p)
      match _                    _         = False

      evalBranch :: NHVal -> Alt -> (NoHeapT m) NHVal
      evalBranch (NHNode t0 vs) (Alt (NodePat t1 nps) body) = do
        p <- askEnv
        let p' = extendEnv p (nps `zip` (NHVal <$> vs))
        localEnv p' (ev body)
      evalBranch _              (Alt _               body) = ev body

evalNoHeap :: (Monad m, MonadFail m, MonadIO m) => Program -> m NHVal
evalNoHeap prog = do
  let ops = [ ("prim_int_add", prim_int_add)
            , ("prim_int_sub", prim_int_sub)
            , ("prim_int_mul", prim_int_mul)
            , ("prim_int_print", prim_int_print)
            , ("prim_int_eq", prim_int_eq)
            ]
  runNoHeapT prog ops (eval (grinMain prog))
  where
    prim_int_add    [(NHVal (LInt64 a)),(NHVal (LInt64 b))] = pure (NHVal (LInt64 (a + b)))
    prim_int_sub    [(NHVal (LInt64 a)),(NHVal (LInt64 b))] = pure (NHVal (LInt64 (a - b)))
    prim_int_mul    [(NHVal (LInt64 a)),(NHVal (LInt64 b))] = pure (NHVal (LInt64 (a * b)))
    prim_int_eq     [(NHVal (LInt64 a)),(NHVal (LInt64 b))] = pure (NHVal (LBool (a == b)))
    prim_int_print  [(NHVal (LInt64 i))] = liftIO $ print i >> pure NHUnit

-- * Test runs

runSum :: IO ()
runSum = do
  print =<< evalNoHeap sump

runFact :: IO ()
runFact = do
  print =<< evalNoHeap fact
