{-# LANGUAGE LambdaCase, TypeFamilies, InstanceSigs #-}
module Tutorial.Chapter02.Exercise02 where

import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Data.Maybe
import Data.Function (fix)
import Grin.Exp
import qualified Grin.TypeEnv as Grin
import Grin.Value hiding (Val, Node)
import Lens.Micro.Platform
import Prelude hiding (fail)

import Grin.Interpreter.Env (Env)
import qualified Grin.Interpreter.Env as Env
import Grin.Interpreter.Store (Store(..))
import qualified Grin.Interpreter.Store as Store
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.Value as Grin

import Grin.Interpreter.Abstract
  ( AbstractT, Cache, TypeEnv, T(..), ST(..), Loc(..), AbsStore, AbsEnv(..), AbsState(..), Node(..)
  , evalCache, fixCache, runAbstractT, absStr, appendFunOut, absEnv, appendEnvOut, typeOfSimpleValue, calcTypeEnv
  )
import Tutorial.Chapter01.Exercise02 as Exercise (grinMain)
import Tutorial.Chapter02.Exercise01 as Exercise



{-
TODO: Explain the exercise.
Implement the functions, find the differences
-}




forMonadPlus :: (MonadPlus m) => [a] -> (a -> m b) -> m b
forMonadPlus xs k = msum (map k xs)

instance (Monad m, MonadIO m, MonadFail m) => Interpreter (AbstractT m) where
  type Val          (AbstractT m) = T
  type HeapVal      (AbstractT m) = Node
  type StoreVal     (AbstractT m) = Set Node
  type Addr         (AbstractT m) = Loc
  type NewStoreInfo (AbstractT m) = Name



  findStore :: T -> AbstractT m T
  findStore v = do
    s <- getStore
    a <- val2addr v
    forMonadPlus (Set.toList $ Store.lookup a s) heapVal2val

  bindPattern :: T -> (Tag, [Name]) -> AbstractT m [(Name, T)]
  bindPattern _t (_tag, _ps) = error "TODO"

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase _ev0 _v _alts = error "TODO"

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem x = (fmap (Set.insert n) x) `mplus` (Just (Set.singleton n))
    updateStore (\(Store m) -> Store (Map.alter changeElem a m))


  localEnv :: Env T -> AbstractT m T -> AbstractT m T
  localEnv env m = do
    appendEnvOut env
    local (absEnv .~ env) m


  literal :: Grin.Literal -> AbstractT m T
  literal = \case
    (Grin.LNode (Grin.Node tag ps)) -> do
      p  <- askEnv
      ts <- pure $ map (Env.lookup p) ps
      pure $ NT $ Node tag $ map (\case
        ST t -> t
        other -> error $ unwords ["value", show other] -- TODO: Include type error
        ) ts
    (Grin.LVal l) -> pure $ typeOfSimpleValue l

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

  name2NewStoreInfo :: Name -> AbstractT m Name
  name2NewStoreInfo = pure

  unit :: AbstractT m T
  unit = pure UT

  -- non-pure
  askEnv :: AbstractT m (Env T)
  askEnv = _absEnv <$> ask


  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ unwords ["lookupFun", nameString fn]) . Map.lookup fn . _absFun) <$> ask

  isOperation :: Name -> AbstractT m Bool
  isOperation n = (Map.member n . _absOps) <$> ask

  operation :: Name -> [T] -> AbstractT m T
  operation n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ unwords ["operation", nameString n, show ps, show ts]
    appendFunOut (n,ts,r)
    pure r

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev0 fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = Env.insert (fps `zip` vs) Env.empty
    v <- localEnv p' (ev0 body)
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
    pure $ ST $ ST_Loc l













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
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache Exercise.eval)) (Exercise.grinMain prog))
  where
    exts = externals prog
    prim_int_add    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_sub    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_mul    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_eq     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_gt     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_print  = (UT, [ST ST_Int64])
