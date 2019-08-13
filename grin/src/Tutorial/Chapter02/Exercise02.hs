{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Tutorial.Chapter02.Exercise02 where

import Grin.Interpreter.Base
import Grin.Interpreter.Abstract
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
import Grin.Value hiding (Val, Node)
import Grin.Interpreter.Base
import Grin.Pretty hiding (SChar)
import Lens.Micro.Platform
import Prelude hiding (fail)

import qualified Data.List as List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.TypeEnv as Grin
import qualified Grin.Value as Grin
import qualified Text.PrettyPrint.ANSI.Leijen as PP


-- newtype AbstractTE m a = AbstractTE (AbstractT m)

instance (Monad m, MonadIO m, MonadFail m) => Interpreter (AbstractT m) where
  type Val          (AbstractT m) = T
  type HeapVal      (AbstractT m) = Node
  type StoreVal     (AbstractT m) = Set Node
  type Addr         (AbstractT m) = Loc
  type NewStoreInfo (AbstractT m) = Name

  value :: Grin.Val -> AbstractT m T
  value = \case
    (CNode (Grin.Node tag ps)) -> do
      p  <- askEnv
      ts <- pure $ map (lookupEnv p) ps
      pure $ NT $ Node tag $ map (\case
        ST t -> t
        other -> error $ unwords ["value", show other] -- TODO: Include type error
        ) ts
    (Lit l) -> pure $ typeOfLit l
    Unit    -> pure $ UT
    (Var v) -> error $ unwords ["value", nameString v]

  val2addr :: T -> AbstractT m Loc
  val2addr = \case
    ST (SLoc l) -> pure l
    other       -> error $ unwords ["val2addr", show other]

  addr2val :: Loc -> AbstractT m T
  addr2val l = pure $ ST $ SLoc l

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

  -- TODO: Exercise
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
  lookupFun fn = (fromMaybe (error $ unwords ["lookupFun", nameString fn]) . Map.lookup fn . _absFun) <$> ask

  isOperation :: Name -> AbstractT m Bool
  isOperation n = (Map.member n . _absOps) <$> ask

  operation :: Name -> [T] -> AbstractT m T
  operation n ps = do
    (r,ts) <- (fromJust . Map.lookup n . _absOps) <$> ask
    when (ps /= ts) $ error $ unwords ["operation", nameString n, show ps, show ts]
    appendFunOut (n,ts,r)
    pure r

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase ev0 v alts = do
    selectedAlts <- filterM isMatching alts
    forMonadPlus selectedAlts extendAlt
    where
      isMatching (Alt DefaultPat      _) = pure True
      isMatching (Alt (LitPat l)      _) = (v ==) <$> value (Lit l)
      isMatching (Alt (NodePat t _ps) _) = case v of
        NT (Node t0 _) -> pure $ t == t0
        _nonNodeType   -> pure False
      isMatching overGenerative = error $ show overGenerative

      extendAlt alt@(Alt DefaultPat     _body) = ev0 alt
      extendAlt alt@(Alt (LitPat _)     _body) = ev0 alt
      extendAlt alt@(Alt (NodePat _ ns) _body) = case v of
        NT (Node _t0 vs) -> do
          p <- askEnv
          localEnv (extendEnv p (ns `zip` (ST <$> vs))) $ ev0 alt
        nonNodeType -> error $ show nonNodeType
      extendAlt overGenerative = error $ show overGenerative

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev0 fn vs = do
    (Def _ fps body) <- lookupFun fn
    let p' = extendEnv emptyEnv (fps `zip` vs)
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
