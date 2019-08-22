{-# LANGUAGE LambdaCase, TypeFamilies, InstanceSigs #-}
module Tutorial.Chapter02.Exercise02 where

import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Function (fix)
import Grin.Exp (Exp(..), CPat(..), Alt, Program, externals, eName)
import qualified Grin.TypeEnv as Grin
import Grin.Value hiding (Val, Node)
import Lens.Micro.Platform
import Prelude hiding (fail)
import Data.Maybe (mapMaybe)

import Grin.Interpreter.Env (Env)
import Grin.Interpreter.Store (Store(..))
import qualified Grin.Interpreter.Env as Env
import qualified Grin.Interpreter.Store as Store
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.Value as Grin

import Grin.Interpreter.Abstract.Base
  ( AbstractT(..), Cache, TypeEnv, T(..), ST(..), Loc(..), AbsStore(..), AbsEnv(..), AbsState(..), Node(..)
  , runAbstractT, absStr, absEnv, forMonadPlus, typeOfSimpleValue
  )
import Grin.Interpreter.Abstract.Interpreter
  ( evalCache, fixCache, collectFunctionType, collectEnv
  )
import Grin.Interpreter.Abstract.TypeInference (calcTypeEnv)

import Tutorial.Chapter02.Exercise01 as Exercise



{-
Exercise:
Read the 'Abstracting Closures' from
https://plum-umd.github.io/abstracting-definitional-interpreters/#%28part._s~3aabstracting-closures%29

Despite the Chapter 3.3 describes how to store closures, in GRIN there is no such thing. The take-away
from that chapter is that the heap now should contain a Set of Node values, which are joined when
the non-deterministic choice happen in the control flow.

Exercise: Read Chatper 3.2.1
https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=83

Exercise:
Read the Grin.Interpreter.Abstract.Base module

One simple abstract interpretation of a GRIN program is the type inference, of the GRIN variables, functions,
and Heap locations.
-}


instance (Monad m, MonadIO m, MonadFail m) => Exercise.Interpreter (AbstractT m) where
  type Val          (AbstractT m) = T
  type HeapVal      (AbstractT m) = Node
  type Addr         (AbstractT m) = Loc

  -- As you remember, the Abstract store can hold a Set of Nodes. The interpretation of
  -- the Fetch operation is to retrieve the 'Set Node' and continue the rest of the
  -- computation one by one, to achieve this we need to use the Non-Det monad, which
  -- is implemented in the LogicT monad.
  fetchStore :: T -> AbstractT m T
  fetchStore v = do
    AbsState s <- get
    a <- val2addr v
    forMonadPlus (Set.toList $ Store.lookup a s) heapVal2val

  bindPattern :: T -> (Tag, [Name]) -> AbstractT m [(Name, T)]
  bindPattern t (tag,ps) =
    -- Exercise: Similar to the bindPattern in the definitional interpreter
    -- match the tag from the value with the names and return the association list.
    -- If the tag doesn't match use the mzero instead of throwing an error.
    undefined

  evalCase :: (Exp -> AbstractT m T) -> T -> [Alt] -> AbstractT m T
  evalCase ev0 v alts =
    -- Exercise: Similar to the definitional interpreter, filter out
    -- the matching alts. Using the forMonadPlus operator for all the
    -- matchin ones extend the environment if necessary and evaluate the
    -- body of the alt
    undefined

  extStore :: T -> T -> AbstractT m ()
  extStore v0 v1 = do
    a <- val2addr v0
    n <- val2heapVal v1
    let changeElem Nothing  = Just (Set.singleton n)
        changeElem (Just m) = Just (Set.insert n m)
    AbstractT $ (modify (over absStr (\(Store m) -> Store (Map.alter changeElem a m))))

  localEnv :: Env T -> AbstractT m T -> AbstractT m T
  localEnv env m = do
    collectEnv env
    local (absEnv .~ env) m


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

  askEnv :: AbstractT m (Env T)
  askEnv = _absEnv <$> ask

  lookupFun :: Name -> AbstractT m Exp
  lookupFun fn = (fromMaybe (error $ unwords ["lookupFun", nameString fn]) . Map.lookup fn . _absFun) <$> ask

  isExternal :: Name -> AbstractT m Bool
  isExternal n = (Map.member n . _absExt) <$> ask

  external :: Name -> [T] -> AbstractT m T
  external n ps = do
    -- Exercise:
    -- Lookup the environment, check if the given parameters has the same type if not throw an 'error'
    -- If they have the same type than return the return type of the external.
    -- Use the collectFunctionType to register the learn types of the function
    undefined

  funCall :: (Exp -> AbstractT m T) -> Name -> [T] -> AbstractT m T
  funCall ev0 fn vs = do
    -- Exercise:
    -- Lookup the (Def _ ps body) constructor of the function, create a new environment binding its
    -- arguments to the given values to call with, after the return of the function register its
    -- type with the collectFunctionType
    undefined

  allocStore :: Name -> AbstractT m T
  allocStore name = pure $ ST $ ST_Loc $ Loc name

-- * Implemented type inference

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
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache Exercise.eval)) (SApp "main" []))
  where
    exts = externals prog
    prim_int_add    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_sub    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_mul    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_eq     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_gt     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_print  = (UT, [ST ST_Int64])
