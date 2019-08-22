{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, InstanceSigs, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, RecordWildCards #-}
module Grin.Interpreter.Abstract.TypeInference where

import Control.Applicative (Alternative(..))
import Control.Monad (forM_, msum)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic hiding (fail)
import Data.Function (fix)
import Data.Maybe (fromJust)
import Data.Maybe (isNothing)
import Grin.Exp (Program, eName, externals, Exp(SApp))
import Grin.TypeEnv hiding (TypeEnv(..), Loc)
import Grin.Value (Name, SimpleValue)
import Grin.Interpreter.Base (baseEval)
import Grin.Pretty hiding (SChar)
import Prelude hiding (fail)
import Grin.GExpToExp (gexpToExp)

import Grin.Interpreter.Store (Store(..))
import Grin.Interpreter.Env (Env(..))
import qualified Data.List as List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Grin.TypeEnv as Grin
import qualified Grin.Examples as Examples

import Grin.Interpreter.Abstract.Base
import Grin.Interpreter.Abstract.Interpreter


-- * Tests

tests :: IO ()
tests = do
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.add))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.fact))
  print =<< (PP <$> (typeInference $ gexpToExp $ Examples.sumSimple))

typeInference :: (Monad m, MonadFail m, MonadIO m) => Program -> m Grin.TypeEnv
typeInference = fmap (calcTypeEnv . fst) . abstractEval

abstractEval :: (Monad m, MonadFail m, MonadIO m) => Program -> m (TypeEnv, Cache)
abstractEval prog = do
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
  (\(_,tc,_) -> tc) <$> runAbstractT prog ops (fixCache (fix (evalCache baseEval)) (SApp "main" []))
  where
    exts = externals prog
    prim_int_add    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_sub    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_mul    = (ST ST_Int64, [ST ST_Int64, ST ST_Int64])
    prim_int_eq     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_gt     = (ST ST_Bool,  [ST ST_Int64, ST ST_Int64])
    prim_int_print  = (UT, [ST ST_Int64])


-- * Convert Abstract.TypeEnv to Grin.TypeEnv

tToType :: Map.Map Loc Int -> T -> Type
tToType ml = \case
  UT   -> T_SimpleType $ T_Unit
  ST s -> T_SimpleType $ stToSimpleType ml s
  NT (Node t ps) -> T_NodeSet $ Map.singleton t (stToSimpleType ml <$> ps)

typeOfValue :: Grin.Value.SimpleValue -> Type
typeOfValue = tToType mempty . typeOfSimpleValue

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
    (locToHeap, locationNodeSet) = locsToLocation _heap
