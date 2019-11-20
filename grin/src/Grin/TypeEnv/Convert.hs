{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards #-}
module Grin.TypeEnv.Convert where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import Grin.Interpreter.Env
import Grin.Interpreter.Store
import Grin.Value as Grin hiding (Node)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Grin.TypeEnv.Intermediate as Intermed
import qualified Grin.TypeEnv.Result as Result


-- TODO: Collect errors.
calcTypeEnv :: Intermed.TypeEnv -> Result.TypeEnv
calcTypeEnv Intermed.TypeEnv{..} = Result.TypeEnv
  { Result._location = locationNodeSet
  , Result._variable = varToVariable locToHeap _variable
  , Result._function = funcToFunctions locToHeap _function
  }
  where
    (locToHeap, locationNodeSet) = locsToLocation _heap

--indexMapToList :: Map.Map Int a -> [a]
--indexMapToList m = [ a | i <- [0..maximum (Map.keys m)], let a = fromMaybe (error $ "Missing index:" ++ show i) (Map.lookup i m) ]

tToType :: Map.Map Intermed.Loc Int -> Intermed.T -> Result.Type
tToType ml = \case
  Intermed.UT
    -> Result.T_SimpleType $ Result.T_Unit
  Intermed.ST s
    -> Result.T_SimpleType $ stToSimpleType ml s
  Intermed.NT (Intermed.Node t ps)
    -> Result.T_NodeSet $ Map.singleton t (stToSimpleType ml <$> ps)

typeOfValue :: Grin.SimpleValue -> Result.Type
typeOfValue = tToType mempty . Intermed.typeOfSimpleValue

stToSimpleType :: Map.Map Intermed.Loc Int -> Intermed.ST -> Result.SimpleType
stToSimpleType ml = \case
  Intermed.ST_Int64  -> Result.T_Int64
  Intermed.ST_Word64 -> Result.T_Word64
  Intermed.ST_Float  -> Result.T_Float
  Intermed.ST_Bool   -> Result.T_Bool
  Intermed.ST_Char   -> Result.T_Char
  Intermed.ST_Loc l  -> Result.T_Location [ml Map.! l]

locsToLocation
  :: Store Intermed.Loc (Set Intermed.Node)
  -> (Map.Map Intermed.Loc Int, Map.Map Int Result.NodeSet)
locsToLocation (Store m0) = (locToHeap, storeToHeap m0)
  where
    locToHeap = Map.fromList $ zip (Map.keys m0) [0..]

    storeToHeap :: Map.Map Intermed.Loc (Set Intermed.Node) -> Map.Map Int Result.NodeSet
    storeToHeap = Map.map (Set.foldl' (flip insertNode) mempty) . Map.mapKeys (locToHeap Map.!)

    insertNode :: Intermed.Node -> Result.NodeSet -> Result.NodeSet
    insertNode (Intermed.Node t ps) = flip Map.alter t $ \case
      Nothing  -> Just (stToSimpleType locToHeap <$> ps)
      Just ps0 -> Just $ zipWith
        (\p0 p1 -> fromMaybe (error $ show (p0,p1)) (unifySimpleType p0 p1))
        ps0
        (stToSimpleType locToHeap <$> ps)

applyIfBoth :: (Applicative f, Alternative f) => (a -> a -> a) -> f a -> f a -> f a
applyIfBoth f a b = (f <$> a <*> b) <|> a <|> b

unifyNodeSet :: Result.NodeSet -> Result.NodeSet -> Maybe Result.NodeSet
unifyNodeSet ns0 ns1 = sequence $ Map.unionWith unifyParams (Map.map Just ns0) (Map.map Just ns1)
  where
    unifyParams :: Maybe [Result.SimpleType] -> Maybe [Result.SimpleType] -> Maybe [Result.SimpleType]
    unifyParams ms0 ms1 = msum
      [ do s0 <- ms0
           s1 <- ms1
           zipWithM unifySimpleType s0 s1
      , ms0
      , ms1
      ]

unifySimpleType :: Result.SimpleType -> Result.SimpleType -> Maybe Result.SimpleType
unifySimpleType t1 t2 = case (t1,t2) of
  (Result.T_Location l1, Result.T_Location l2)  -> Just $ Result.T_Location (List.nub $ l1 ++ l2)
  (st1, st2) | st1 == st2         -> Just st2
             | otherwise          -> Nothing

unifyType :: Result.Type -> Result.Type -> Maybe Result.Type
unifyType t1 t2 = case (t1,t2) of
  (Result.T_NodeSet n1,     Result.T_NodeSet n2)     -> Result.T_NodeSet    <$> unifyNodeSet n1 n2
  (Result.T_SimpleType st1, Result.T_SimpleType st2) -> Result.T_SimpleType <$> unifySimpleType st1 st2
  _ -> Nothing

funcToFunctions
  :: Map.Map Intermed.Loc Int -> Map.Map Name Intermed.FunctionT
  -> Map.Map Name (Result.Type, [Result.Type])
funcToFunctions ml = Map.map
  (\(Intermed.FunctionT (r,ps))
    -> ( fromMaybe (error $ "funcToFunctions r " ++ show r) $ unifyTypes $ Set.map (tToType ml) r
       , fromMaybe (error $ "funcToFunctions p " ++ show ps) $ sequence $ fmap (unifyTypes . Set.map (tToType ml)) ps
       )
  )

varToVariable :: Map.Map Intermed.Loc Int -> Env (Set Intermed.T) -> Map.Map Name Result.Type
varToVariable ml (Env m) = Map.map (fromJust . unifyTypes . Set.map (tToType ml)) m

unifyTypes :: Set Result.Type -> Maybe Result.Type
unifyTypes = unifyTypes' . Set.toList where
  unifyTypes' []     = Nothing
  unifyTypes' (t:ts) = foldM unifyType t ts
