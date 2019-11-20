{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Grin.Analysis.CreatedBy where

import Grin.Value hiding (Node)
import Grin.Transformation.Base
import Grin.Datalog as D hiding (External, FunctionParameter)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)

import Grin.Exp                   as G
import Grin.Value                 as G
import Grin.TypeEnv.Intermediate  as TEI
import Grin.TypeEnv.Result        as TER
import Grin.TypeEnv.Convert       as TEC
import Grin.Interpreter.Store     as Store
import Grin.Interpreter.Env       as Env

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


data CreatedBy = CreatedBy

instance Analysis CreatedBy where
  type Fact   CreatedBy = CreatedByData
  type Result CreatedBy = CreatedByResult
  analysisName    _ = "CreatedBy"
  datalogProgram  _ = "created-by"
  outputFactFiles _ = [ "AbstractLocation"
                      , "VariableSimpleType"
                      , "VariableNodeTag"
                      , "VariableNodeParamType"
                      , "VariableAbstractLocation"
                      , "Heap"
                      , "FunParam"
                      , "FunReturn"
                      ]
  fromFacts       _ = CreatedByResult . typeEnvFromCreatedByData

data CreatedByData
  = Heap                      { location :: Name, target :: Name }
  | AbstractLocation          { variable :: Name }
  | VariableSimpleType        { variable :: Name, st :: D.SimpleType }
  | VariableNodeTag           { variable :: Name, t :: D.Tag }
  | VariableNodeParamType     { variable :: Name, t :: D.Tag , i :: Int, st :: D.SimpleType }
  | VariableAbstractLocation  { variable :: Name, loc :: Name }
  | FunParam         { function :: Name, i :: Int, parameter :: Name }
  | FunReturn        { function :: Name, variable :: Name }
--  | VarPointsTo               { variable :: Name, target :: Name }
-- TODO: External, ExternalParameter, FunctionParameter, FunctionReturn
  deriving (Show)

instance FromFact CreatedByData where
--  fromFact "VarPointsTo"              [v,t]       = Just $ VarPointsTo (fromString v) (fromString t)
  fromFact "Heap"                     [l,t]       = Just $ Heap (fromString l) (fromString t)
  fromFact "AbstractLocation"         [v]         = Just $ AbstractLocation (fromString v)
  fromFact "VariableSimpleType"       [v,t]       = Just $ VariableSimpleType (fromString v) (D.SimpleType t)
  fromFact "VariableAbstractLocation" [v,l]       = Just $ VariableAbstractLocation (fromString v) (fromString l)
  fromFact "VariableNodeTag"          [n,t]       = Just $ VariableNodeTag (fromString n) (D.Tag (fromString t))
  fromFact "VariableNodeParamType"    [n,t,i,st]  = Just $ VariableNodeParamType (fromString n) (D.Tag (fromString t)) (read i) (D.SimpleType st)
  fromFact "FunParam"                 [f,i,p]     = Just $ FunParam (fromString f) (read i) (fromString p)
  fromFact "FunReturn"                [f,r]       = Just $ FunReturn (fromString f) (fromString r)
  fromFact _ _                                    = Nothing

data CreatedByResult = CreatedByResult TER.TypeEnv
  deriving (Show)

valuesFromCreatedByData :: [CreatedByData] -> Map.Map Name (Set.Set TEI.T)
valuesFromCreatedByData xs = Map.unionsWith Set.union
  [ Map.fromList
      [ (n, Set.singleton (ST (ST_Loc (Loc l))))
      | VariableAbstractLocation n l <- xs
      ]
  , Map.fromList
      [ (v, Set.singleton (dToType t))
      | VariableSimpleType v t <- xs
      ]
  , createNodeVariables xs
  ]

heapValues :: Map.Map Name (Set.Set TEI.T) -> [CreatedByData] -> Map.Map TEI.Loc (Set.Set TEI.Node)
heapValues variables xs = Map.unionsWith Set.union
  [ Map.singleton (TEI.Loc l) v
  | Heap l v <- xs
  , v <- pure $ fromMaybe Set.empty $ (fmap . Set.map) (\(NT n) -> n) $ Map.lookup v variables
  ]

-- TODO: Uniqueness check!
typeEnvFromCreatedByData :: [CreatedByData] -> TER.TypeEnv
typeEnvFromCreatedByData xs = TEC.calcTypeEnv $ TEI.TypeEnv heap variable functions
  where
    variables = valuesFromCreatedByData xs
    heap      = Store.Store $ heapValues variables xs
    variable  = Env.Env variables
    functions = functionsFromCreatedByData variables xs

functionsFromCreatedByData :: Map.Map Name (Set.Set TEI.T) -> [CreatedByData] -> Map.Map Name TEI.FunctionT
functionsFromCreatedByData variables xs
  = Map.map (\(r, ps) -> FunctionT (r, parameters id ps))
  $ Map.unionsWith (\(r1, ps1) (r2, ps2) -> (r1 <> r2, Map.unionWith (<>) ps1 ps2))
      [ retOrParam
      | x <- xs
      , retOrParam <- case x of
          FunParam f i p -> [Map.singleton f (Set.empty, Map.singleton i $ fromMaybe (error ("Missing: " ++ show p)) $ Map.lookup p variables)]
          FunReturn f r  -> [Map.singleton f (fromMaybe (error ("Missing: " ++ show r)) $ Map.lookup r variables, Map.empty)]
          _ -> []
      ]


createNodeVariables :: [CreatedByData] -> Map.Map Name (Set.Set TEI.T)
createNodeVariables xs
  = (Map.map (Set.fromList . concatMap (uncurry toNodes) . Map.toList . Map.map nodeParams)) $ Map.unionsWith unionTags $
      [ Map.singleton v nMap
      | x <- xs
      , (v, nMap) <- case x of
          VariableNodeParamType v t i st ->
            [ (v, Map.singleton (dTagToTag t) $ Map.singleton i $ Set.singleton $ dToSimpleType st) ]
          VariableNodeTag v t ->
            [ (v, Map.singleton (dTagToTag t) $ Map.empty) ]
          _ -> []
      ]
  where
    unionTags
      :: Map.Map G.Tag (Map.Map Int (Set.Set TEI.ST))
      -> Map.Map G.Tag (Map.Map Int (Set.Set TEI.ST))
      -> Map.Map G.Tag (Map.Map Int (Set.Set TEI.ST))
    unionTags m1 m2 = Map.unionWith unionIndex m1 m2

    unionIndex
      :: Map.Map Int (Set.Set TEI.ST)
      -> Map.Map Int (Set.Set TEI.ST)
      -> Map.Map Int (Set.Set TEI.ST)
    unionIndex = Map.unionWith Set.union

    nodeParams :: Map.Map Int (Set.Set TEI.ST) -> [[TEI.ST]]
    nodeParams = parameters (checkNonRedundancy . Set.toList)

    checkNonRedundancy []  = error $ "Missing index."
    checkNonRedundancy [t] = [t]
    checkNonRedundancy ts
      | all (\case { TEI.ST_Loc _ -> True; _ -> False }) ts = ts
      | otherwise = error $ "Non-uniform simple types: " ++ show ts

    toNodes :: G.Tag -> [[TEI.ST]] -> [TEI.T]
    toNodes t []     = [NT $ TEI.Node t []]
    toNodes t params = map (NT . TEI.Node t) $ mixAndMatch params

parameters :: (Ord a) => (Set.Set a -> b) -> Map.Map Int (Set.Set a) -> [b]
parameters _               m | Map.null m = []
parameters checkParameters m = map
  (\i -> maybe (error ("Missing index: " ++ show i)) checkParameters $ Map.lookup i m)
  [ 0 .. maximum (Map.keys m) ]

-- mixAndMatch [] = [""]
-- mixAndMatch ["a"]        = ["a"]
-- mixAndMatch ["ab"]       = ["a","b"]
-- mixAndMatch ["ab", "c"]  = ["ac","bc"]
mixAndMatch :: [[a]] -> [[a]]
mixAndMatch []       = [ [] ]
mixAndMatch (xs:xss) = [ (y:ys) | y <- xs, ys <- mixAndMatch xss ]

dTagToTag :: D.Tag -> G.Tag
dTagToTag (D.Tag n) = G.Tag C n

dToSimpleType :: D.SimpleType -> TEI.ST
dToSimpleType d = case dToType d of
  TEI.UT    -> error $ "dToSimpleType unit"
  TEI.ST st -> st
  TEI.NT _  -> error $ "dToSimpleType: impossible"

dToType :: D.SimpleType -> TEI.T
dToType (D.SimpleType t) = case t of
  "Int64"   -> TEI.ST TEI.ST_Int64
  "Word64"  -> TEI.ST TEI.ST_Int64
  "Bool"    -> TEI.ST TEI.ST_Bool
  "Char"    -> TEI.ST TEI.ST_Char
  "Float"   -> TEI.ST TEI.ST_Float
  "Unit"    -> TEI.UT
  var       -> TEI.ST (TEI.ST_Loc (Loc $ fromString var))


-- * Some test cases

{-

test1 :: G.Exp
test1 = Program
  [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
  , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
  ]
  [ Def "main" [] $
      EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "m1") $
      EBind (SPure (Val (VPrim (SInt64 2)))) (BVar "m2") $
      EBind (SPure (Val (VNode (G.Node (G.Tag C "Cons") ["m1", "m2"])))) (BVar "m3") $
      EBind (SStore "m3") (BVar "m4") $
      EBind (SApp "upto" ["m4"]) (BVar "m5") $
      SPure (Var "m5")
  , Def "upto" ["u1"] $
      EBind (SFetch "u1") (BVar "u2") $
      EBind (ECase "u2" $
        [ G.Alt "u3" (NodePat (G.Tag C "Cons") ["u4", "u5"]) $
            EBind (SApp "prim_int_add" ["u4", "u5"]) (BVar "u6") $
            EBind (SApp "prim_int_print" ["u6"]) (BVar "u7") $
            SPure (Var "u7")
        ]) (BVar "u8") $
      SPure (Var "u8")
  ]

test2 :: G.Exp
test2 = Program
  [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
  , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
  ]
  [ Def "main" [] $
      EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "m1") $
      EBind (SPure (Val (VPrim (SInt64 2)))) (BVar "m2") $
      EBind (SPure (Val (VNode (G.Node (G.Tag C "Nil") [])))) (BVar "m3") $
      EBind (SStore "m3") (BVar "m4") $
      EBind (SApp "upto" ["m4"]) (BVar "m5") $
      SPure (Var "m5")
  , Def "upto" ["u1"] $
      EBind (SFetch "u1") (BVar "u2") $
      EBind (ECase "u2" $
        [ G.Alt "u3" (NodePat (G.Tag C "Nil") []) $
            EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "u4") $
            EBind (SPure (Val (VPrim (SInt64 2)))) (BVar "u5") $
            EBind (SApp "prim_int_add" ["u4", "u5"]) (BVar "u6") $
            EBind (SApp "prim_int_print" ["u6"]) (BVar "u7") $
            SPure (Var "u7")
        ]) (BVar "u8") $
      SPure (Var "u8")
  ]

-}
