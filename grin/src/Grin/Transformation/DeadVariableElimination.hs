{-# LANGUAGE LambdaCase, ViewPatterns, TypeFamilies, DeriveGeneric #-}
module Grin.Transformation.DeadVariableElimination where

import Grin.Exp
import Grin.Value
import Grin.TypeEnv.Result
import Data.Set
import Data.Functor.Foldable
import Grin.Datalog hiding (External)
import Grin.Transformation.Base
import GHC.Generics
import Data.String (fromString)


data DeadCodeAnalysis = DeadCodeAnalysis
  deriving Show

instance Analysis DeadCodeAnalysis where
  type Fact   DeadCodeAnalysis = DeadVariable
  type Result DeadCodeAnalysis = DeadVariables
  analysisName        = show
  datalogProgram _    = "dead-code"
  outputFactFiles _   = ["DeadVariable"]
  fromFacts _ ds      = DeadVariables
                          $ Data.Set.fromList
                          $ fmap (\(DeadVariable (Variable n)) -> n) ds

data DeadVariableElimination = DeadVariableElimination

instance Transformation DeadVariableElimination where
  type AnalysisOf DeadVariableElimination = DeadCodeAnalysis
  analysisOf _ = DeadCodeAnalysis
  transform _ = deadVariableElimination

data DeadVariable = DeadVariable { deadVariable :: Variable }
  deriving (Eq, Show, Ord, Generic)

instance FromFact DeadVariable where
  fromFact "DeadVariable"  [name] = Just $ DeadVariable $ Variable $ fromString name
  fromFact _               _      = Nothing

-- Set of non effect non-used variables
newtype DeadVariables = DeadVariables (Set Name)
  deriving (Show)

deadVariableElimination :: DeadVariables -> Exp -> Maybe (Exp, [ExpChanged])
deadVariableElimination (DeadVariables vars) e
  | Data.Set.null vars  = Nothing
  | otherwise           = Just (changeExp vars e, [VarRemoved])

changeExp :: Set Name -> Exp -> Exp
changeExp vars = cata folder
  where
    folder :: ExpF Exp -> Exp
    folder = \case
      bind@(EBindF _lhs (bpatVar -> n) rhs)
        | Data.Set.member n vars -> rhs
        | otherwise              -> embed bind
      rest -> embed rest

deadVariableTestExp :: Exp
deadVariableTestExp =
  Program
  [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
  , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
  ]
  [ Def "main" [] $
      EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "x") $
      EBind (SPure (Val (VPrim (SInt64 2)))) (BVar "y") $
      EBind (SApp "prim_int_add" ["x", "y"]) (BVar "z") $
      EBind (SApp "prim_int_add" ["x", "y"]) (BVar "w") $
      EBind (SApp "prim_int_print" ["w"]) (BVar "r") $
      SPure (Var "x")
  ]
