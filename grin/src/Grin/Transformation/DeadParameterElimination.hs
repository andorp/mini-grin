{-# LANGUAGE DeriveGeneric, TypeFamilies #-}
module Grin.Transformation.DeadParameterElimination where

import GHC.Generics
import Grin.Datalog
import Grin.Transformation.Base


data DeadParameterElimination = DeadParameterElimination
  deriving Show

instance Analysis DeadParameterElimination where
  type Fact   DeadParameterElimination = DeadParameter
  type Result DeadParameterElimination = DeadParameters
  analysisName      = show
  datalogProgram _  = "dead-code"
  outputFactFiles _ = ["DeadParameter"]
  fromFacts _ _     = DeadParameters

-- TODO
instance Transformation DeadParameterElimination where
  transform _ _ _ = Nothing

data DeadParameter = DeadParameter { deadVariable :: Variable }
  deriving (Eq, Show, Ord, Generic)

data DeadParameters = DeadParameters
