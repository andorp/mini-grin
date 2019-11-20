{-# LANGUAGE TypeFamilies #-}
module Grin.Transformation.Base where

import Grin.Exp
import Grin.Datalog hiding (Fact)
import Data.Functor.Foldable

data ExpChanged
  = NewBlock
  | VarIntroduced
  | VarRemoved
  deriving (Eq, Show)

class Analysis t where
  type Fact   t :: *
  type Result t :: *
  analysisName    :: t -> String
  datalogProgram  :: t -> String
  outputFactFiles :: t -> [String]
  fromFacts       :: t -> [Fact t] -> Result t

class Transformation t where
  type AnalysisOf t :: *
  analysisOf  :: t -> AnalysisOf t
  transform   :: t -> Result (AnalysisOf t) -> Exp -> Maybe (Exp, [ExpChanged])

bindNormalisation :: Exp -> Exp
bindNormalisation = hylo alg coalg where
  alg :: ExpF Exp -> Exp
  alg (BlockF e) = e
  alg e          = embed e

  coalg :: Exp -> ExpF Exp
  coalg (EBind lhs1 pat1 rhs1)
    | EBind lhs2 pat2 rhs2 <- rmBlocks lhs1
    = BlockF $ EBind lhs2 pat2 (EBind (Block rhs2) pat1 rhs1)
  coalg e = project e

  rmBlocks :: Exp -> Exp
  rmBlocks (Block e) = rmBlocks e
  rmBlocks e          = e

