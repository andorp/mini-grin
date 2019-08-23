{-# LANGUAGE LambdaCase #-}
module Tutorial.Chapter03.Exercise02 where

import Grin.Exp
import Grin.TypeEnv
import Data.Functor.Foldable
import Lens.Micro ((^.))
import Grin.Interpreter.Abstract.Base hiding (TypeEnv)
import Grin.Interpreter.Abstract.TypeInference (typeOfValue)
import Data.Maybe (mapMaybe)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map hiding (filter)

{-
Sparse case optimisation is a very powerful optimisation, which removes the
unnecessary case alternatives.

Exercise:
Read the Chapter 4.3.6
https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=143
-}

-- | Sparse case optimisation uses ana to transform the program from top to down.
-- It checks if the given alterntive can be removed. The alternative can be removed
-- if it does not match any of the Nodes in the type associated with the variable.
-- Literal matching alternatives must be kept.
sparseCaseOptimisation :: TypeEnv -> Exp -> Exp
sparseCaseOptimisation te = ana $ \case
  ECase n alts ->
    let ty = (te ^. variable) Map.! n
    in ECaseF n $
        filter (\(Alt cpat _) -> matchingAlt ty cpat) $ removeTheRedundantDefault ty alts
  -- Exercise: Use the function from the Data.Functor.Foldable library
  other -> undefined other


-- | Returns True if the given pattern can be matched with the type of the scrutinee
matchingAlt :: Type -> CPat -> Bool
matchingAlt _                 DefaultPat      = True
matchingAlt (T_SimpleType _)  (NodePat{})     = False
matchingAlt (T_NodeSet{})     (LitPat{})      = False
-- Exercise: The type of the literal should match the simple type
matchingAlt (T_SimpleType st) (LitPat l)      = undefined
-- Exercise: The tag from the pattern should be present in the NodeSet.
matchingAlt (T_NodeSet ns)    (NodePat t ps)  = undefined

-- | Remove the redundant detault
removeTheRedundantDefault :: Type -> [Alt] -> [Alt]
-- As we erase the actual value of the primitive, we can not be sure, of the
-- Default pattern is redunant, thus it must be kept.
removeTheRedundantDefault (T_SimpleType{}) alts = alts

-- Exercise: If every element from the nodeset is covered by the alts
-- and there is a DefaultPat, it can be removed as it redundant
-- at it will never be accessed.
removeTheRedundantDefault (T_NodeSet ns) alts = undefined
