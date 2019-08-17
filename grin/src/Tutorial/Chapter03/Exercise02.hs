{-# LANGUAGE LambdaCase #-}
module Tutorial.Chapter03.Exercise02 where

import Grin.Exp
import Grin.TypeEnv
import Data.Functor.Foldable
import Data.Map.Strict as Map hiding (filter)
import Lens.Micro ((^.))


sparseCaseOptimisation :: TypeEnv -> Exp -> Exp
sparseCaseOptimisation te = ana $ \case
  ECase n alts ->
    let ty = (te ^. variable) Map.! n
    in ECaseF n $
        removeTheRedundantDefault ty $
        filter (\(Alt cpat _) -> matchingAlt ty cpat) alts
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
removeTheRedundantDefault (T_SimpleType{}) alts = alts
-- Exercise: If every element from the nodeset is covered by the alts
-- and there is a DefaultPat, it can be removed as it redundant
-- at it will never be accessed.
removeTheRedundantDefault (T_NodeSet ns) alts = undefined
