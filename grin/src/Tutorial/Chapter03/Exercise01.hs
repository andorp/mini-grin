{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Tutorial.Chapter03.Exercise01 where

import Data.Functor.Foldable
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Grin.Exp
import Grin.Value

{-
Exercise:
Write a transformation which first collects the non-used parameters of functions.
And than removes the parameters from the definitions and function calls.

TODO: Explanation of the functor foldable library.
-}

data Info
  = Names        { used :: Set.Set Name }
  | NonUsedParam { nonUsedParams :: Map.Map Name [(Name, Int)] }

instance Semigroup Info where
  Names n1          <> Names n2         = Names (n1 <> n2)
  NonUsedParam nu1  <> NonUsedParam nu2 = NonUsedParam (nu1 <> nu2)

instance Monoid Info where
  mempty = Names mempty


collectNonUsedParams :: Exp -> Info
collectNonUsedParams = cata $ \case
  ProgramF _exts infos -> mconcat infos

  -- Exercise: Create a singleton map for the function name with the non-used parameters if there is any.
  DefF funName ps (Names used) -> NonUsedParam $ undefined ps used

  -- Exercise: Collect the names from the literal
  SPureF literal -> Names undefined

  SStoreF n       -> Names $ Set.singleton n
  SFetchF n       -> Names $ Set.singleton n
  SUpdateF n1 n2  -> Names $ Set.fromList [n1,n2]

  EBindF lhs BUnit rhs    -> lhs <> rhs
  EBindF lhs (BVar n) rhs -> mconcat [lhs, Names (Set.singleton n), rhs]

  -- Exercise: Collect all the used names
  EBindF lhs (BNodePat _tag names) rhs -> undefined

  ECaseF n alts -> mconcat $ Names (Set.singleton n) : alts
  AltF _ body   -> body


removeFunctionParams :: Info -> Exp -> Exp
removeFunctionParams (NonUsedParam funsToChange) = ana $ \case
  Def funName ps body -> case Map.lookup funName funsToChange of
    Nothing           -> DefF funName ps body
    -- Exercise: remove the parameters from the ps which are in the paramsToElim
    Just paramsToElim -> DefF funName (undefined ps) body

  SApp funName params -> case Map.lookup funName funsToChange of
    Nothing           -> SAppF funName params
    -- Exercise: remove the parameters from the function call that are in the paramsToElim
    --           use the Int index parameter
    Just paramsToElim -> SAppF funName (undefined params)

  e -> project e


transform :: Exp -> Exp
transform e =
  let info = collectNonUsedParams e
  in removeFunctionParams info e
