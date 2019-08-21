{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeFamilies #-}
module Tutorial.Chapter03.Exercise01 where

import Data.Functor.Foldable (cata, hylo, embed, project)
import Data.Functor.FoldableM
import Control.Monad.State.Strict

import Grin.Value (Name, VarOrValue(..), mkName)
import Grin.Exp
import qualified Data.Map as Map


-- Exercise:
-- Open the Data.Functor.Foldable library and read the Recursive, Corecursive typeclass, cata, ana function.
renameVars :: Name -> Name -> Int -> Exp -> Exp
renameVars ep arg i = cata $ \case
  -- Exercise: Undestand, how the BaseFunctor like EBindF plays a role in this expression.
  EBindF (lhs :: Exp) BUnit (rhs :: Exp) -> EBind lhs BUnit rhs

  EBindF lhs (BVar n)         rhs -> EBind lhs (BVar (new n)) rhs
  EBindF lhs (BNodePat t as)  rhs -> EBind lhs (BNodePat t (map new as)) rhs
  SPureF (Var n)            -> SPure (Var (new n))
  SPureF (Val l)            -> SPure (Val l)
  SStoreF n                 -> SStore (new n)
  SFetchF n                 -> SFetch (new n)
  SUpdateF n1 n2            -> SUpdate (new n1) (new n2)
  SAppF f as                -> SApp f (map new as)
  ECaseF n (alts :: [Exp])  -> ECase (new n) alts
  AltF DefaultPat body      -> Alt DefaultPat body
  AltF (LitPat l) body      -> Alt (LitPat l) body
  AltF (NodePat t as) body  -> Alt (NodePat t (map new as)) body
  BlockF body               -> Block body
  other -> error $ show other -- This function shouldn't be applied defs and above
  where
    -- Replace the original e1 argument with the actual call parameter in
    -- the call side.
    new n | n == ep   = arg
          | otherwise = n <> (mkName $ show i)


-- Exercise: Open the Data.Functor.FoldableM module and read the description of the apoM function.
inlineEval :: Exp -> Exp
inlineEval prog
  = bindNormalisation
  $ flip evalState (0 :: Int)
  $ flip apoM prog $ \case
      Program exts defs -> pure $ ProgramF exts $ map Right $ filter notEval defs

      -- Inline the body replacing the name of the variables with a given index.
      SApp "eval" [arg] -> BlockF <$> inlineBody arg

      -- Exercise: Find out which function to use from the Data.Functor.Foldable library to
      -- complete the definition
      other -> pure $ fmap Right $ undefined other
      where
        -- Find eval
        (Def "eval" [v] b) = (programToDefs prog) Map.! "eval"

        -- Exercise: Rewrite notEval to return True on the eval
        notEval _ = False

        inlineBody arg = do
          i <- get
          modify succ
          -- If the apoM gets a Left value it stops the recursion on that branch
          -- and just returns the computed value. In this case, we compute the
          -- inlined body of the eval.
          pure $ Left $ renameVars v arg i b

-- * Helper

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
