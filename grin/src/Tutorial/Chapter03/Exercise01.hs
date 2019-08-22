{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeFamilies #-}
module Tutorial.Chapter03.Exercise01 where

import Data.Functor.Foldable (cata, hylo, embed, project)
import Data.Functor.FoldableM
import Control.Monad.State.Strict

import Grin.Value (Name, VarOrValue(..), Value(..), Node(..), mkName)
import Grin.Exp
import qualified Data.Map as Map


-- Exercise:
-- Read the Chapter 4.2.1 about this transformation:
-- https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=100

-- Exercise:
-- Open the Data.Functor.Foldable library and read the Recursive, Corecursive typeclass, cata, ana function.
-- http://hackage.haskell.org/package/recursion-schemes-5.1.3/docs/src/Data.Functor.Foldable.html#Recursive
-- http://hackage.haskell.org/package/recursion-schemes-5.1.3/docs/src/Data.Functor.Foldable.html#Corecursive

-- Hint:
-- The Data.Functor.Foldable library creates the Base Functor data type with template Haskell.
-- Turning every constuctor to a Functor, based on the recursive instance of the type in the constructor.
-- E.g: ((Def Name [Name] Exp) :: Exp) will be turned into ((DefF Name [Name] r) :: ExpF r)

-- Rename Vars uses a the cata from the Recursive type class, where the information we build up from
-- the buttom to top is the Exp AST itself, instead of another type. This way we can write program
-- transformations easily, without applying the recursion boilerplate, that could be errorprone.
renameVars :: Name -> Name -> Int -> Exp -> Exp
renameVars ep arg i = cata $ \case
  -- Exercise: Undestand, how the BaseFunctor like EBindF plays a role in this expression.
  EBindF (lhs :: Exp) BUnit (rhs :: Exp) -> EBind lhs BUnit rhs

  -- Exercise: Read all the constructors above
  EBindF lhs (BVar n)         rhs -> EBind lhs (BVar (new n)) rhs
  EBindF lhs (BNodePat t as)  rhs -> EBind lhs (BNodePat t (map new as)) rhs

  SPureF (Var n)                    -> SPure (Var (new n))
  SPureF (Val (VPrim vp))           -> SPure (Val (VPrim vp))
  SPureF (Val (VNode (Node t ns)))  -> SPure (Val (VNode (Node t (map new ns))))

  SStoreF n                 -> SStore (new n)
  SFetchF n                 -> SFetch (new n)
  SUpdateF n1 n2            -> SUpdate (new n1) (new n2)
  SAppF f as                -> SApp f (map new as)
  -- Hint: Alts are already computed in the previous steps of the recursion
  -- Same applies to the rest of the instances where we have a 'body'
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
-- FoldableM is part of this repository.
--
-- InlineEval uses an integer to index to generate new names during the inline-ing.
inlineEval :: Exp -> Exp
inlineEval prog
  = bindNormalisation         -- This is a helper which removes the inserted blocks.
  $ flip evalState (0 :: Int) -- 0 eval inlined so far.
  $ flip apoM prog $ \case
      -- Filter out the eval function from the final result.
      -- NOTE: Don't forget, we now build the tree from top to botton, thus
      -- the inlining mechanist won't run on the eval, as eval wont be part of
      -- the definition in the next recursive step.
      Program exts defs -> pure $ ProgramF exts $ map Right $ filter notEval defs

      -- As we use recursion schemes, we need to tackle down the interesting
      -- constructors. Which are the application of the eval function, in some
      -- expression.

      -- Inline the body replacing the name of the variables with a given index.
      SApp "eval" [arg] -> BlockF <$> inlineBody arg

      -- Exercise: Find out which function to use from the Data.Functor.Foldable library to
      other -> pure $ fmap Right $ undefined other
      -- complete the definition. Why?
      where
        -- Find eval
        (Def "eval" [v] b) = (programToDefs prog) Map.! "eval"

        notEval _ = False
        -- Exercise: Rewrite notEval to return False on the eval

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
