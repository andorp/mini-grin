{-# LANGUAGE FlexibleContexts #-}
module Data.Functor.FoldableM where

import Control.Monad ((<=<))
import Data.Functor.Foldable

{-
anaM :: (Monad m, Traversable (Base t), Corecursive t)
     => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
     a = (pure . embed) <=< traverse a <=< coalg
-}

-- | apoM is the monadic counterpart of the apomorphism.
-- Exercise: Read the definition of the ana and apo
-- http://hackage.haskell.org/package/recursion-schemes-5.1.3/docs/src/Data.Functor.Foldable.html#ana
-- http://hackage.haskell.org/package/recursion-schemes-5.1.3/docs/src/Data.Functor.Foldable.html#apo
--
-- In general apo is an extended anamorphism. Anamoprhism builds up an expression tree, using
-- a seed and a function that creates a new layer of the tree. Anamorphism builds the tree from
-- top to bottom.
--
-- The apomorphism, has the ability to generate a subtree in one go and stop the recursion there.
--
-- apoM is the Monadic variant, which can have some side effect meanwhile the generation of the
-- next layer happens.
apoM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = a where
  a = (pure . embed) <=< traverse f <=< coalg
  f = either pure a

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t ->  m a
cataM alg = c where
    c = alg <=< traverse c . project
