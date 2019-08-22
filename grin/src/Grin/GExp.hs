{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, LambdaCase, ConstraintKinds, UndecidableInstances #-}
module Grin.GExp
  ( module Grin.GExp
  , module Grin.Exp
  , module Grin.Value
  , module Grin.TypeEnv
  ) where

import Data.Kind (Constraint)
import Grin.Exp (BPat(..), CPat(..), External(..))
import Grin.Value
import Grin.TypeEnv
import GHC.TypeLits


data ExpCtx
  = Simple    -- Simple Expressions
  | Bind_     -- Expressions
  | Case_     -- Case Expression
  | Alt_      -- Alternative of a case
  | Def_      -- Function definitions
  | Prg       -- Program definition

data Exp (ctx :: ExpCtx) where

  Program
    :: [External]
    -> [Exp 'Def_]  -- ^ definitions
    -> Exp 'Prg

  Def
    :: Name
    -> [Name]  -- ^ arguments
    -> Exp 'Bind_
    -> Exp 'Def_

  App
    :: Name
    -> [Name]  -- ^ arguments
    -> Exp 'Simple

  Pure
    :: VarOrValue
    -> Exp 'Simple

  Store
    :: Name
    -> Exp 'Simple

  Fetch
    :: Name
    -> Exp 'Simple

  Update
    :: Name  -- ^ reference to update
    -> Name  -- ^ new value
    -> Exp 'Simple

  Alt
    :: CPat
    -> Exp 'Bind_  -- ^ continuation
    -> Exp 'Alt_

  Case
    :: Name         -- ^ scrutinee
    -> [Exp 'Alt_]  -- ^ possible alternatives of a case
    -> Exp 'Case_

  -- |
  -- > Ebind lhs bpat rhs
  -- corresponds to
  -- > lhs >>= \bpat -> rhs
  Bind
    :: (IsExp lhs ['Simple, 'Case_], IsExp rhs ['Simple, 'Case_, 'Bind_])
    => Exp lhs
    -> BPat
    -> Exp rhs
    -> Exp 'Bind_


type IsExp c cs = Elem c cs cs

type family Elem (c :: ExpCtx) (xs :: [ExpCtx]) (cs :: [ExpCtx]) :: Constraint where
  Elem c xs (c : _)  = ()
  Elem c xs (d : cs) = Elem c xs cs
  Elem c xs '[]      = TypeError ('Text "Expected expression type " ':<>: 'ShowType xs
                                  ':$$:
                                  'Text "but got " ':<>: 'ShowType c)
