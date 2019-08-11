{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, LambdaCase, ConstraintKinds #-}
module Grin.GExp where

import Data.Kind (Constraint)
import Grin.Exp hiding (Exp(..))
import Grin.Value

-- TODO: Explanation
data ExpCtx
  = SECtx   -- Simple Expressions
  | ECtx    -- Expressions
  | CaseCtx -- Case Expression
  | AltCtx  -- Alternative of a case
  | DefCtx  -- Function definitions
  | PrgCtx  -- Program definition

data Exp (ctx :: ExpCtx) where
  Program
    :: [External]
    -> [Exp 'DefCtx]  -- ^ definitions
    -> Exp 'PrgCtx
  Def
    :: Name
    -> [Name]  -- ^ arguments
    -> Exp 'ECtx
    -> Exp 'DefCtx
  SApp
    :: Name
    -> [Name]  -- ^ arguments
    -> Exp 'SECtx
  SPure
    :: Val
    -> Exp 'SECtx
  SStore
    :: Name
    -> Exp 'SECtx
  SFetch
    :: Name
    -> Exp 'SECtx
  SUpdate
    :: Name  -- ^ reference to update
    -> Name  -- ^ new value
    -> Exp 'SECtx
  Alt
    :: CPat
    -> Exp 'ECtx  -- ^ continuation
    -> Exp 'AltCtx
  ECase
    :: Name  -- ^ scrutinee
    -> [Exp 'AltCtx]  -- ^ possible cases
    -> Exp 'CaseCtx
  -- |
  -- > Ebind lhs bpat rhs
  -- corresponds to
  -- > lhs >>= \bpat -> rhs
  EBind
    :: (Elem lhs ['SECtx, 'CaseCtx], Elem rhs ['SECtx, 'CaseCtx, 'ECtx])
    => Exp lhs
    -> BPat
    -> Exp rhs
    -> Exp 'ECtx

type family Elem (c :: ExpCtx) (cs :: [ExpCtx]) :: Constraint where
  Elem c (c : _)  = ()
  Elem c (d : cs) = Elem c cs
