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
  Program :: [External] -> [Exp 'DefCtx]              -> Exp 'PrgCtx
  Def     :: Name       -> [Name]        -> Exp 'ECtx -> Exp 'DefCtx
  SApp    :: Name       -> [Name]                     -> Exp 'SECtx
  SPure   :: Val                                      -> Exp 'SECtx
  SStore  :: Name                                     -> Exp 'SECtx
  SFetch  :: Name                                     -> Exp 'SECtx
  SUpdate :: Name       -> Name                       -> Exp 'SECtx
  Alt     :: CPat       -> Exp 'ECtx                  -> Exp 'AltCtx
  ECase   :: Name       -> [Exp 'AltCtx]              -> Exp 'CaseCtx
  EBind   :: (Elem lhs ['SECtx, 'CaseCtx], Elem rhs ['SECtx, 'CaseCtx, 'ECtx])
          => Exp lhs -> BPat -> Exp rhs -> Exp 'ECtx

type family Elem (c :: ExpCtx) (cs :: [ExpCtx]) :: Constraint where
  Elem c (c : _)  = ()
  Elem c (d : cs) = Elem c cs
