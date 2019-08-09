{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, LambdaCase, ConstraintKinds #-}
module Grin.GExp where

import Data.Kind (Constraint)
import Grin.Exp hiding (Exp(..))
import qualified Grin.Exp as Grin


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
          => Exp lhs -> Val -> Exp rhs -> Exp 'ECtx

-- Chapter 1: Write a conversion from the Type-Safe Expression
-- to the overgenerative one
toNonSafeExp :: forall ctx . Exp ctx -> Grin.Exp
toNonSafeExp = \case
  Program exts defs -> Grin.Program exts (toNonSafeExp <$> defs)
  Def     n ps body -> Grin.Def n ps $ toNonSafeExp body
  SApp    n ps      -> Grin.SApp n ps
  SPure   v         -> Grin.SPure v
  SStore  n         -> Grin.SStore n
  SFetch  n         -> Grin.SFetch n
  SUpdate n v       -> Grin.SUpdate n v
  Alt     c body    -> Grin.Alt c $ toNonSafeExp body
  ECase   n alts    -> Grin.ECase n (toNonSafeExp <$> alts)
  EBind   lhs v rhs -> Grin.EBind (toNonSafeExp lhs) v (toNonSafeExp rhs)

type family Elem (c :: ExpCtx) (cs :: [ExpCtx]) :: Constraint where
  Elem c (c : _)  = ()
  Elem c (d : cs) = Elem c cs

fact :: Exp 'PrgCtx
fact =
  Program
    [ External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_mul"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_print" (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] True  PrimOp
    ]
    [ Def "fact" ["f1"] $
        EBind (SPure (Lit (LInt64 0))) (Var "f2") $
        EBind (SApp "prim_int_eq" ["f1", "f2"]) (Var "f3") $
        ECase "f3"
          [ Alt (LitPat (LBool True)) $
                EBind (SPure (Lit (LInt64 1))) (Var "f7") $
                SPure (Var "f7")
          , Alt (LitPat (LBool False)) $
                EBind (SPure (Lit (LInt64 1))) (Var "f4") $
                EBind (SApp "prim_int_sub" ["f1", "f4"]) (Var "f5") $
                EBind (SApp "fact" ["f5"]) (Var "f6") $
                SApp "prim_int_mul" ["f1", "f6"]
          ]
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SApp "fact" ["m1"]) (Var "m2") $
        SApp "prim_int_print" ["m2"]
    ]
