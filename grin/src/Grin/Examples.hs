module Grin.Examples where

import Grin.Exp

-- * Test expression

add =
  Program
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False PrimOp
    ]
    [ Def "add" ["s1", "s2"] (SApp "prim_int_add" ["s1", "s2"])
    , Def "main" [] $
        EBind (SPure (Lit (LInt64 10))) (Var "m1") $
        EBind (SPure (Lit (LInt64 20))) (Var "m2") $
        SApp "add" ["m1", "m2"]
    ]

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
                SPure (Lit (LInt64 1))
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

sumSimple =
  Program
    [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_gt"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False PrimOp
    , External "prim_int_print" (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] True  PrimOp
    ]
    [ Def "main" [] $
        EBind (SPure (Lit (LInt64 1))) (Var "m1") $
        EBind (SPure (Lit (LInt64 100))) (Var "m2") $
        EBind (SPure (ConstTagNode (Tag C "Int") ["m1"])) (Var "m3") $
        EBind (SPure (ConstTagNode (Tag C "Int") ["m2"])) (Var "m4") $
        EBind (SStore "m3") (Var "m5") $
        EBind (SStore "m4") (Var "m6") $
        EBind (SPure (ConstTagNode (Tag F "upto") ["m5", "m6"])) (Var "m7") $
        EBind (SStore "m7") (Var "m8") $
        EBind (SPure (ConstTagNode (Tag F "sum") ["m8"])) (Var "m9") $
        EBind (SStore "m9") (Var "m10") $
        EBind (SApp "eval" ["m10"]) (ConstTagNode (Tag C "Int") ["m11"]) $
        SApp "prim_int_print" ["m11"]
    , Def "upto" ["u1", "u2"] $
        EBind (SApp "eval" ["u1"]) (ConstTagNode (Tag C "Int") ["u3"]) $
        EBind (SApp "eval" ["u2"]) (ConstTagNode (Tag C "Int") ["u4"]) $
        EBind (SApp "prim_int_gt" ["u3", "u4"]) (Var "u5") $
        ECase "u5"
          [ Alt (LitPat (LBool True)) $
                SPure (ConstTagNode (Tag C "Nil") [])
          , Alt (LitPat (LBool False)) $
                EBind (SPure (Lit (LInt64 1))) (Var "u6") $
                EBind (SApp "prim_int_add" ["u3", "u6"]) (Var "u7") $
                EBind (SPure (ConstTagNode (Tag C "Int") ["u7"])) (Var "u8") $
                EBind (SStore "u8") (Var "u9") $
                EBind (SPure (ConstTagNode (Tag F "upto") ["u9", "u2"])) (Var "u10") $
                EBind (SStore "u10") (Var "u11") $
                SPure (ConstTagNode (Tag C "Cons") ["u1", "u11"])
          ]
    , Def "sum" ["s1"] $
        EBind (SApp "eval" ["s1"]) (Var "s2") $
        ECase "s2"
          [ Alt (NodePat (Tag C "Nil") []) $
                EBind (SPure (Lit (LInt64 0))) (Var "s3") $
                SPure (ConstTagNode (Tag C "Int") ["s3"])
          , Alt (NodePat (Tag C "Cons") ["s5", "s6"]) $
                EBind (SApp "eval" ["s5"]) (ConstTagNode (Tag C "Int") ["s7"]) $
                EBind (SApp "sum" ["s6"]) (ConstTagNode (Tag C "Int") ["s8"]) $
                EBind (SApp "prim_int_add" ["s7", "s8"]) (Var "s9") $
                SPure (ConstTagNode (Tag C "Int") ["s9"])
          ]
    , Def "eval" ["e1"] $
        EBind (SFetch "e1") (Var "e2") $
        ECase "e2"
          [ Alt (NodePat (Tag C "Int") ["e3"]) $
                SPure (ConstTagNode (Tag C "Int") ["e3"])
          , Alt (NodePat (Tag C "Nil") []) $
                SPure (ConstTagNode (Tag C "Nil") [])
          , Alt (NodePat (Tag C "Cons") ["e4", "e5"]) $
                SPure (ConstTagNode (Tag C "Cons") ["e4", "e5"])
          , Alt (NodePat (Tag F "upto") ["e6", "e7"]) $
                EBind (SApp "upto" ["e6", "e7"]) (Var "e8") $
                EBind (SUpdate "e1" "e8") Unit $
                SPure (Var "e8")
          , Alt (NodePat (Tag F "sum") ["e9"]) $
                EBind (SApp "sum" ["e9"]) (Var "e10") $
                EBind (SUpdate "e1" "e10") Unit $
                SPure (Var "e10")
          ]
    ]
