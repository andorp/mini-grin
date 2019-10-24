module Tutorial.Chapter01.Exercise01Spec where

import Grin.Exp
import Grin.Value
import Grin.TypeEnv
import qualified Grin.Examples as Examples

import Tutorial.Chapter01.Exercise01

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GExp to Exp" $ do
    it "works for Add" $ do
      (convertGExpToExp Examples.add) `shouldBe` add

    it "works for Fact" $ do
      (convertGExpToExp Examples.fact) `shouldBe` fact

    it "works for Sum" $ do
      (convertGExpToExp Examples.sumSimple) `shouldBe` sumSimple

-- * Test data

add :: Program
add =
  Program
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    ]
    [ Def "add" ["s1", "s2"] $
        EBind (SApp "prim_int_add" ["s1", "s2"]) (BVar "s3") $
        SPure (Var "s3")
    , Def "main" [] $
        EBind (SPure (Val (VPrim(SInt64 10)))) (BVar "m1") $
        EBind (SPure (Val (VPrim(SInt64 20)))) (BVar "m2") $
        SApp "add" ["m1", "m2"]
    ]

fact :: Program
fact =
  Program
    [ External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_mul"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "fact" ["f1"] $
        EBind (SPure (Val (VPrim (SInt64 0)))) (BVar "f2") $
        EBind (SApp "prim_int_eq" ["f1", "f2"]) (BVar "f3") $
        ECase "f3"
          [ Alt (LitPat (SBool True)) $
                EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "f7") $
                SPure (Var "f7")
          , Alt (LitPat (SBool False)) $
                EBind (SPure (Val (VPrim(SInt64 1)))) (BVar "f4") $
                EBind (SApp "prim_int_sub" ["f1", "f4"]) (BVar "f5") $
                EBind (SApp "fact" ["f5"]) (BVar "f6") $
                SApp "prim_int_mul" ["f1", "f6"]
          ]
    , Def "main" [] $
        EBind (SPure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        EBind (SApp "fact" ["m1"]) (BVar "m2") $
        EBind (SApp "prim_int_print" ["m2"]) BUnit $
        SPure (Var "m2")
    ]

sumSimple :: Program
sumSimple =
  Program
    [ External "prim_int_add"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_sub"   (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_eq"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_gt"    (TySimple T_Bool)   [TySimple T_Int64, TySimple T_Int64] False
    , External "prim_int_print" (TySimple T_Unit)   [TySimple T_Int64, TySimple T_Int64] True
    ]
    [ Def "main" [] $
        EBind (SPure (Val (VPrim (SInt64 1)))) (BVar "m1") $
        EBind (SPure (Val (VPrim (SInt64 100)))) (BVar "m2") $
        EBind (SPure (Val (VNode (Node (Tag C "Int") ["m1"])))) (BVar "m3") $
        EBind (SPure (Val (VNode (Node (Tag C "Int") ["m2"])))) (BVar "m4") $
        EBind (SStore "m3") (BVar "m5") $
        EBind (SStore "m4") (BVar "m6") $
        EBind (SPure (Val (VNode (Node (Tag F "upto") ["m5", "m6"])))) (BVar "m7") $
        EBind (SStore "m7") (BVar "m8") $
        EBind (SPure (Val (VNode (Node (Tag F "sum") ["m8"])))) (BVar "m9") $
        EBind (SStore "m9") (BVar "m10") $
        EBind (SApp "eval" ["m10"]) (BNodePat "p1" (Tag C "Int") ["m11"]) $
        SApp "prim_int_print" ["m11"]
    , Def "upto" ["u1", "u2"] $
        EBind (SApp "eval" ["u1"]) (BNodePat "p2" (Tag C "Int") ["u3"]) $
        EBind (SApp "eval" ["u2"]) (BNodePat "p3" (Tag C "Int") ["u4"]) $
        EBind (SApp "prim_int_gt" ["u3", "u4"]) (BVar "u5") $
        ECase "u5"
          [ Alt (LitPat (SBool True)) $
                EBind (SPure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "u12") $
                SPure (Var "u12")
          , Alt (LitPat (SBool False)) $
                EBind (SPure (Val (VPrim(SInt64 1)))) (BVar "u6") $
                EBind (SApp "prim_int_add" ["u3", "u6"]) (BVar "u7") $
                EBind (SPure (Val (VNode (Node (Tag C "Int") ["u7"])))) (BVar "u8") $
                EBind (SStore "u8") (BVar "u9") $
                EBind (SPure (Val (VNode (Node (Tag F "upto") ["u9", "u2"])))) (BVar "u10") $
                EBind (SStore "u10") (BVar "u11") $
                SPure (Val (VNode (Node (Tag C "Cons") ["u1", "u11"])))
          ]
    , Def "sum" ["s1"] $
        EBind (SApp "eval" ["s1"]) (BVar "s2") $
        ECase "s2"
          [ Alt (NodePat (Tag C "Nil") []) $
                EBind (SPure (Val (VPrim(SInt64 0)))) (BVar "s3") $
                SPure (Val (VNode (Node (Tag C "Int") ["s3"])))
          , Alt (NodePat (Tag C "Cons") ["s5", "s6"]) $
                EBind (SApp "eval" ["s5"]) (BNodePat "p4" (Tag C "Int") ["s7"]) $
                EBind (SApp "sum" ["s6"]) (BNodePat "p5" (Tag C "Int") ["s8"]) $
                EBind (SApp "prim_int_add" ["s7", "s8"]) (BVar "s9") $
                SPure (Val (VNode (Node (Tag C "Int") ["s9"])))
          ]
    , Def "eval" ["e1"] $
        EBind (SFetch "e1") (BVar "e2") $
        ECase "e2"
          [ Alt (NodePat (Tag C "Int") ["e3"]) $
                EBind (SPure (Val (VNode (Node (Tag C "Int") ["e3"])))) (BVar "e11") $
                SPure (Var "e11")
          , Alt (NodePat (Tag C "Nil") []) $
                EBind (SPure (Val (VNode (Node (Tag C "Nil") [])))) (BVar "e12") $
                SPure (Var "e12")
          , Alt (NodePat (Tag C "Cons") ["e4", "e5"]) $
                EBind (SPure (Val (VNode (Node (Tag C "Cons") ["e4", "e5"])))) (BVar "e13") $
                SPure (Var "e13")
          , Alt (NodePat (Tag F "upto") ["e6", "e7"]) $
                EBind (SApp "upto" ["e6", "e7"]) (BVar "e8") $
                EBind (SUpdate "e1" "e8") BUnit $
                SPure (Var "e8")
          , Alt (NodePat (Tag F "sum") ["e9"]) $
                EBind (SApp "sum" ["e9"]) (BVar "e10") $
                EBind (SUpdate "e1" "e10") BUnit $
                SPure (Var "e10")
          ]
    ]
