{-# LANGUAGE DataKinds #-}
module Tutorial.Chapter03.Exercise01Spec where

import Tutorial.Chapter01.Exercise02 (interpreter, knownExternals)
import Tutorial.Chapter03.Exercise01
import Test.Hspec
import Grin.Exp (programToDefs)
import Grin.GExp
import Grin.GExpToExp
import qualified Data.Map as Map
-- import Grin.Pretty (PP(..))


spec :: Spec
spec = do
  it "Inlive eval works for a simple function" $ do
    let ex = gexpToExp evalExp
--    print $ PP ex
    let expInlinedEval = inlineEval ex
--    print $ PP expInlinedEval
    resBefore <- interpreter knownExternals ex
    resAfter  <- interpreter knownExternals expInlinedEval
    let defs = programToDefs expInlinedEval
    defs `shouldNotSatisfy` (Map.member "eval")
    resBefore `shouldBe` resAfter

evalExp :: Exp 'Prg
evalExp =
  Program
    [ External "prim_int_add" (TySimple T_Int64)  [TySimple T_Int64, TySimple T_Int64] False
    ]
    [ Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 10)))) (BVar "m1") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m1"])))) (BVar "m2") $
        Bind (Store "m2") (BVar "m3") $
        Bind (Pure (Val (VPrim (SInt64 20)))) (BVar "m4") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["m4"])))) (BVar "m5") $
        Bind (Store "m5") (BVar "m6") $
        Bind (Pure (Val (VNode (Node (Tag F "add") ["m3", "m6"])))) (BVar "m7") $
        Bind (Store "m7") (BVar "m8") $
        Bind (App ("eval") ["m8"]) (BVar "m9") $
        Pure (Var "m9")

    , Def "add" ["a1", "a2"] $
        Bind (App "eval" ["a1"]) (BNodePat "p1" (Tag C "Int") ["a3"]) $
        Bind (App "eval" ["a2"]) (BNodePat "p2" (Tag C "Int") ["a4"]) $
        Bind (App "prim_int_add" ["a3", "a4"]) (BVar "a5") $
        Bind (Pure (Val (VNode (Node (Tag C "Int") ["a5"])))) (BVar "a6") $
        Pure (Var "a6")

    , Def "eval" ["e1"] $
        Bind (Fetch "e1") (BVar "e2") $
        Case "e2"
          [ Alt (NodePat (Tag C "Int") ["e3"]) $
                Bind (Pure (Val (VNode (Node (Tag C "Int") ["e3"])))) (BVar "e4") $
                Pure (Var "e4")
          , Alt (NodePat (Tag F "add") ["e5", "e6"]) $
                Bind (App "add" ["e5", "e6"]) (BVar "e7") $
                Bind (Update "e1" "e7") BUnit $
                Pure (Var "e7")
          ]
    ]
