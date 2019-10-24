{-# LANGUAGE DataKinds #-}
module Tutorial.Chapter03.Exercise02Spec where

import Test.Hspec hiding (before, after)
import Grin.GExp
import Grin.GExpToExp
import Grin.Pretty (PP(..))
import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter01.Exercise02 (interpreter, knownExternals)
import Tutorial.Chapter02.Exercise02 (typeInference)
import Tutorial.Chapter03.Exercise01 (inlineEval)
import Tutorial.Chapter03.Exercise02

import Tutorial.Chapter03.Exercise01Spec (evalExp)


spec :: Spec
spec = do
  it "removes non necessary alternatives" $ do
    let beforeExp = convertGExpToExp before
    print $ PP beforeExp
    typeEnv <- typeInference beforeExp
    print $ PP typeEnv
    let result = sparseCaseOptimisation typeEnv beforeExp
    let expected = convertGExpToExp after
    print $ PP result
    result `shouldBe` expected

  it "removes the non-neccary cases of inlined evals" $ do
    let ex = gexpToExp evalExp
    resBefore <- interpreter knownExternals ex

    let expInlinedEval = inlineEval ex
    print $ PP expInlinedEval
    typeEnv   <- typeInference expInlinedEval
    print $ PP typeEnv
    let expSparseCase = sparseCaseOptimisation typeEnv expInlinedEval
    print $ PP expSparseCase

    resAfter  <- interpreter knownExternals expSparseCase
    resBefore `shouldBe` resAfter


-- * Test data

before :: Exp 'Prg
before = Program
  []
  [Def "main" [] $
    Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
    Bind (Pure (Val (VNode (Node (Tag C "Index") ["m1"])))) (BVar "m2") $
    Case "m2"
      [ Alt "alt1" (NodePat (Tag C "None") []) $
            Bind (Pure (Val (VPrim (SInt64 0)))) (BVar "m3") $
            Pure (Var "m3")
      , Alt "alt2" (NodePat (Tag C "Index") ["m4"]) $
            Bind (Pure (Var "m4")) (BVar "m5") $
            Pure (Var "m5")
      , Alt "alt3" DefaultPat $
            Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m6") $
            Pure (Var  "m6")
      ]
  ]

after :: Exp 'Prg
after =  Program
  []
  [Def "main" [] $
    Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
    Bind (Pure (Val (VNode (Node (Tag C "Index") ["m1"])))) (BVar "m2") $
    Case "m2"
      [ Alt "alt2" (NodePat (Tag C "Index") ["m4"]) $
            Bind (Pure (Var "m4")) (BVar "m5") $
            Pure (Var "m5")
      ]
  ]
