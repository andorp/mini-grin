{-# LANGUAGE DataKinds #-}
module Tutorial.Chapter03.Exercise02Spec where

import Test.Hspec hiding (before, after)
import Grin.GExp
import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter02.Exercise02 (typeInference)
import Tutorial.Chapter03.Exercise02


spec :: Spec
spec = do
  it "removes non necessary alternatives" $ do
    let beforeExp = convertGExpToExp before
    typeEnv <- typeInference beforeExp
    let result = sparseCaseOptimisation typeEnv beforeExp
    let expected = convertGExpToExp after
    result `shouldBe` expected

-- * Test data

before :: Exp 'Prg
before = Program
  []
  [Def "main" [] $
    Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
    Bind (Pure (Val (VNode (Node (Tag C "Index") ["m1"])))) (BVar "m2") $
    Case "m2"
      [ Alt (NodePat (Tag C "None") []) $
            Bind (Pure (Val (VPrim (SInt64 0)))) (BVar "m3") $
            Pure (Var "m3")
      , Alt (NodePat (Tag C "Index") ["m4"]) $
            Bind (Pure (Var "m4")) (BVar "m5") $
            Pure (Var "m5")
      , Alt DefaultPat $
            Bind (Pure (Var "m3")) (BVar "m6") $
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
      [ Alt (NodePat (Tag C "Index") ["m4"]) $
            Bind (Pure (Var "m4")) (BVar "m5") $
            Pure (Var "m5")
      ]
  ]
