{-# LANGUAGE DataKinds #-}
module Tutorial.Chapter03.Exercise01Spec where

import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter03.Exercise01
import Grin.GExp
import Test.Hspec


spec :: Spec
spec = do
  it "Removes the unused parameter" $ do
    let prog = convertGExpToExp simpleProg
    let expected = convertGExpToExp exptectedProg
    let result = transform prog
    result `shouldBe` expected

simpleProg :: Exp 'Prg
simpleProg =
  Program
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    ]
    [ Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
        Bind (Pure (Val (VPrim (SInt64 2)))) (BVar "m2") $
        App "something" ["m1", "m2"]
    , Def "something" ["s1", "s2"] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "s3") $
        App "prim_int_add" ["s1", "s3"]
    ]

exptectedProg :: Exp 'Prg
exptectedProg =
  Program
    [ External "prim_int_add" (TySimple T_Int64) [TySimple T_Int64, TySimple T_Int64] False
    ]
    [ Def "main" [] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "m1") $
        Bind (Pure (Val (VPrim (SInt64 2)))) (BVar "m2") $
        App "something" ["m1"]
    , Def "something" ["s1"] $
        Bind (Pure (Val (VPrim (SInt64 1)))) (BVar "s3") $
        App "prim_int_add" ["s1", "s3"]
    ]
