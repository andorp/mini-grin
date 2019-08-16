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
simpleProg = undefined

exptectedProg :: Exp 'Prg
exptectedProg = undefined

