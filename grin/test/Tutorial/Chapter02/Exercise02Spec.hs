module Tutorial.Chapter02.Exercise02Spec where

import Test.Hspec
import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter02.Exercise02 as Exercise
import Grin.Examples
import Grin.Pretty
import qualified Grin.Interpreter.Abstract as Solution



spec :: Spec
spec = do
  it "Works for add" $ do
    let prog = convertGExpToExp add
    expected <- Solution.typeInference prog
    result   <- Exercise.typeInference prog
    (PP result) `shouldBe` (PP expected)

  it "Works for fact" $ do
    let prog = convertGExpToExp fact
    expected <- Solution.typeInference prog
    result   <- Exercise.typeInference prog
    (PP result) `shouldBe` (PP expected)

  it "Works for sumSimple" $ do
    let prog = convertGExpToExp sumSimple
    expected <- Solution.typeInference prog
    result   <- Exercise.typeInference prog
    (PP result) `shouldBe` (PP expected)
