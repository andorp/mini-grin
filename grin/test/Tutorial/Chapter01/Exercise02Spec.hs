module Tutorial.Chapter01.Exercise02Spec where

import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter01.Exercise02
import Test.Hspec
import Grin.Value
import Grin.Examples as Examples


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Simple interpreter" $ do
    it "works for add" $ do
      let ctx = externalCalls
      res <- interpret ctx $ convertGExpToExp Examples.add
      res `shouldBe` (Lit $ LInt64 30)

    it "works for fact" $ do
      let ctx = externalCalls
      res <- interpret ctx $ convertGExpToExp Examples.fact
      res `shouldBe` (Lit $ LInt64 3628800)

    it "works for sumSimple" $ do
      let ctx = externalCalls
      res <- interpret ctx $ convertGExpToExp Examples.sumSimple
      res `shouldBe` Unit

-- * Test data
