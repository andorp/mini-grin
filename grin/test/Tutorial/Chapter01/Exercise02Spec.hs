module Tutorial.Chapter01.Exercise02Spec where

import Tutorial.Chapter01.Exercise01 (convertGExpToExp)
import Tutorial.Chapter01.Exercise02
import Test.Hspec
import Grin.Examples as Examples


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Simple interpreter" $ do
    it "works for add" $ do
      res <- interpreter knownExternals $ convertGExpToExp Examples.add
      res `shouldBe` (Prim $ SInt64 30)

    it "works for fact" $ do
      res <- interpreter knownExternals $ convertGExpToExp Examples.fact
      res `shouldBe` (Prim $ SInt64 3628800)

    it "works for sumSimple" $ do
      res <- interpreter knownExternals $ convertGExpToExp Examples.sumSimple
      res `shouldBe` Unit

-- * Test data
