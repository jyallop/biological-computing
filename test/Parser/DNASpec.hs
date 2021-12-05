module Parser.DNASpec where

import Test.Hspec
import Parser.DNA
import Data.DNA

spec :: Spec
spec = do
  describe "DNA Parser Test" $ do
    it "Parse empty string" $
      parseDNA "" `shouldBe` []

    it "Parse Single A" $
      parseDNA "A" `shouldBe` [A]

    it "Parse Single C" $
      parseDNA "C" `shouldBe` [C]

    it "Parse Single G" $
      parseDNA "G" `shouldBe` [G]

    it "Parse Single T" $
      parseDNA "T" `shouldBe` [T]
