module Parser.DNASpec where

import Test.Hspec
import Parser.DNA
import Data.DNA

spec :: Spec
spec = do
  describe "DNA Parser Test" $ do
    it "Parse empty string" $
      parseDNA "" `shouldBe` Just []

    it "invalid input returns nothing" $
      parseDNA "ASCXVASDF" `shouldBe` Nothing
      
    it "Parse Single A" $
      parseDNA "A" `shouldBe` Just [A]

    it "Parse Single C" $
      parseDNA "C" `shouldBe` Just [C]

    it "Parse Single G" $
      parseDNA "G" `shouldBe` Just [G]

    it "Parse Single T" $
      parseDNA "T" `shouldBe` Just [T]

    it "Parse Sample Sequence" $
      parseDNA "ACGACTGTCAGTA" `shouldBe` Just [A, C, G, A, C, T, G, T, C, A, G, T, A]
