module Parser.DNASpec where

import Test.Hspec
import Parser.DNA
import Data.DNA
import Data.Nucleotide

spec :: Spec
spec = do
  describe "DNA Parser Test" $ do
    it "Parse empty string" $
      parseDNA "" `shouldBe` Right []

    it "invalid input returns nothing" $
      parseDNA "ASCXVASDF" `shouldBe` Left "Unable to parse character: S"
      
    it "Parse Single A" $
      parseDNA "A" `shouldBe` Right [A]

    it "Parse Single C" $
      parseDNA "C" `shouldBe` Right [C]

    it "Parse Single G" $
      parseDNA "G" `shouldBe` Right [G]

    it "Parse Single T" $
      parseDNA "T" `shouldBe` Right [T]

    it "Parse Sample Sequence" $
      parseDNA "ACGACTGTCAGTA" `shouldBe` Right [A, C, G, A, C, T, G, T, C, A, G, T, A]
