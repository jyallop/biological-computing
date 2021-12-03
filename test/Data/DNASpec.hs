module Data.DNASpec (spec) where

import Test.Hspec
import Data.DNA

spec :: Spec
spec = do
  describe "count" $ do
    it "empty list should return all zeros" $
      count [] `shouldBe` (0, 0, 0, 0)

    it "list of all As" $
      count [A, A, A, A] `shouldBe` (4, 0, 0, 0)

    it "list of all Cs" $
      count [C, C, C, C] `shouldBe` (0, 4, 0, 0)

    it "list of all Gs" $
      count [G, G, G, G] `shouldBe` (0, 0, 4, 0)

    it "list of all Ts" $
      count [T, T, T, T] `shouldBe` (0, 0, 0, 4)

    it "test on a real list" $
      count [A, C, G, T, T, T] `shouldBe` (1, 1, 1, 3)

