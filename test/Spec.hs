{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import ThreePartition (padInput)

main :: IO ()
main = hspec $ do
  describe "padInput" $ do
    it "pads list of length 11 to length 12" $
      length (padInput [1 .. 11]) `shouldBe` 12

    it "does not pad list already divisible by 3" $
      padInput [1 .. 9] `shouldBe` [1 .. 9]

    it "pads with zeros" $
      drop 4 (padInput [1 .. 4]) `shouldBe` [0, 0]

    it "handles empty list" $
      padInput [] `shouldBe` []

    it "handles single element" $
      padInput [42] `shouldBe` [42, 0, 0]

    it "handles two elements" $
      padInput [1, 2] `shouldBe` [1, 2, 0]

    prop "always produces length divisible by 3" $
      \(xs :: [Int]) -> length (padInput xs) `mod` 3 == 0

    prop "preserves original elements" $
      \(xs :: [Int]) -> take (length xs) (padInput xs) == xs

    prop "only pads with zeros" $
      \(xs :: [Int]) -> all (== 0) (drop (length xs) (padInput xs))
