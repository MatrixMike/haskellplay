{-# LANGUAGE BlockArguments #-}


import Test.QuickCheck
import Test.Hspec

-- import Tasty


-- Hasura:
-- Python test-suite - OSS (Golden-Tests)
-- https://hspec.github.io
-- https://hackage.haskell.org/package/hspec-golden
-- https://hackage.haskell.org/package/QuickCheck
-- https://hackage.haskell.org/package/tasty

-- Exotic:
-- By hand
-- DocTests


-- Entrypoints:
-- 


prop_reflexivity_equal :: Int -> Bool
prop_reflexivity_equal i = i == i

prop_ord :: Int -> Int -> Property
prop_ord a b = a /= b ==> a > b || b > a

-- main :: IO ()
-- main = do
--     quickCheck prop_reflexivity_equal

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "bla" $ do
      property prop_reflexivity_equal

    it "bla1" $ do
      property prop_ord
