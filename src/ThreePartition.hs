{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ThreePartition (
  solve,
  padInput,
)
where

import Control.Exception (assert)
import Data.Foldable
import Data.List (sortOn)
import Data.Proxy
import Data.Reflection
import Data.SBV hiding (pred, solve)
import Data.SBV.Internals (SatModel (..))
import Prelude hiding (pi)

-- | A solution is a sequence of set assignments for each input
data Solution s v = Solution
  { assignments :: [v]
  , sums :: [v]
  }
  deriving (Show)

-- We inject the size of the input list, in order to know how to group the
-- variables coming from the solver.
instance (SatModel v, Reifies s Int) => SatModel (Solution s v) where
  parseCVs as = do
    let len = reflect (Proxy :: Proxy s)
        n = len `div` 3
    (a, bs) <- parseCVs as
    return
      ( Solution
          (take len a)
          (take n (drop len a))
      , bs
      )

isValid :: [Int] -> Solution s SWord32 -> SBool
isValid i Solution {..} =
  assert
    ( length assignments == length i
        && length sums == n
        && length i `rem` 3 == 0
    )
    $ sAnd
      [ sAll (\x -> x .>= 0 .&& x .< n) assignments
      , sAll
          ( \ci ->
              sum (map (\a -> oneIf (ci .== a)) assignments) .== (3 :: SWord32)
          )
          (map fromIntegral [0 .. length sums - 1 :: Int])
      , ala
          sAnd
          ( \ci x ->
              x
                .== ala
                  sum
                  (\ai a -> ite (ci .== a) (fromIntegral (i !! ai)) 0)
                  assignments
          )
          sums
      ]
 where
  n :: (Num a) => a
  n = fromIntegral (length i `div` 3)

  ala k f = k . zipWith (\idx -> f (fromIntegral (idx :: Int))) [0 ..]

intoSets :: [Int] -> Solution s Word32 -> [[Int]]
intoSets i Solution {..} = map f assigned
 where
  assigned =
    reverse (map fst (sortOn snd (zip ([0 ..] :: [Word32]) sums)))
  f :: Word32 -> [Int]
  f gi =
    concat $
      zipWith
        (\ai x -> [i !! ai | x == gi])
        [0 ..]
        assignments

-- | Pad an input list to make its length divisible by 3.
padInput :: [Int] -> [Int]
padInput l = l ++ replicate ((3 - length l `rem` 3) `mod` 3) 0

{- | Solve the 3-partition problem for a list of integers.
Returns the list of groups if a solution is found.
-}
solve :: [Int] -> IO (Maybe [[Int]])
solve i = reify (length i) $ \(Proxy :: Proxy s) -> do
  LexicographicResult res <- optimize Lexicographic $ do
    let n = length i `div` 3
    assignments <- mkFreeVars (length i)
    sums <- mkFreeVars n
    constrain $ isValid i Solution {..}
    minimize "set-sums" $ foldl' smax 0 sums
  case extractModel res :: Maybe (Solution s Word32) of
    Nothing -> return Nothing
    Just model -> return $ Just (intoSets i model)
