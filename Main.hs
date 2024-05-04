{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Exception (assert)
import Data.Foldable
import Data.List (genericLength, sortOn)
import Data.Proxy
import Data.Reflection
import Data.SBV
import Prelude hiding (pi)

-- | A solution is a sequence of set assignments for each input
data Solution s v = Solution
  { assignments :: [v],
    sums :: [v]
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
          (take n (drop len a)),
        bs
      )

isValid :: [Int] -> Solution s SWord32 -> SBool
isValid i Solution {..} =
  assert
    ( length assignments == length i
        && length sums == n
        && length i `rem` 3 == 0
    )
    $ sAnd
      [ sAll (\x -> x .>= 0 .&& x .< n) assignments,
        sAll
          ( \ci ->
              sum (map (\a -> oneIf (ci .== a)) assignments) .== (3 :: SWord32)
          )
          [0 .. pred (genericLength sums)],
        ala
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
    n :: Num a => a
    n = fromIntegral (length i `div` 3)

    ala k f = k . zipWith f [0 ..]

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

partition :: [Int] -> IO ()
partition i = do
  putStrLn "Finding partition solution...\n"
  reify (length i) $ \(Proxy :: Proxy s) -> do
    LexicographicResult res <- optimize Lexicographic $ do
      let n = length i `div` 3
      assignments <- mkFreeVars (length i)
      sums <- mkFreeVars n
      constrain $ isValid i Solution {..}
      minimize "set-sums" $ foldl' smax 0 sums
    maybe
      (error "No model found")
      dispSolution
      (extractModel res :: Maybe (Solution s Word32))
  where
    dispSolution model = do
      print $ intoSets i model
      print $ map sum (intoSets i model)
      putStrLn $
        "\nValid: "
          ++ show (isValid i (literalize model))
      print model
      where
        literalize s =
          s
            { assignments = map literal (assignments s),
              sums = map literal (sums s)
            }

main :: IO ()
main = do
  let l =
        [ 10,
          100,
          82,
          39,
          87,
          12,
          88,
          63,
          95,
          61,
          88
        ]
      l' = l ++ replicate ((3 - length l `rem` 3) `mod` 3) 0
  print l'
  Main.partition l'
