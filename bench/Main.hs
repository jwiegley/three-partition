module Main where

import Criterion.Main
import ThreePartition (padInput)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "padInput"
        [ bench "small" $ nf padInput [1 .. 9]
        , bench "medium" $ nf padInput [1 .. 99]
        , bench "large" $ nf padInput [1 .. 999]
        ]
    ]
