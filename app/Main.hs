module Main where

import ThreePartition (padInput, solve)

main :: IO ()
main = do
  let l =
        [ 10
        , 100
        , 82
        , 39
        , 87
        , 12
        , 88
        , 63
        , 95
        , 61
        , 88
        ]
      l' = padInput l
  print l'
  putStrLn "Finding partition solution...\n"
  result <- solve l'
  case result of
    Nothing -> putStrLn "No solution found"
    Just sets -> do
      print sets
      print $ map sum sets
