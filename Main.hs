{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- import Data.Csv

import Control.Exception (assert)
import Data.Foldable
import Data.List (genericLength, intercalate, nub, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Data.Reflection
import Data.SBV
import Data.Time
import Prelude hiding (pi)

isValid ::
  [Int] -> -- input values
  [SWord8] -> -- mapping
  SBool
isValid maxGroupSize g p s =
  assert
    ( length (solAssignments s) == length p
        && length (solBlackFacilitators s) == length g
        && length (solFacilitators s) == length g
        && length (solParticipants s) == length g
    )
    $
    -- Every participant is assigned to an applicable group, and the
    -- constraints hold for being associated with that group
    ala
      sAnd
      solAssignments
      ( \pi x ->
          x .>= 0
            .&& x .< genericLength g
            .&& sAll
              ( \gi ->
                  fromIntegral gi .== x
                    .=> eachParticipant (p !! pi) x (g !! gi) gi
              )
              [0 .. length g - 1]
      )
      -- Track how many facilitators are in each group
      .&& ala sAnd solFacilitators (\gi x -> x .== facilitators gi)
      -- Track how many black facilitators are in each group
      .&& ala sAnd solBlackFacilitators (\gi x -> x .== blackFacilitators gi)
      -- Track how many participants are in each group
      .&& ala sAnd solParticipants (\gi x -> x .== participants gi)
      -- Track how many black participants are in each group
      .&& ala sAnd solBlackParticipants (\gi x -> x .== blackParticipants gi)
      -- Ensure correct group sizes
      .&& sAll
        ( \gi ->
            (participants gi .== 0 .&& facilitators gi .== 0)
              .|| ( participants gi .> 0
                      .&& participants gi .<= fromIntegral maxGroupSize
                      .&& facilitators gi .>= 2
                      .&& blackFacilitators gi .>= 1
                  )
        )
        [0 .. length g - 1]
      -- All pairings are honored
      .&& sAll
        ( \(i, j) ->
            solAssignments s !! i .== solAssignments s !! j
        )
        (pairings pPairWith p)
      .&& sAll
        ( \(i, j) ->
            solAssignments s !! i ./= solAssignments s !! j
        )
        (pairings pDoNotPairWith p)
  where
    eachParticipant Participant {..} x Group {..} gi =
      fromBool
        ( gDayOfWeek
            `elem` map
              availDayOfWeek
              pAvailability
            && any
              (canMeet pIsFacilitator gStartTime)
              pAvailability
        )
        .&& maybe
          minBound
          fromIntegral
          pPrefereredMinGroupSize
          .<= solParticipants s !! gi
        .&& maybe
          maxBound
          fromIntegral
          pPrefereredMaxGroupSize
          .>= solParticipants s !! gi
        .&& x .== maybe x fromIntegral pFixed

    ala k acc f = k (zipWith f [0 ..] (acc s))

    participants = countParticipants (const True)
    blackParticipants = countParticipants pIsBlack
    facilitators = countParticipants pIsFacilitator
    blackFacilitators =
      countParticipants (\i -> pIsFacilitator i && pIsBlack i)

    countParticipants :: (Participant -> Bool) -> Int -> SWord8
    countParticipants f gi =
      ala
        sum
        solAssignments
        ( \pi a ->
            oneIf
              ( fromIntegral gi .== a
                  .&& fromBool (f (p !! pi))
              )
        )

showSchedule :: [Participant] -> [Group] -> Solution s Word8 -> String
showSchedule p g s =
  unlines $ intercalate [""] $ map f assigned
  where
    assigned = nub (solAssignments s)

    f :: Word8 -> [String]
    f gi =
      [ gName (g !! fromIntegral gi),
        "===================="
      ]
        ++ concat
          ( zipWith
              (\pi i -> [getName (p !! pi) | i == gi])
              [0 ..]
              (solAssignments s)
          )

    getName i =
      ( if pIsBlack i
          then "*"
          else " "
      )
        ++ ( if pIsFacilitator i
               then "F"
               else " "
           )
        ++ " "
        ++ pName i

scheduleGroups :: Size -> [Participant] -> IO ()
scheduleGroups maxGroupSize p = do
  let g = determineGroups p
  putStrLn "Finding scheduling solution...\n"
  reify (length g, length p) $ \(Proxy :: Proxy s) -> do
    LexicographicResult res <- optimize Lexicographic $ do
      solAssignments <- mkFreeVars (length p)
      solBlackFacilitators <- mkFreeVars (length g)
      solFacilitators <- mkFreeVars (length g)
      solBlackParticipants <- mkFreeVars (length g)
      solParticipants <- mkFreeVars (length g)
      constrain $ isValid maxGroupSize g p Solution {..}
      minimize "number-facilitators" $ foldl' smax 0 solFacilitators
      maximize "balance-participants" $ foldl' smin 0 solBlackParticipants
    case extractModel res :: Maybe (Solution s Word8) of
      Nothing -> error "No model found"
      Just model -> dispSolution g model
  where
    dispSolution :: [Group] -> Solution s Word8 -> IO ()
    dispSolution g model = do
      putStr $ showSchedule p g model
      putStrLn $
        "\nValid: "
          ++ show (isValid maxGroupSize g p (literalize model))
      where
        literalize s =
          s
            { solAssignments = map literal (solAssignments s),
              solBlackFacilitators = map literal (solBlackFacilitators s),
              solFacilitators = map literal (solFacilitators s),
              solBlackParticipants = map literal (solBlackParticipants s),
              solParticipants = map literal (solParticipants s)
            }

-- Given a CSV file of the proper schedule, generate and display an updated
-- version of that CSV file which assigns participants to groups.
c2gSchedule :: FilePath -> IO ()
c2gSchedule _ = undefined

main :: IO ()
main = do
  -- print $ pairings pPairWith participants
  -- print $ pairings pDoNotPairWith participants
  scheduleGroups 20 participants
  where
    participants =
      [ Participant
          { pName = "Aaron",
            pIsFacilitator = True,
            pIsBlack = False,
            pAvailability =
              [ Available Thursday 1200 2000,
                Available Friday 1200 2000,
                Available Saturday 1200 2000
              ],
            pPairWith = ["Susan"],
            pDoNotPairWith = [],
            pPrefereredMinGroupSize = Nothing,
            pPrefereredMaxGroupSize = Nothing,
            pFixed = Nothing
          },
        Participant
          { pName = "Regina",
            pIsFacilitator = True,
            pIsBlack = True,
            pAvailability =
              [ Available Thursday 1200 2000,
                Available Friday 1200 2000,
                Available Saturday 1200 2000
              ],
            pPairWith = [],
            pDoNotPairWith = [],
            pPrefereredMinGroupSize = Nothing,
            pPrefereredMaxGroupSize = Nothing,
            pFixed = Nothing
          },
        Participant
          { pName = "John",
            pIsFacilitator = True,
            pIsBlack = False,
            pAvailability =
              [ Available Monday 1200 2000,
                Available Tuesday 1200 2000,
                Available Wednesday 1200 2000,
                Available Thursday 1200 2000
              ],
            pPairWith = [],
            pDoNotPairWith = [],
            pPrefereredMinGroupSize = Nothing,
            pPrefereredMaxGroupSize = Nothing,
            pFixed = Nothing
          },
        Participant
          { pName = "Cherlynn",
            pIsFacilitator = True,
            pIsBlack = True,
            pAvailability =
              [ Available Monday 1200 2000,
                Available Tuesday 1200 2000,
                Available Wednesday 1200 2000
              ],
            pPairWith = [],
            pDoNotPairWith = [],
            pPrefereredMinGroupSize = Nothing,
            pPrefereredMaxGroupSize = Nothing,
            pFixed = Nothing
          },
        Participant
          { pName = "Susan",
            pIsFacilitator = True,
            pIsBlack = False,
            pAvailability =
              [ Available Monday 1200 2000,
                Available Tuesday 1200 2000,
                Available Wednesday 1200 2000,
                Available Thursday 1200 2000,
                Available Friday 1200 2000,
                Available Saturday 1200 2000
              ],
            pPairWith = [],
            pDoNotPairWith = [],
            pPrefereredMinGroupSize = Nothing,
            pPrefereredMaxGroupSize = Nothing,
            pFixed = Nothing
          }
      ]
