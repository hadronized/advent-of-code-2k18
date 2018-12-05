{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.State (MonadState, gets, modify)
import Data.Foldable (traverse_)
import Data.List (maximumBy, sortBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time (LocalTime(..), TimeOfDay(..), defaultTimeLocale, parseTimeOrError)
import Data.Text as T (Text, drop, lines, split, unpack, words)
import Data.Text.IO as T (getContents)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.Natural (Natural)

readT :: (Read a) => Text -> a
readT = read . unpack

data Entry = Entry {
    timestamp :: LocalTime,
    action :: Text
  } deriving (Show)

type GuardianId = Natural

data Action
  = BeginShift GuardianId
  | WakeUp
  | FallAsleep
    deriving (Eq, Ord, Show)

data St = St {
    stLastGuardianId :: GuardianId,
    stLastAction :: Action
  }

entryFromString :: Text -> Maybe Entry
entryFromString s = case split (== ']') s of
    [timestamp, action] -> Just $ Entry (parse . unpack $ T.drop 1 timestamp) action
    _ -> Nothing
  where
    parse = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"

readGuardianId :: Text -> Natural
readGuardianId = readT . T.drop 1

treatEntries :: [Entry] -> [(LocalTime, Action)]
treatEntries = map $ \entry ->
  let time = timestamp entry
  in case T.words (action entry) of
    ["Guard", ident, "begins", "shift"] -> (time, BeginShift $ readGuardianId ident)
    ("falls":_) -> (time, FallAsleep)
    ("wakes":_) -> (time, WakeUp)
    _ -> error "lol"

type Minute = Natural
type Minutes = [Minute]

sleepers :: [(LocalTime, Action)] -> [(GuardianId, Minutes)]
sleepers entries = go Nothing entries
  where
    go _ [] = []

    go (Just (Just lastT, lastGID)) ((t, BeginShift gid):xs) =
      (lastGID, toMinutes lastT t) : go (Just (Nothing, gid)) xs

    go _ ((t, BeginShift gid):xs) = go (Just (Nothing, gid)) xs

    go (Just (_, lastGID)) ((t, FallAsleep):xs) = go (Just (Just t, lastGID)) xs

    go (Just (Just lastT, lastGID)) ((t, WakeUp):xs) =
      (lastGID, toMinutes lastT t) : go (Just (Nothing, lastGID)) xs

    go a b = error $ "sleepers error: " <> show a <> ": " <> show b

    toMinutes a b = map fromIntegral [todMin (localTimeOfDay a) .. todMin (localTimeOfDay b) - 1]

type Count = Natural

freqTable :: (Ord a) => [a] -> Map a Count
freqTable = M.fromListWith (+) . map (,1)

findMostOccurring :: Map a Count -> (a, Count)
findMostOccurring = maximumBy (comparing snd) . M.toList

findSleepiest :: [(GuardianId, Minutes)] -> (GuardianId, (Minute, Count))
findSleepiest guardians = maximumBy (comparing $ snd . snd) $ M.toList sleepyGuardians
  where
    guardiansMap = M.fromListWith (++) guardians
    sleepyGuardians = fmap (findMostOccurring . freqTable) guardiansMap

main :: IO ()
main = do
  entries <- fmap (fromJust . traverse entryFromString . T.lines) T.getContents
  let byTimeSorted = sortBy (comparing timestamp) entries
      actions = treatEntries byTimeSorted
      sleepies = sleepers actions
      sleepiest = findSleepiest sleepies
      gid = fst sleepiest
      mins = fst (snd sleepiest)
      result = gid * mins

  print $ "Sleepiest: " <> show sleepiest <> "(" <> show result <> ")"
