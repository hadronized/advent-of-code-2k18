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

-- Interpret the entries into a stream of timed action.
treatEntries :: [Entry] -> [(LocalTime, Action)]
treatEntries = map $ \entry ->
  let time = timestamp entry
  in case T.words (action entry) of
    ["Guard", ident, "begins", "shift"] -> (time, BeginShift $ readGuardianId ident)
    ("falls":_) -> (time, FallAsleep)
    ("wakes":_) -> (time, WakeUp)
    _ -> error "lol"

-- Dispatch actions to their respective guardian.
dispatchActions :: [(LocalTime, Action)] -> Map GuardianId [(LocalTime, Action)]
dispatchActions = go mempty Nothing
  where
    go guardians _ ((t, action@(BeginShift gid)):xs) =
      go (insertAction gid t action guardians) (Just gid) xs

    go guardians jgid@(Just gid) ((t, action):xs) = go (insertAction gid t action guardians) jgid xs

    go guardians _ [] = fmap V.toList guardians

    go _ _ _ = error "dispatchActions: the impossible fucking occurred!"

    insertAction gid t action guardians =
      M.insertWith (flip (<>)) gid (V.singleton (t, action)) guardians

-- Compute sleep times for a single guardian.
--
-- It returns a list of 60 elements giving the number of times a guardian was asleep at a given
-- minute.
minutesCounts :: [(LocalTime, Action)] -> Minutes
minutesCounts = go zeroMinutes Nothing
  where
    zeroMinutes = replicate 60 0

    go minutes (Just sleepTime) ((t, action):xs) =
      case action of
        BeginShift _ -> go (addSleepCount minutes sleepTime t) Nothing xs
        FallAsleep -> go minutes (Just t) xs
        WakeUp -> go (addSleepCount minutes sleepTime t) Nothing xs

    go minutes Nothing ((t, action):xs) =
      case action of
        FallAsleep -> go minutes (Just t) xs
        _ -> go minutes Nothing xs

    go minutes _ [] = minutes
    
    addSleepCount minutes sleepTime t = zipWith (+) minutes range
      where
        range :: Minutes
        range = replicate sleepTime' 0 <> replicate (fromIntegral t' - 1 - sleepTime') 1 <> replicate (59 - t') 0
        sleepTime' = todMin (localTimeOfDay sleepTime)
        t' = todMin (localTimeOfDay t)

type Minute = Natural
type Minutes = [Minute]

type Count = Natural

freqTable :: (Ord a) => [a] -> Map a Count
freqTable = M.fromListWith (+) . map (,1)

findMostOccurring :: Map a Count -> (a, Count)
findMostOccurring = maximumBy (comparing snd) . M.toList

findSleepiest :: Map GuardianId [(LocalTime, Action)] -> (GuardianId, (Minute, Count))
findSleepiest = fmap (findMostOccurring . freqTable) . maximumBy (comparing $ sum . snd) . M.toList . fmap minutesCounts

main :: IO ()
main = do
  putStrLn ""
  entries <- fmap (fromJust . traverse entryFromString . T.lines) T.getContents
  let byTimeSorted = sortBy (comparing timestamp) entries
      sleepiest = findSleepiest . dispatchActions $ treatEntries byTimeSorted
      gid = fst sleepiest
      mins = fst (snd sleepiest)
      result = gid * mins

  print $ "Sleepiest: " <> show sleepiest <> "(" <> show result <> ")"
