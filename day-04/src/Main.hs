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

main :: IO ()
main = do
  putStrLn ""
  entries <- fmap (fromJust . traverse entryFromString . T.lines) T.getContents
  let byTimeSorted = sortBy (comparing timestamp) entries
      dispatched = dispatchActions (treatEntries byTimeSorted)
      sleepiest = findSleepiest dispatched
      (gid, mins) = findMostFrequentlySleepy dispatched
      computeResult s =
        let gid = fst s
            mins = fst (snd s)
        in gid * mins

  print $ "Sleepiest: " <> show (computeResult sleepiest)
  print $ "Frequently sleepy: " <> show (gid * mins)

-- part 1

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
    asMinutes = todMin . localTimeOfDay

    go minutes (Just sleepTime) ((t, action):xs) =
      case action of
        BeginShift _ -> go minutes Nothing xs --go (addSleepCount minutes (asMinutes sleepTime) 60) Nothing xs
        FallAsleep -> go minutes (Just t) xs
        WakeUp -> go (addSleepCount minutes (asMinutes sleepTime) (asMinutes t)) Nothing xs

    go minutes Nothing ((t, action):xs) =
      case action of
        FallAsleep -> go minutes (Just t) xs
        _ -> go minutes Nothing xs

    go minutes _ [] = minutes
    
    addSleepCount minutes sleepTime t = zipWith (+) minutes range
      where
        range :: Minutes
        range = replicate sleepTime 0 <> replicate (fromIntegral t - sleepTime) 1 <> replicate (60 - t) 0

type Minute = Natural
type Minutes = [Minute]

type Count = Natural

freqTable :: (Ord a) => [a] -> Map a Count
freqTable = M.fromListWith (+) . map (,1)

findMostOccurring :: Map a Count -> (a, Count)
findMostOccurring = maximumBy (comparing snd) . M.toList

findSleepiest :: Map GuardianId [(LocalTime, Action)] -> (GuardianId, (Minute, Count))
findSleepiest =
    fmap (findMostOccurring . freqTable . spanIndex) . maximumBy (comparing $ sum . snd) . M.toList . fmap minutesCounts
  where
    spanIndex = concatMap (\(i, x) -> replicate (fromIntegral x) i) . zip [0..]

-- part 2
findMostFrequentlySleepy :: Map GuardianId [(LocalTime, Action)] -> (GuardianId, Minute)
findMostFrequentlySleepy =
    fmap findMin . maximumBy (comparing $ maximum . snd) . M.toList . fmap minutesCounts
  where
    findMin = fst . maximumBy (comparing snd) . zip [0..]
