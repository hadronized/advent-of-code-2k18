{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Monad.State (MonadState, gets, modify)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (maximumBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.Text as T (Text, drop, lines, split, unpack, words)
import Data.Text.IO as T (getContents)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric.Natural (Natural)

readT :: (Read a) => Text -> a
readT = read . unpack

data Entry = Entry {
    timestamp :: UTCTime,
    action :: Text
  } deriving (Show)

type GuardianId = Natural

data Action
  = BeginShift UTCTime
  | WakeUp UTCTime
  | FallAsleep UTCTime
    deriving (Eq, Ord, Show)

data St = St {
    stLastGuardianId :: GuardianId,
    stLastAction :: Action,
    stGuardianMinutes :: M.Map GuardianId (Vector Minute)
  }

entryFromString :: Text -> Maybe Entry
entryFromString s = case split (== ']') s of
    [timestamp, action] -> Just $ Entry (parse . unpack $ T.drop 1 timestamp) action
    _ -> Nothing
  where
    parse = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"

readGuardianId :: Text -> Natural
readGuardianId = readT . T.drop 1

-- read an entry and accumulate state
accumEntry :: MonadState St m => Entry -> m ()
accumEntry entry = do
  case T.words (action entry) of
    ["Guard", ident, "begins", "shift"] -> do
      -- if the previous one wasn’t sleeping
      gets stLastAction >>= \case
        BeginShift start -> do
          -- 

      modify $ \st -> do
        st { stLastGuardianId = readGuardianId ident,
             stLastAction = BeginShift time
           }

    ("falls":_) -> modify $ \st -> st { stLastAction = FallAsleep time }

    ("wakes":_) -> do
      modify $ \st -> st { stLastAction = FallAsleep time }

    _ -> pure ()
  where
    time = timestamp entry

type Minute = Int

accumMinutes :: GuardianId -> Minute -> Minute -> M.Map GuardianId (Vector Minute) -> M.Map GuardianId (Vector Minute)
accumMinutes gid start end guardians =
    case M.lookup gid guardians of
      Nothing -> M.insert gid (accumMinutes zeroMinutes) guardians
      Just minutes -> M.insert gid (accumMinutes minutes) guardians
  where
    accumMinutes minutes = V.accum (+) minutes [(m, 1) | m <- [start..end]]
    zeroMinutes = V.fromList [0..59]

findMostAsleep :: M.Map GuardianId (Vector Minute) -> (GuardianId, Minute)
findMostAsleep = maximumBy (compare `on` snd) . M.toList . fmap V.maximum

main :: IO ()
main = do
  entries <- fmap (fromJust . traverse entryFromString . T.lines) T.getContents
  let byTimeSorted = sortBy (compare `on` timestamp) entries
  traverse_ print byTimeSorted
