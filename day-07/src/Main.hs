module Main where

import Debug.Trace (trace)

import Data.Char (ord)
import Data.Foldable (foldr')
import Data.List (minimumBy, sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Numeric.Natural (Natural)

main :: IO ()
main = do
  steps <- fmap (traverse parseStep . lines) getContents
  case steps of
    Left e -> do
      putStrLn $ "nope: " <> e

    Right steps' -> do
      let gr = foldr' accumDep mempty steps'
          plan = stepAvailable gr
          planPar = stepAvailablePar 0 mempty gr

      print plan
      print planPar

data Dep = Dep {
    -- | Required step to go through first.
    dependency :: Step,
    -- | Step to take after.
    dependent :: Step
  } deriving (Eq, Show)

parseStep :: String -> Either String Dep
parseStep line = case words line of
  ["Step", step0, "must", "be", "finished", "before", "step", step1, "can", "begin."] ->
    Right $ Dep (head step0) (head step1)

  _ -> Left line

type Graph = Map Step (Set Step)
type Step = Char

-- part 1

accumDep :: Dep -> Graph -> Graph
accumDep dep = M.insertWith (<>) (dependent dep) (S.singleton dependency') . M.insertWith (<>) dependency' mempty
  where
    dependency' = dependency dep

getAvailable :: Graph -> [Step]
getAvailable gr = [step | (step, set) <- M.toList gr, S.null set]

stepAvailable :: Graph -> [Step]
stepAvailable gr = case sort (getAvailable gr) of
    [] -> []
    (s:sx) -> s : stepAvailable (removeStep s gr)

removeStep :: Step -> Graph -> Graph
removeStep s = purgeDep s . M.delete s
  where
    purgeDep = fmap . S.delete

-- part 2

type Time = Natural
type RemainingTime = Time
type Work = Map Step RemainingTime

maxWorkerNb :: Int
maxWorkerNb = 5

stepAvailablePar :: Time -> Work -> Graph -> Time
stepAvailablePar currentTime work gr =
    case timeIncrement of
      Just (_, timeIncrement') ->
        stepAvailablePar (currentTime + timeIncrement') (fmap (decrWork timeIncrement') backlog) gr'

      Nothing -> trace (show work) currentTime
  where
    (done, running) = M.partition (== 0) work
    gr' = foldl (flip removeStep) gr (map fst $ M.toList done)
    backlog = running <> (M.take availableWorkers . flip M.difference running . M.fromList . map timeStep $ getAvailable gr')
    timeStep s = (s, fromIntegral $ 61 + ord s - ord 'A')
    availableWorkers = maxWorkerNb - length running
    decrWork = flip (-)
    timeIncrement = case M.toList backlog of
      [] -> Nothing
      bl -> Just (minimumBy (comparing snd) bl)
