module Main where

import Data.Foldable (foldr')
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  steps <- fmap (traverse parseStep . lines) getContents
  case steps of
    Left e -> do
      putStrLn $ "nope: " <> e

    Right steps' -> do
      let gr = foldr' accumDep mempty steps'
          plan = stepAvailable gr
      print plan

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
    (s:sx) -> s : stepAvailable (purgeDep s $ M.delete s gr)
  where
    purgeDep = fmap . S.delete
