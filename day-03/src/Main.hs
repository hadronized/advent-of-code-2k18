module Main where

import Data.Foldable (foldl')
import Data.Map.Strict as M (Map, filter, insertWith)
import Data.Text (Text, pack, split, strip, unpack)
import Numeric.Natural (Natural)

main :: IO ()
main = do
  input <- fmap (traverse (parseClaim . pack) . lines) getContents

  case input of
    Just claims -> do
      -- compute the overlapping area
      let plan = checkClaims claims
          inches = overlappingInches plan

      putStrLn $ "Overlap (inches): " <> show inches

    Nothing -> putStrLn "oops"

-- First part

type Claim = (Id, Pos, Area)
type Id = Text
type Pos = (Int, Int)
type Area = (Int, Int)

-- Parse a single claim.
parseClaim :: Text -> Maybe Claim
parseClaim claim =
    case stripped of
      [id, pos, dim] -> case (unPos pos, unDim dim) of
        ([line, row], [width, height]) -> Just (id, (readT line, readT row), (readT width, readT height))
        _ -> Nothing
      _ -> Nothing
  where
    stripped = map strip $ split (\c -> c == '@' || c == ':') claim
    unPos = split (== ',')
    unDim = split (== 'x')
    readT = read . unpack

-- The plan as seen by Elves – exploded into 1x1 inches piece of fabric.
type Plan = Map (Int, Int) Natural

-- Take claims into account.
checkClaims :: [Claim] -> Plan
checkClaims = foldl' (\p (x, y) -> checkInch x y p) mempty . concatMap claim1x1

-- Function called whenever an inch of fabric is claimed by a dude.
checkInch :: Int
          -- ^ Line coordinate.
          -> Int 
          -- ^ Row coordinate.
          -> Plan
          -- ^ Claims associated with the number of occurrences.
          -> Plan
checkInch line row = insertWith (+) (line, row) 1

-- Get the area in inches of overlapping fabric.
overlappingInches :: Plan -> Int
overlappingInches = length . M.filter (> 1)

-- Convert a claim into a list of 1x1 pieces of fabric.
claim1x1 :: Claim -> [(Int, Int)]
claim1x1 (_, (line, row), (width, height)) =
  [(x, y) | x <- [line .. line + width - 1], y <- [row .. row + height - 1]]
