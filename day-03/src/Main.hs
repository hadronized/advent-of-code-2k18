module Main where

import Data.Foldable (foldl')
import Data.Map.Strict as M (Map, adjust, filter, fromList, insert, insertWith, lookup, toList)
import Data.Maybe (fromJust)
import Data.Set as S (Set, insert, singleton)
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
      putStrLn $ "Non-overlap (ID): " <> show (searchNonOverlapped claims)

    Nothing -> putStrLn "oops"

-- First part

-- The plan as seen by Elves – exploded into 1x1 inches piece of fabric.
type Plan = Map (Int, Int) Natural
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
claim1x1 :: Claim -> [Pos]
claim1x1 (_, (line, row), (width, height)) =
  [(x, y) | x <- [line .. line + width - 1], y <- [row .. row + height - 1]]

-- Second part

-- Per piece of 1x1, all claims 
type Claimed = Map Pos (Set Id)

-- Number of time a given fabric overlaps another
type Overlapped = Map Id Natural

-- Find the fabric ID that doesn’t overlap any other.
searchNonOverlapped :: [Claim] -> Maybe Id
searchNonOverlapped claims =
    case M.toList filtered of
      [(i, _)] -> Just i
      _ -> Nothing
  where
    (_, overlapped) = indexClaims claims
    filtered = M.filter (== 0) overlapped

-- Index all claims.
indexClaims :: [Claim] -> (Claimed, Overlapped)
indexClaims claims = foldl' (\(cl, ov) c -> indexClaim c cl ov) initial claims
  where
    initial = (mempty, M.fromList [(i, 0) | (i, _, _) <- claims])

-- Index a whole claim.
indexClaim :: Claim -> Claimed -> Overlapped -> (Claimed, Overlapped)
indexClaim claim@(i, _, _) claimed overlapped =
  foldl' (\(cl, ov) pos -> indexFabric i pos cl ov) (claimed, overlapped) (claim1x1 claim)

-- Index a piece of fabric for a given Elve.
indexFabric :: Id -> Pos -> Claimed -> Overlapped -> (Claimed, Overlapped)
indexFabric i pos claimed overlapped =
    case M.lookup pos claimed of
      Nothing ->
        let newClaimed = M.insert pos (S.singleton i) claimed
        in (newClaimed, overlapped)
      Just ids ->
        let ids_ = S.insert i ids
            newClaimed = M.insert pos ids_ claimed
            -- there’s already something here; so we must increment the overlap of everything that we find
            -- in that set
            newOverlapped = foldl' (\o id_ -> adjust succ id_ o) overlapped ids_
        in (newClaimed, newOverlapped)
