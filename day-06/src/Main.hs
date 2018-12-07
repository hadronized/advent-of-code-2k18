{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (foldl')
import Data.List (maximumBy, sortBy)
import Data.Map as M (Map, fromListWith, toList)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text as T (Text, lines, split, strip, unpack)
import Data.Text.IO as T (getContents)
import Numeric.Natural (Natural)

readT :: (Read a) => Text -> a
readT = read . unpack

parse :: Text -> Point 
parse t = case split (== ',') t of
  [x, y] -> (readT $ strip x, readT $ strip y)
  _ -> error $ "parse: " <> unpack t

type Point = (Int, Int)

-- part 1

-- Axis Aligned Bounding Box
data AABB = AABB {
    aabbLower :: Point,
    aabbUpper :: Point
  } deriving (Eq, Show)

findAABB :: [Point] -> AABB
findAABB [] = error "nein"
findAABB (a:ls) = foldl' updateAABB (AABB a a) ls
  where
    updateAABB (AABB (lx, ly) (ux, uy)) (x, y) = AABB {
        aabbLower = (min (min lx x) lx, min (min ly y) ly),
        aabbUpper = (max (max ux x) ux, max (max uy y) uy)
      }

aabbToStream :: AABB -> [Point]
aabbToStream (AABB (lx, ly) (ux, uy)) =
  [(x, y) | x <- [lx .. ux], y <- [ly .. uy]]

liesOnAABB :: Point -> AABB -> Bool
liesOnAABB (x, y) (AABB (lx, ly) (ux, uy)) =
  x == lx || x == ux || y == ly || y == uy

manhDist :: Point -> Point -> Int
manhDist (a, b) (c, d) = abs (a - c) + abs (b - d)

nearest :: Point -> [(Int, Point)] -> Maybe Int
nearest p points =
  case sortBy (comparing snd) $ map (\(i, x) -> (i, manhDist p x)) points of
    [a] -> Just (fst a)
    a:b:_ -> if snd a == snd b then Nothing else Just (fst a)
    _ -> error "nearest"

freqs :: (Ord a) => [a] -> Map a Natural
freqs = fromListWith (+) . map (,1)

biggestArea :: [Maybe Int] -> Natural
biggestArea = snd . maximumBy (comparing snd) . M.toList . freqs . catMaybes

blackListPoints :: [(Point, Maybe Int)] -> AABB -> Set Int
blackListPoints points aabb = foldl' blacklist mempty points
  where
    blacklist blist (p, Just i) = if liesOnAABB p aabb then S.insert i blist else blist
    blacklist blist _ = blist

filterByBlacklist :: Set Int -> [(Point, Maybe Int)] -> [(Point, Maybe Int)]
filterByBlacklist blacklist = filter lookupBlacklist
  where
    lookupBlacklist (_, Just i) = i `S.notMember` blacklist
    lookupBlacklist _ = False

main :: IO ()
main = do
  coords <- fmap (map parse . T.lines) T.getContents
  let aabb = findAABB coords
      points = aabbToStream aabb
      indexed = zip [0..] coords
      nearests = map (\p -> (p, nearest p indexed)) points
      blacklist = blackListPoints nearests aabb
      filteredNearests = filterByBlacklist blacklist nearests
      area = biggestArea (map snd filteredNearests)

  print area
