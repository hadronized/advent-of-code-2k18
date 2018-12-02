{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Vector as V (Vector, accum, replicate)
import Numeric.Natural (Natural)

main :: IO ()
main = do
  hashes <- fmap lines getContents
  let checksum = uncurry (*) $ foldl' treatHash (0, 0) hashes 

  putStrLn $ "Checksum: " <> show checksum
  putStrLn $ "Common letters: " <> searchCommon hashes

-- First problem

type State = Vector Natural

initialState :: State
initialState = V.replicate 26 0

treatHash :: (Natural, Natural) -> String -> (Natural, Natural)
treatHash occurs hash =
    let result :: State = foldl' replace initialState hash
    in addPair occurs (foldl' incrOccur (0, 0) result)
  where
    replace st l = accum (+) st [(ord l - ord 'a', 1)]
    incrOccur i@(twos, threes) x
      | x == 2 = (1, threes)
      | x == 3 = (twos, 1)
      | otherwise = i
    addPair (a, b) (c, d) = (a + c, b + d)

-- Second problem

-- O(n²), to start off
searchCommon :: [String] -> String
searchCommon [] = ""
searchCommon (h:hs) = search h hs
  where
    search h [] = searchCommon hs
    search h (x:xs)
      | almostSame h x = commonLetters h x
      | otherwise = search h xs

-- recognize that strings are almost identical by a single character change
almostSame :: String -> String -> Bool
almostSame = go 0
  where
    go diffNb (l:ls) (r:rs)
      | diffNb > 1 = False
      | l /= r = go (succ diffNb) ls rs
      | otherwise = go diffNb ls rs
    go diffNb _ _ = diffNb == 1

-- get the letters that two strings have in common;
-- this function is optimized and works only on strings which have passed the almostSame
-- property
commonLetters :: String -> String -> String
commonLetters (l:ls) (r:rs)
  | l == r = l : commonLetters ls rs
  | otherwise = ls -- all the rest is identic, smartass
