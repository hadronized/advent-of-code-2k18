module Main where

import Data.Char (isLetter, isLower, isUpper, toLower)

main :: IO ()
main = do
  input <- getContents
  print . length $ reduce input
  print (bruteForce input)

-- part 1

reduce :: String -> String
reduce = go []
  where
    go [] (x:xs) = go [x] xs
    go a [] = a
    go (a:xs) (b:bs)
      | not (isLetter b) = go (a:xs) bs
      | (toLower a /= toLower b) || (isLower a && isLower b) || (isUpper a && isUpper b) = go (b:a:xs) bs
      | otherwise = go xs bs

-- part 2

bruteForce :: String -> Int
bruteForce polymers = minimum (map length allReduced)
  where
    types = ['a' .. 'z']
    allReduced = map (\c -> reduce $ filter (\x -> toLower x /= c) polymers) types
