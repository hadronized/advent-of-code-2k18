module Main where

import Data.Char (isLetter, isLower, isUpper, toLower)

main :: IO ()
main = getContents >>= print . length . reduce

reduce :: String -> String
reduce = go []
  where
    go [] (x:xs) = go [x] xs
    go a [] = a
    go (a:xs) (b:bs)
      | not (isLetter b) = go (a:xs) bs
      | (toLower a /= toLower b) || (isLower a && isLower b) || (isUpper a && isUpper b) = go (b:a:xs) bs
      | otherwise = go xs bs
