module Main where

import Data.IntSet (IntSet, empty, insert, member)

main :: IO ()
main = do
    numbers <- fmap (map parse . lines) getContents

    putStrLn $ "Sum: " <> show (sum numbers)
    putStrLn $ "Freq twice: " <> show (findFreq empty 0 $ cycle numbers)
  where
    parse ('+':xs) = read xs
    parse n = read n
    findFreq _ freq [] = freq
    findFreq knownFreq freq (change:xs)
        | newFreq `member` knownFreq = newFreq
        | otherwise = findFreq (insert newFreq knownFreq) newFreq xs
      where
        newFreq = freq + change
