{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Main where

import Debug.Trace (trace)

import Control.Monad (replicateM)
import Control.Monad.State (State, gets, evalState, modify)
import Data.List.NonEmpty as NE (NonEmpty, fromList, toList)
import Numeric.Natural (Natural)

data Node = Node {
    nodeHeader :: NodeHeader,
    nodeChildren :: [Node],
    nodeMetadata :: NonEmpty Natural
  } deriving (Eq, Show)

data NodeHeader = NodeHeader {
    nodeHeaderNodeNb :: Natural,
    nodeHeaderMetadataNb :: Natural
  } deriving (Eq, Show)

data ParseStackInfo
  = ChildrenNb Natural
  | MetadataNb Natural
    deriving (Eq, Show)

newtype Parser a = Parser { runParser :: State [Natural] a } deriving (Applicative, Functor, Monad)

parse :: String -> Node
parse = evalState (runParser parseNode) . map read . words

readInput :: Parser Natural
readInput = Parser $ gets head <* modify tail

parseNode :: Parser Node
parseNode = do
  childrenNb <- readInput
  metadataNb <- readInput

  children <- replicateM (fromIntegral childrenNb) parseNode
  metadata <- fmap NE.fromList (replicateM (fromIntegral metadataNb) readInput)

  pure $ Node (NodeHeader childrenNb metadataNb) children metadata

-- part 1
checksum :: Node -> Natural
checksum node = metadataChecksum (nodeMetadata node) + sum (map checksum $ nodeChildren node)

metadataChecksum :: NonEmpty Natural -> Natural
metadataChecksum = sum . NE.toList

-- part 2
nodeValue :: Node -> Natural
nodeValue (Node _ [] metadata) = metadataChecksum metadata
nodeValue (Node _ children metadata) = sum [nodeValue n | Just n <- map index (NE.toList metadata)]
  where
    index i =
      let i' = fromIntegral i - 1
      in if i' < 0 || i' >= length children then Nothing else Just (children !! i')

main :: IO ()
main = do
  root <- fmap parse getContents

  putStrLn $ "Checksum: " <> show (checksum root)
  putStrLn $ "Root value: " <> show (nodeValue root)
