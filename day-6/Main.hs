{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import qualified Data.Text as T 

getGroupTotal g = Sum . length $ foldr intersect (mconcat g) g

solvePartTwo = readFile "./input.txt" >>= print . mconcat . fmap getGroupTotal . parseInput

solvePartOne = readFile "./input.txt" >>= print . foldMap (Sum . length . nub . mconcat) . parseInput

parseInput = (fmap T.unpack <$> T.splitOn "\n" <$>) . T.splitOn "\n\n" . T.pack

main :: IO ()
main = solvePartOne >> solvePartTwo