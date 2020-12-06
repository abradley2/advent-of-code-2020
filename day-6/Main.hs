{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec as P
import Data.List

type Survey = [Char]
type Group = [Survey]
type Input = [Group]

getGroupTotal :: [Survey] -> Sum Int
getGroupTotal g = Sum . length $ foldr intersect (mconcat g) g

solvePartTwo :: Input -> IO ()
solvePartTwo = print . mconcat . fmap getGroupTotal

solvePartOne :: Input -> IO ()
solvePartOne = print . foldMap (Sum . length . nub . mconcat)

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print err

surveyParser :: Parser Survey
surveyParser = P.many1 $ P.noneOf "\n"

groupParser :: Parser Group
groupParser = do
  x <- surveyParser
  xs <- P.try (P.newline >> P.newline >> return []) 
    P.<|> (P.newline >> groupParser)
    P.<|> return []
  return (x : xs)

inputParser :: Parser Input
inputParser = do
  x <- P.many groupParser
  P.eof
  return x
