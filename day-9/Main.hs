{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List (maximum, minimum)
import Text.ParserCombinators.Parsec as P

type Input = [Sum Int]

data Program = Program
  { tailVals :: [Sum Int],
    nextVals :: [Sum Int]
  }

trim :: Int -> [a] -> [a]
trim m l = drop (length l - m) l

checkSum :: Sum Int -> [Sum Int] -> Maybe ()
checkSum _ [] = Nothing
checkSum total (x : xs) = if total `elem` (mappend x <$> xs) then Just () else checkSum total xs

solvePartOne :: ([Sum Int] -> [Sum Int]) -> Program -> Maybe Int
solvePartOne trimmer program =
  let (tailVals_, nextVals_) = (tailVals program, nextVals program)
      nTailVals = trimmer (tailVals_ <> take 1 nextVals_)
      nNextVals = drop 1 nextVals_
      next = solvePartOne trimmer (Program nTailVals nNextVals)
   in case (length nTailVals, nNextVals !!? 0) of
        (_, Nothing) -> Nothing
        (25, Just val) ->
          maybe
            (Just $ getSum val)
            (const next)
            (checkSum val nTailVals)
        (_, Just _) -> next

solvePartTwo :: Input -> Sum Int -> Int -> [Sum Int] -> [Sum Int] -> Maybe (Sum Int)
solvePartTwo input total currentIndex _ [] = do
  let nextInput = drop currentIndex input
  preamble <- (: []) <$> nextInput !!? 0
  let nextVals = drop 1 nextInput
   in solvePartTwo input total currentIndex nextVals preamble
solvePartTwo input total currentIndex (x : xs) preamble =
  let newPreamble = (x : preamble)
      newVal = mconcat newPreamble
      nextIndex = currentIndex + 1
   in if newVal == total
        then Just $ mconcat [List.minimum preamble, List.maximum preamble]
        else
          ( if newVal > total
              then solvePartTwo input total nextIndex xs []
              else solvePartTwo input total currentIndex xs newPreamble
          )

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) ->
      maybe
        (putStrLn "Failed to find value")
        print
        (solvePartOne (trim 25) $ Program [] input)
    ("2", Right input) ->
      maybe (print "Failed") print $
        solvePartOne (trim 25) (Program [] input) >>= (\total -> solvePartTwo input (Sum total) 0 input [])
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print err

inputParser :: Parser Input
inputParser = do
  x <- P.many P.digit >>= maybe (fail "Could not parse int") (pure . Sum) . readMaybe
  xs <- (P.newline >> inputParser) P.<|> (P.eof >> return [])
  return (x : xs)