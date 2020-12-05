{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Text.ParserCombinators.Parsec as P

type SeatingChart = [(Int, [Int])]

splitLeft :: [a] -> [a]
splitLeft l =
  fst $ splitAt (floor $ len / 2) l
  where
    len = fromIntegral $ length l :: Float

splitRight :: [a] -> [a]
splitRight l =
  snd $ splitAt (floor $ len / 2) l
  where
    len = fromIntegral $ length l :: Float

seats :: SeatingChart
seats = (,[0 .. 7]) <$> [0 .. 127]

data Partition = F | B | R | L deriving (Read, Show)

getSeat :: SeatingChart -> [Partition] -> Either String Int
getSeat seats [] =
  let chosen =
        seats !!? 0
          >>= \(rowNum, cols) -> (+ (rowNum * 8)) <$> cols !!? 0
   in maybeToRight "Could not find seat" chosen
getSeat seats (x : xs) =
  let nextSeats = case x of
        F -> splitLeft seats
        B -> splitRight seats
        R -> second splitRight <$> seats
        L -> second splitLeft <$> seats
   in getSeat nextSeats xs

solvePartOne :: [[Partition]] -> IO ()
solvePartOne =
  print . fmap (foldr (\x y -> if x > y then x else y) 0) . sequence . fmap (getSeat seats)

findMatches :: [Int] -> [Int]
findMatches seatIds =
  (+1) <$> filter
    ( \cur -> elem (cur + 2) seatIds && (not $ elem (cur + 1) seatIds))
    seatIds

solvePartTwo :: [[Partition]] -> IO ()
solvePartTwo input =
  let seatIds = sequence $ fmap (getSeat seats) input
   in putStrLn $ show $ findMatches <$> rightToMaybe seatIds

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  putStrLn "Day (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Left err) -> print err
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"

rowParser :: Parser [Partition]
rowParser = do
  x <-
    P.oneOf "FBRL"
      >>= maybe (fail "Unexpected character not belonging to FBRL") pure . read
  xs <- rowParser P.<|> pure []
  return (x : xs)
  where
    read :: Char -> Maybe Partition
    read = readMaybe . (: [])

inputParser :: Parser [[Partition]]
inputParser = do
  x <- rowParser
  xs <- (P.eof >> return []) P.<|> (P.newline >> inputParser)
  return (x : xs)