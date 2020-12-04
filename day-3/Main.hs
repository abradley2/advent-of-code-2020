{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec as P

data Slope = Tree | Snow deriving (Show)

type InputLines = [[Slope]]

traverseSlope :: (Int, Int) -> (Int, Int) -> [[Slope]] -> Int
traverseSlope (xPos, yPos) (xDelta, yDelta) slopes =
  let currentNode = slopes !!? yPos >>= (!!? xPos)
      nextPos = (xPos + xDelta, yPos + yDelta)
   in case currentNode of
        Just Tree -> 1 + traverseSlope nextPos (xDelta, yDelta) slopes
        Just Snow -> traverseSlope nextPos (xDelta, yDelta) slopes
        Nothing -> 0

solvePartOne :: [[Slope]] -> IO ()
solvePartOne = print . show . traverseSlope (0, 0) (3, 1)

solvePartTwo :: [[Slope]] -> IO ()
solvePartTwo slopes =
  print . show $ foldr (\deltas total ->
    total * traverseSlope (0, 0) deltas slopes  
  )
  1
  [ (1, 1)
  , (3, 1)
  , (5, 1)
  , (7, 1)
  , (1, 2)
  ]

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2) ?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print $ show err

parseInputLineChar :: Parser Slope
parseInputLineChar =
  P.oneOf "#."
    >>= \case
      '#' -> pure Tree
      '.' -> pure Snow
      _ -> fail "Invalid character found"

parseInputLines :: Parser [[Slope]]
parseInputLines = do
  x <- P.many parseInputLineChar >>= pure . cycle
  xs <- (P.char '\n' >> parseInputLines) P.<|> return []
  return (x : xs)

inputParser :: Parser InputLines
inputParser = do
  x <- parseInputLines
  P.eof
  return x