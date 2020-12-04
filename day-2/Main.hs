{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec as P

data InputLine = InputLine
  { range :: (Int, Int),
    charRequirement :: Char,
    content :: String
  }
  deriving (Show)

countCharacters :: Char -> [Char] -> Int
countCharacters _ [] = 0
countCharacters c (x : xs) = countCharacters c xs + if x == c then 1 else 0

countPositions :: Char -> [Char] -> [Int] -> Int
countPositions _ _ [] = 0
countPositions char str (pos : next) =
  countPositions char str next + if maybe False (== char) (str !!? (pos - 1)) then 1 else 0

validateInput :: InputLine -> Bool
validateInput input =
  let c = countCharacters (charRequirement input) (content input)
      range_ = range input
   in and [c >= fst range_, c <= snd range_]

validateInputPartTwo :: InputLine -> Bool
validateInputPartTwo input =
  let range_ = range input
      c = countPositions (charRequirement input) (content input) [fst range_, snd range_]
   in c == 1

solvePartOne :: [InputLine] -> IO ()
solvePartOne =
  print . show . length . filter id . fmap validateInput

solvePartTwo :: [InputLine] -> IO ()
solvePartTwo =
  print . show . length . filter id . fmap validateInputPartTwo

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  putStrLn "Part (1) or (2)?"
  dayNum <- getLine
  case (dayNum, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> putStrLn $ "Failed to parse input: " <> show err

lineParser :: Parser InputLine
lineParser = do
  range_ <-
    ( do
        min_ <- P.many P.digit >>= maybe (fail "Failed to parse int") pure . readMaybe
        P.char '-'
        max_ <- P.many P.digit >>= maybe (fail "Failed to parse int") pure . readMaybe
        return (min_, max_)
      )
  P.spaces
  charRequirement_ <- P.noneOf ":"
  P.many $ P.oneOf " :"
  content_ <- P.many1 $ P.noneOf "\n"
  return $ InputLine range_ charRequirement_ content_

inputParser :: Parser [InputLine]
inputParser = do
  x <- lineParser
  xs <- (P.char '\n' >> inputParser) P.<|> return []
  return (x : xs)