{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T (pack, unpack)
import Text.ParserCombinators.Parsec as Parsec

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
countPositions char str (idx : next) =
  countPositions char str next + if maybe False (== char) (str !!? idx) then 1 else 0

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
  rawInput <- T.pack <$> readFile "./input.txt"
  putStrLn "Part (1) or (2)?"
  dayNum <- getLine
  case (dayNum, Parsec.parse inputParser "(unknown)" (T.unpack rawInput)) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> putStrLn $ "Failed to parse input: " <> show err

lineParser :: Parser InputLine
lineParser = do
  range_ <-
    ( do
        min_ <- Parsec.many Parsec.digit >>= maybe (fail "Failed to parse int") pure . readMaybe
        Parsec.char '-'
        max_ <- Parsec.many Parsec.digit >>= maybe (fail "Failed to parse int") pure . readMaybe
        return (min_, max_)
      )
  Parsec.spaces
  charRequirement_ <- Parsec.noneOf ":"
  Parsec.many $ Parsec.oneOf " :"
  content_ <- Parsec.many1 $ Parsec.noneOf "\n"
  return $ InputLine range_ charRequirement_ content_
-- hi
inputParser :: Parser [InputLine]
inputParser = do
  x <- lineParser
  xs <- (Parsec.char '\n' >> inputParser) Parsec.<|> return []
  return (x : xs)