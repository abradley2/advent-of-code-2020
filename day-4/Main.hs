{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec as P
import Data.List (nub)

type PassportField = (String, String)

type Passport = [PassportField]

hexParser :: Parser String
hexParser = do
  x <- P.char '#'
  xs <-
    P.many (P.oneOf "0123456789abcdef") >>= \str ->
      if length str == 6 then pure str else fail "Hex must be len = 6"
  P.eof
  return $ x : xs

heightParser :: Parser (Int, String)
heightParser = do
  h <- P.many1 P.digit >>= maybe (fail "not a digit") pure . readMaybe
  u <- P.many1 P.anyChar
  P.eof
  case (h, u) of
    (h, "in") -> if h >= 59 && h <= 76 then pure (h, u) else fail "Invalid in height"
    (h, "cm") -> if h >= 150 && h <= 193 then pure (h, u) else fail "Invalid cm height"
    _ -> fail "Invalid units"

pidParser :: Parser String
pidParser = do
  r <- P.many1 P.digit >>= \v ->
    if length v == 9 then pure v else fail "Pid is incorrect length"
  P.eof
  return r

validField :: PassportField -> Bool
validField ("byr", val) = maybe False (\v -> v >= 1920 && v <= 2002) (readMaybe val)
validField ("iyr", val) = maybe False (\v -> v >= 2010 && v <= 2020) (readMaybe val)
validField ("eyr", val) = maybe False (\v -> v >= 2020 && v <= 2030) (readMaybe val)
validField ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField ("hgt", val) = isRight $ P.parse heightParser "height" val
validField ("hcl", val) = isRight $ P.parse hexParser "hair color" val
validField ("pid", val) = isRight $ P.parse pidParser "pid" val
validField _ = True

validPartOne :: Passport -> Bool
validPartOne =
  (== 7) . length . filter (`elem` ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) . nub . fmap fst

solvePartOne :: [Passport] -> IO ()
solvePartOne =
  print . length . filter id . fmap validPartOne

solvePartTwo :: [Passport] -> IO ()
solvePartTwo =
  solvePartOne . filter (all (== True) . fmap validField)

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Left err) -> print err
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"

passportFieldParser :: Parser PassportField
passportFieldParser = do
  foo <- P.many1 $ P.noneOf ":"
  P.char ':'
  bar <- P.many1 $ P.noneOf "\n "
  return (foo, bar)

passportParser :: Parser Passport
passportParser = do
  x <- passportFieldParser
  xs <-
    (P.eof >> return [])
      P.<|> (P.try (P.newline >> P.newline) >> return [])
      P.<|> ((P.newline P.<|> P.char ' ') >> passportParser)
  return (x : xs)

inputParser :: Parser [Passport]
inputParser = do
  passports <- P.many1 passportParser
  P.eof
  return passports
