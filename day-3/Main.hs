{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec as P

data Slope = Tree | Snow | End deriving (Show)

type InputLines = [[Slope]]

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2) ?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> print $ show input
    ("2", Right _) -> putStrLn "Not implemented"
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print $ show err

parseInputLineChar :: Parser Slope
parseInputLineChar =
  P.oneOf "#."
    >>= ( \case
            '#' -> pure Tree
            '.' -> pure Snow
            _ -> fail "Invalid character found"
        )

parseInputLine :: Parser [Slope]
parseInputLine = do
  x <- parseInputLineChar
  xs <- (P.char '\n' >> parseInputLine) P.<|> return []
  return (x : xs)

inputParser :: Parser InputLines
inputParser = do
  x <- P.many parseInputLine
  P.eof
  return (x <> [repeat End])