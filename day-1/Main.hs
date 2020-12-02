{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack)

findMatchPartTwo :: Int -> [Int] -> Maybe Int
findMatchPartTwo total (x : xs) =
  case (* x) <$> findMatchPartOne (total - x) xs of
    Just m -> Just m
    Nothing -> findMatchPartTwo total xs
findMatchPartTwo _ [] = Nothing

findMatchPartOne :: Int -> [Int] -> Maybe Int
findMatchPartOne total (x : xs) =
  fromMaybe (findMatchPartOne total xs) $
    ((!!? 0) . filter isJust) $
      (\val -> if x + val == total then Just (x * val) else Nothing)
        <$> xs
findMatchPartOne _ [] = Nothing

dayOnePartTwo :: [Int] -> IO ()
dayOnePartTwo inputs =
  putStrLn $ maybe "Could not find match!" show (findMatchPartTwo 2020 inputs)

dayOnePartOne :: [Int] -> IO ()
dayOnePartOne inputs =
  putStrLn $ maybe "Could not find match!" show (findMatchPartOne 2020 inputs)

parseInput :: Text -> Maybe [Int]
parseInput =
  mapM (readMaybe . unpack)
    . splitOn "\n"

getInput :: IO (Maybe [Int])
getInput =
  fmap parseInput $ pack <$> readFile "./input.txt"

main :: IO ()
main = do
  _ <- putStrLn "Part (1) or (2)?"
  day <- getLine
  input <- getInput
  case (day, input) of
    ("1", Just parsedInput) -> dayOnePartOne parsedInput
    ("2", Just parsedInput) -> dayOnePartTwo parsedInput
    (_, Nothing) -> putStrLn "Failed to properly parse puzzle input"
    _ -> putStrLn "Invalid: Enter part '1' or '2'"