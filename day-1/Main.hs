{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.Text (pack, splitOn, unpack)

findMatchPartTwo :: [Int] -> Maybe Int
findMatchPartTwo list =
  indexedFoldr
    ( \(value, idx) match ->
        case match of
          Just m -> Just m
          Nothing ->
            ( \siblingList ->
                indexedFoldr
                  ( \(_, siblingIdx) match ->
                      case match of
                        Just m -> Just m
                        Nothing ->
                          (* value) <$> findMatchPartOne (getSiblings siblingIdx siblingList) (2020 - value)
                  )
                  Nothing
                  siblingList
            )
              $ getSiblings idx list -- don't compare the value to itself
    )
    Nothing
    list

findMatchPartOne :: [Int] -> Int -> Maybe Int
findMatchPartOne list total =
  indexedFoldr
    ( \(value, idx) match ->
        case match of
          Just m -> Just m
          Nothing ->
            join $
              ((!!? 0) . filter isJust) $ -- then take the first match
                (\v -> if v + value == total then Just (v * value) else Nothing) -- evaluate if we add up to 2020
                  <$> getSiblings idx list -- don't compare the value to itself
    )
    Nothing
    list

getSiblings :: Int -> [a] -> [a]
getSiblings index =
  (\(x, xs) -> x <> drop 1 xs) . splitAt index

indexedFoldr :: ((a, Int) -> b -> b) -> b -> [a] -> b
indexedFoldr fn a =
  fst
    . foldr
      (\cur (acc, idx) -> (fn (cur, idx) acc, idx + 1))
      (a, 0)

dayOnePartTwo :: [Int] -> IO ()
dayOnePartTwo inputs =
  putStrLn $ maybe "Could not find match!" show (findMatchPartTwo inputs)

dayOnePartOne :: [Int] -> IO ()
dayOnePartOne inputs =
  putStrLn $ maybe "Could not find match!" show (findMatchPartOne inputs 2020)

parseInput :: Text -> Maybe [Int]
parseInput =
  mapM (readMaybe . unpack)
    . splitOn "\n"

getInput :: IO (Maybe [Int])
getInput =
  liftM parseInput $ pack <$> readFile "./input.txt"

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