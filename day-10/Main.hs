{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Text.ParserCombinators.Parsec as P

type Input = [Int]

data ProcessTree = ProcessTree
  { processed :: [Int],
    nextProcessed :: Either Bool [ProcessTree]
  }
  deriving (Show)

checkPossible :: Int -> Int -> [Int] -> Bool
checkPossible maxJolt maxApplied items = maxApplied + (length comparableItems * 3) >= maxJolt
  where
    comparableItems = filter (> maxApplied) items

countTree :: ProcessTree -> Int
countTree tree =
  case nextProcessed tree of
    (Left True) -> 1
    (Left False) -> 0
    (Right children) -> getSum $ mconcat (fmap (Sum . countTree) children)

maximum :: Ord a => NonEmpty a -> a
maximum ne = foldr max (head ne) ne

extractItem :: Eq a => a -> [a] -> Maybe (a, [a])
extractItem predicate =
  (\(match, list) -> (,list) <$> match)
    . foldr
      ( \cur (found, rest) ->
          if cur == predicate then (Just cur, rest) else (found, cur : rest)
      )
      (Nothing, [])

getConnectors :: (Eq a, Num a) => a -> [a] -> [(a, [a])]
getConnectors prevValue inputs =
  foldr (\cur acc -> maybe acc (: acc) cur) [] $ (`extractItem` inputs) . (+) prevValue <$> [1, 2, 3]

getConnector :: Int -> Int -> [Int] -> Maybe (Int, [Int])
getConnector prevValue inc inputs =
  if inc > 3
    then Nothing
    else case extractItem (prevValue + inc) inputs of
      Just result -> Just result
      Nothing -> getConnector prevValue (inc + 1) inputs

evaluateProcessed :: [Int] -> (Int, Int) -> Int
evaluateProcessed [_] (delta1, delta3) = delta1 * delta3
evaluateProcessed (x : y : xs) (delta1, delta3) =
  case abs $ x - y of
    1 -> evaluateProcessed (y : xs) (delta1 + 1, delta3)
    3 -> evaluateProcessed (y : xs) (delta1, delta3 + 1)
    _ -> evaluateProcessed (y : xs) (delta1, delta3)

solvePartOne :: Input -> [Int] -> Int -> IO ()
solvePartOne [] processed maxJolt = print $ evaluateProcessed (maxJolt : processed) (0, 0)
solvePartOne inputs processed maxJolt =
  let processedMax = maybe 0 maximum (nonEmpty processed)
   in case getConnector processedMax 1 inputs of
        Nothing -> print "Failed to process next connector"
        Just (nextProcessed, nextInputs) -> solvePartOne nextInputs (nextProcessed : processed) maxJolt

solvePartTwo :: Input -> [Int] -> Int -> ProcessTree
solvePartTwo inputs processed maxJolt =
  let processedMax = maybe 0 maximum (nonEmpty processed)
      next = getConnectors processedMax inputs
      remaining =
        ( \(nextProcessed, nextInputs) -> solvePartTwo nextInputs (nextProcessed : processed) maxJolt
        )
          <$> filter (uncurry (checkPossible maxJolt)) next
   in ProcessTree
        { processed = processed,
          nextProcessed =
            if null remaining
              then Left $ maybe False ((== maxJolt) . (3 +)) (processed !!? 0)
              else Right remaining
        }

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right (maxJolt, input)) -> solvePartOne input [0] maxJolt
    ("2", Right (maxJolt, input)) -> print $ countTree $ solvePartTwo input [0] maxJolt
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print err

inputParser :: P.Parser (Int, [Int])
inputParser = do
  x <- P.many P.digit >>= maybe (fail "Invalid int found") pure . readMaybe
  xs <- (P.newline >> snd <$> inputParser) <|> return []
  P.eof
  return ((3 +) . maximum $ x :| xs, x : xs)