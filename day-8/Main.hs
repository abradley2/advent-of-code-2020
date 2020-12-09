{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Text.ParserCombinators.Parsec as P

data Program = Program {total :: Int, visited :: [Int], lastToggle :: Int} deriving (Show)

type InputLine = (String, Int)

data Zipper a = Zipper {current :: (Int, a), whole :: [a]} deriving (Show)

setItem :: Int -> a -> [a] -> [a]
setItem idx item list =
  let (front, back) = splitAt idx list
   in front <> [item] <> drop 1 back

nextToggle :: Program -> Zipper InputLine -> Maybe (Program, Zipper InputLine)
nextToggle program zipper =
  let lastToggle_ = lastToggle program
      nProgram = program {lastToggle = lastToggle_ + 1, total = 0, visited = []}
   in case whole zipper !!? lastToggle_ of
        Just (instruction, value) ->
          case instruction of
            "nop" -> Just (nProgram, zipper {whole = setItem lastToggle_ ("jmp", value) (whole zipper)})
            "jmp" -> Just (nProgram, zipper {whole = setItem lastToggle_ ("nop", value) (whole zipper)})
            _ -> nextToggle nProgram zipper
        Nothing ->
          Nothing

zipRight :: Int -> Zipper a -> Maybe (Zipper a)
zipRight delta z = createZipper (whole z) (fst (current z) + delta)

createZipper :: [a] -> Int -> Maybe (Zipper a)
createZipper l idx =
  let (left_, right_) = splitAt idx l
   in (\a -> Zipper (idx, a) l) <$> l !!? idx


runInstruction :: Program -> Zipper InputLine -> (String, Int) -> Maybe (Program, Zipper InputLine)
runInstruction program zipper (instruction, value) =
  case instruction of
    "nop" -> (program,) <$> zipRight 1 zipper
    "acc" -> (program {total = total program + value},) <$> zipRight 1 zipper
    "jmp" -> (program,) <$> zipRight value zipper

lineValue :: InputLine -> Int
lineValue (instruction, value) =
  case instruction of
    "acc" -> value
    _ -> 0

runPartTwo :: [InputLine] -> Program -> Zipper InputLine -> IO ()
runPartTwo input program zipper =
  let current_ = current zipper
      (idx, instruction) = current_
      nProgram = program {visited = idx : visited program}
   in if idx `elem` visited program
        then
          maybe
            (print "cannot toggle to next instruction")
            (uncurry $ runPartTwo input)
            (createZipper input 0 >>= nextToggle program)
        else
          maybe
            ((print $ nProgram { total = total nProgram + lineValue instruction }) >> print zipper)
            (uncurry $ runPartTwo input)
            (runInstruction nProgram zipper instruction)

runPartOne :: Program -> Zipper InputLine -> IO ()
runPartOne program zipper =
  let current_ = current zipper
      (idx, instruction) = current_
      nProgram = program {visited = idx : visited program}
   in if idx `elem` visited program
        then print program
        else
          maybe
            (putStrLn $ mconcat ["Failed to evaluate instruction at ", show idx])
            (uncurry runPartOne)
            (runInstruction nProgram zipper instruction)

solvePartTwo :: [InputLine] -> IO ()
solvePartTwo input =
  maybe
    (putStrLn "Could not create zipper")
    (runPartTwo input (Program 0 [] 0))
    (createZipper input 0)

solvePartOne :: [InputLine] -> IO ()
solvePartOne input =
  maybe
    (putStrLn "Could not create zipper")
    (runPartOne (Program 0 [] 0))
    (createZipper input 0)

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse parseInput "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print err

parseInput :: Parser [InputLine]
parseInput = do
  x <- do
    command <- P.many1 P.alphaNum
    P.space
    modifier <- P.oneOf "-+"
    value <- P.many1 P.alphaNum >>= maybe (fail "Not a valid Int") pure . readMaybe
    return (command, value * (if modifier == '-' then -1 else 1))
  xs <- (P.eof >> return []) P.<|> (P.newline >> parseInput)
  return (x : xs)