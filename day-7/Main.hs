{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Graph as Graph
import Text.ParserCombinators.Parsec as P

sumEdges :: InputGraph -> [InputEdge] -> Maybe (Sum Int)
sumEdges input edges =
  mconcat <$>
    mapM
      ( \(key_, count_) ->
          Sum . (*) count_ . getSum <$> countNodes input key_
      )
      edges

countNodes :: InputGraph -> String -> Maybe (Sum Int)
countNodes input k = do
  (node_, _, _) <- find ((== k) . \(_, nodeKey, _) -> nodeKey) input
  let count = mconcat $ Sum . snd <$> edges node_
  childCount <- sumEdges input (edges node_)
  return (count <> childCount)

solvePartTwo :: InputGraph -> String -> IO ()
solvePartTwo input k = maybe (putStrLn "Failed to count nodes") print (countNodes input k)

solvePartOne :: InputGraph -> IO ()
solvePartOne input =
  let (graph, _, vertexFromKey) = Graph.graphFromEdges input
   in case vertexFromKey "shiny_gold" of
        Nothing ->
          putStrLn "Graph wasn't built with expected target"
        Just shinyGold ->
          print $ flip (-) 1 $ length $ filter (flip (Graph.path graph) shinyGold) (Graph.vertices graph)

main :: IO ()
main = do
  rawInput <- readFile "./input.txt"
  _ <- putStrLn "Part (1) or (2)?"
  part <- getLine
  case (part, P.parse inputParser "input.txt" rawInput) of
    ("1", Right input) -> solvePartOne input
    ("2", Right input) -> solvePartTwo input "shiny_gold"
    (_, Right _) -> putStrLn "Invalid part, enter '1' or '2'"
    (_, Left err) -> print err

type Count = Int

type Key = String

data InputNode = InputNode {key :: Key, edges :: [InputEdge]} deriving (Show)

type InputEdge = (Key, Count)

type InputLine = (InputNode, Key, [Key])

type InputGraph = [(InputNode, Key, [Key])]

joinAdjectives :: [String] -> String
joinAdjectives = mconcat . intersperse "_"

bagKW :: Parser ()
bagKW = P.try $ do
  P.string "bag"
  P.char 's' P.<|> pure 's'
  P.notFollowedBy P.alphaNum

noOtherKW :: Parser ()
noOtherKW = P.try $ do
  P.string "no other"
  P.notFollowedBy P.alphaNum
  void $ P.space
  bagKW
  void $ P.char '.'

containKW :: Parser ()
containKW = P.try $ do
  P.string "contain"
  P.notFollowedBy P.alphaNum

edgeParser :: Parser InputEdge
edgeParser = do
  c <- P.many P.digit >>= maybe (fail "Not an int") pure . readMaybe
  P.space
  a <- fmap joinAdjectives adjectivesParser
  return $ (a, c)

edgesParser :: Parser [InputEdge]
edgesParser = do
  x <- edgeParser
  xs <- (P.string ", " >> edgesParser) P.<|> (P.char '.' >> return [])
  return (x : xs)

adjectivesParser :: Parser [String]
adjectivesParser = do
  x <- P.many1 $ P.noneOf " "
  P.char ' '
  xs <- (bagKW >> return []) P.<|> adjectivesParser
  return (x : xs)

nodeParser :: Parser InputLine
nodeParser = do
  a <- fmap joinAdjectives adjectivesParser
  P.space
  containKW
  P.space
  e <- (noOtherKW >> return []) P.<|> edgesParser
  void P.newline P.<|> P.eof
  return (InputNode a e, a, fst <$> e)

inputParser :: Parser InputGraph
inputParser = P.many nodeParser