module Day5 where

import Text.Parsec
import Data.List

data Input = Input { ranges :: [(Int, Int)], ingredients :: [Int] }

day5part1 :: String -> IO ()
day5part1 file = do
  input <- readFile file
  let result = fmap solve $ parseInput input
  either print print result

number = parsecMap read (many digit)

range :: Parsec String () (Int, Int)
range = do
  list <- number `sepBy1` (char '-')
  case list of
    [a, b] -> return (a, b)
    _ -> parserFail "Not a pair"

rangeList :: Parsec String () [(Int, Int)]
rangeList = range `endBy1` char '\n' <* char '\n'

ingredientsParser :: Parsec String () [Int]
ingredientsParser = number `endBy1` char '\n'

inputParser :: Parsec String () Input
inputParser =
  do
    r <- rangeList
    i <- ingredientsParser
    _ <- eof
    return $ Input r i

parseInput :: String -> Either ParseError Input
parseInput input = parse inputParser "" input

solve :: Input -> Int
solve input = length $ filter (fresh $ ranges input) $ ingredients input

fresh :: [(Int, Int)] -> Int -> Bool
fresh ranges x = any inRange ranges
  where
    inRange (a, b) = x >= a && x <= b
