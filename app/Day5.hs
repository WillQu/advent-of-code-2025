module Day5 where

import Text.Parsec
import Data.List
import Data.Maybe

data Input = Input { ranges :: [(Int, Int)], ingredients :: [Int] }

day5part1 :: String -> IO ()
day5part1 file = do
  input <- readFile file
  let result = fmap solve $ parseInput input
  either print print result

day5part2 :: String -> IO ()
day5part2 file = do
  input <- readFile file
  let result = fmap solve2 $ parseInput input
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

solve2 :: Input -> Int
solve2 = sum . (map numRange) . buildRanges . ranges

numRange :: (Int, Int) -> Int
numRange (a, b) = b - a + 1

buildRanges :: [(Int, Int)] -> [(Int, Int)]
buildRanges [] = []
buildRanges (x:xs) = maybe skip combine $ find (overlap x) xs
  where
    skip = x:buildRanges xs
    combine y = buildRanges $ (combineRanges x y) ++ (delete y xs)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (a, b) (c, d) = (c >= a && c <= b) || (d >= a && d <= b) || (a >= c && a <= d) || (b >= c && b <= d)

combineRanges :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
combineRanges (a, b) (c, d) = [(minimum [a, b, c, d], maximum [a, b, c, d])]
