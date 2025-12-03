module Day3 where

import Text.Parsec

day3part1 :: String -> IO ()
day3part1 file = do
  input <- readFile file
  let result = fmap solve $ parseInput input
  either print print result

number :: Parsec String () Int
number = parsecMap (read . (:[])) digit

line :: Parsec String () [Int]
line = many1 number <* char '\n'

parseFile :: Parsec String () [[Int]]
parseFile = many1 line <* eof

parseInput :: String -> Either ParseError [[Int]]
parseInput input = parse parseFile "" input

lineJoltage :: [Int] -> Int
lineJoltage line = 
  let
    tens = maximum $ init line
    rest = tail $ dropWhile (/= tens) line
    units = maximum rest
  in
    10 * tens + units

solve :: [[Int]] -> Int
solve = sum . (map lineJoltage)
