module Day3 where

import Text.Parsec
import Data.List

day3part1 :: String -> IO ()
day3part1 = printSolution solve

day3part2 :: String -> IO ()
day3part2 = printSolution solve2

printSolution :: ([[Int]] -> Int) -> String -> IO ()
printSolution f file = do
  input <- readFile file
  let result = fmap f $ parseInput input
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

lineJoltage2 :: Int -> [Int] -> Int
lineJoltage2 0 _ = 0
lineJoltage2 n line = 
  let
    m = maximum $ drop (n - 1) $ reverse line
    rest = tail $ dropWhile (/= m) line
  in
    (m * 10^(n-1)) + lineJoltage2 (n - 1) rest

solve :: [[Int]] -> Int
solve = sum . (map lineJoltage)

solve2 :: [[Int]] -> Int
solve2 = sum . (map (lineJoltage2 12))
