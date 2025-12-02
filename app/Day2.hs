module Day2 where

import Text.Parsec
import Data.List

day2part1 :: String -> IO ()
day2part1 file = do
  input <- readFile file
  let result = fmap (show . solve) $ parseInput input
  either print putStrLn result

day2part2 :: String -> IO ()
day2part2 file = do
  input <- readFile file
  let result = fmap (show . solve2) $ parseInput input
  either print putStrLn result

number = parsecMap read (many digit)

range :: Parsec String () (Int, Int)
range = do
  list <- number `sepBy1` (char '-')
  case list of
    [a, b] -> return (a, b)
    _ -> parserFail "Not a pair"

rangeList :: Parsec String () [(Int, Int)]
rangeList = range `sepBy` char ',' <* optional (char '\n') <* eof

parseInput :: String -> Either ParseError [(Int, Int)]
parseInput input = parse rangeList "" input

solve :: [(Int, Int)] -> Int
solve = sum . (>>= sillyNumbers)

solve2 :: [(Int, Int)] -> Int
solve2 = sum . (>>= sillyNumbers2)

sillyNumbers :: (Int, Int) -> [Int]
sillyNumbers (a, b) = filter isSillyNumber [a .. b]

sillyNumbers2 :: (Int, Int) -> [Int]
sillyNumbers2 (a, b) = filter isSillyNumber2 [a .. b]

isSillyNumber :: Int -> Bool
isSillyNumber n = a == b
  where
    a = n `div` (10 ^ (m `div` 2))
    b = n `mod` (10 ^ (m `div` 2))
    m = magnitude 0 n

magnitude :: Int -> Int -> Int
magnitude m n = if n `div` (10 ^ m) == 0 then m else magnitude (m + 1) n

isSillyNumber2 :: Int -> Bool
isSillyNumber2 n = detectRepetition $ figures n

figures :: Int -> [Int]
figures 0 = []
figures n = (n `mod` 10) : figures (n `div` 10)

detectRepetition :: [Int] -> Bool
detectRepetition [] = False
detectRepetition [x] = False
detectRepetition (x:xs) = detect [x] xs

detect pattern list | length pattern > length list = False
detect pattern (list@(x:xs)) = if fit [] pattern list then True else detect (pattern ++ [x]) xs

fit acc pattern list | acc == list = True
fit acc pattern list | length acc >= length list = False
fit acc pattern list = fit (acc ++ pattern) pattern list
