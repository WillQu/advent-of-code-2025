module Day4 where

import Text.Parsec
import Data.Functor
import Control.Monad

day4part1 :: String -> IO ()
day4part1 = printSolution solve

printSolution :: ([[Bool]] -> Int) -> String -> IO ()
printSolution f file = do
  input <- readFile file
  let result = fmap f $ parseInput input
  either print print result

roll :: Parsec String () Bool
roll = (char '@' $> True) <|> (char '.' $> False)

line :: Parsec String () [Bool]
line = many1 roll <* char '\n'

parseFile :: Parsec String () [[Bool]]
parseFile = many1 line <* eof

parseInput :: String -> Either ParseError [[Bool]]
parseInput input = parse parseFile "" input

solve :: [[Bool]] -> Int
solve rolls = length $ filter (< 4) $ do
    let h = length rolls
    let l = length $ head rolls
    y <- [0 .. h - 1]
    x <- [0 .. l - 1]
    guard $ at rolls x y
    return $ neighbors rolls x y

neighbors :: [[Bool]] -> Int -> Int -> Int
neighbors rolls x y = length $ filter id $
  [ at rolls (x - 1) (y - 1)
  , at rolls x (y - 1)
  , at rolls (x + 1) (y - 1)
  , at rolls (x - 1) y
  , at rolls (x + 1) y
  , at rolls (x - 1) (y + 1)
  , at rolls x (y + 1)
  , at rolls (x + 1) (y + 1)
  ]

at :: [[Bool]] -> Int -> Int -> Bool
at rolls x y = if x < 0 || y < 0 || x >= length (head rolls) || y >= length rolls then False else rolls !! y !! x
