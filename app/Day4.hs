module Day4 where

import Text.Parsec
import Data.Functor
import Control.Monad
import Control.Lens
import Control.Lens.At

day4part1 :: String -> IO ()
day4part1 = printSolution solve

day4part2 :: String -> IO ()
day4part2 = printSolution solve2

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
    guard $ get rolls x y
    return $ neighbors rolls x y

solve2 :: [[Bool]] -> Int
solve2 rolls =
  let
    reachable = filter (\(x, y) -> neighbors rolls x y < 4) $ do
      let h = length rolls
      let l = length $ head rolls
      y <- [0 .. h - 1]
      x <- [0 .. l - 1]
      guard $ get rolls x y
      return (x, y)
    num = length reachable
    updated = foldr (\(x, y) r -> remove r x y) rolls reachable
  in
    if num == 0 then 0 else num + solve2 updated

neighbors :: [[Bool]] -> Int -> Int -> Int
neighbors rolls x y = length $ filter id $
  [ get rolls (x - 1) (y - 1)
  , get rolls x (y - 1)
  , get rolls (x + 1) (y - 1)
  , get rolls (x - 1) y
  , get rolls (x + 1) y
  , get rolls (x - 1) (y + 1)
  , get rolls x (y + 1)
  , get rolls (x + 1) (y + 1)
  ]

get :: [[Bool]] -> Int -> Int -> Bool
get rolls x y = if x < 0 || y < 0 || x >= length (head rolls) || y >= length rolls then False else rolls !! y !! x

remove :: [[Bool]] -> Int -> Int -> [[Bool]]
remove rolls x y = rolls & ix y . ix x .~ False
