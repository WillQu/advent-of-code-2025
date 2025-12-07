module Day7 where

import Control.Lens
import Control.Lens.At
import Control.Lens.Tuple
import Control.Monad
import Data.List
import Data.Maybe

day7part1 :: String -> IO ()
day7part1 file = do
  input <- readFile file
  let result = solve $ lines input
  print result

step :: [Char] -> [Char] -> ([Char], Int)
step previous next = foldr splitTachyon (next, 0) $ enumerate previous
  where
    splitTachyon (i, c) (accLine, accResult) =
      case c of
        '.' -> (accLine, accResult)
        '^' -> (accLine, accResult)
        '|' -> case accLine !! i of
          '^' -> ((accLine & ix (i - 1::Int) .~ '|') & ix (i + 1::Int) .~ '|', accResult + 1)
          _ -> (accLine & ix i .~ '|', accResult)
        'S' -> (accLine & ix i .~ '|', accResult)

solve :: [[Char]] -> Int
solve (x:xs) = snd $ foldl s (x, 0) xs
  where
    s (x, i) line = (step x line) & _2 +~ i

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

---

data Cell = Ray Int | Splitter

add :: Int -> Cell -> Cell
add y (Ray x) = Ray (x + y)
add y Splitter = Ray y

day7part2 :: String -> IO ()
day7part2 file = do
  input <- readFile file
  let result = solve2 . parse $ lines input
  print result

parse :: [[Char]] -> [[Cell]]
parse = map (map r)
  where
    r c = case c of
      '^' -> Splitter
      'S' -> Ray 1
      '.' -> Ray 0

step2 :: [Cell] -> [Cell] -> [Cell]
step2 previous next = foldr splitTachyon next $ enumerate previous
  where
    splitTachyon (i, c) acc =
      case c of
        Splitter -> acc
        Ray x -> case acc !! i of
          Splitter -> (acc & ix (i - 1::Int) %~ (add x)) & ix (i + 1::Int) %~ (add x)
          _ -> acc & ix i %~ (add x)

solve2 :: [[Cell]] -> Int
solve2 (x:xs) = foldr f 0 $ foldl step2 x xs
  where
    f elem acc =
      case elem of
        Ray x -> acc + x
        Splitter -> acc
