module Day7 where

import Control.Lens
import Control.Lens.At
import Control.Lens.Tuple

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
