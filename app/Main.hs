module Main where

import Day1 ( day1, day1part2 )
import Day2 ( day2part1, day2part2 )

main :: IO ()
main = do
  putStrLn "Day 1 example"
  day1 "day1.example"
  putStrLn "Day 1"
  day1 "day1"
  putStrLn "Day 1 part 2 example"
  day1part2 "day1.example"
  putStrLn "Day 1 part 2"
  day1part2 "day1"
  putStrLn "Day 2 part 1 example"
  day2part1 "day2.example"
  putStrLn "Day 2 part 1"
  day2part1 "day2"
  putStrLn "Day 2 part 2 example"
  day2part2 "day2.example"
  putStrLn "Day 2 part 2"
  day2part2 "day2"
