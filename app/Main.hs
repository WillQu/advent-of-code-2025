module Main where

main :: IO ()
main = do
  putStrLn "Day 1 example"
  day1 "day1.example"
  putStrLn "Day 1"
  day1 "day1"

day1 :: String -> IO ()
day1 file = do
  input <- readInput file
  let parsed = mapM readLine input
  either putStrLn (print . countZeros) parsed

readInput :: String -> IO [String]
readInput file = fmap lines $ readFile file

readLine :: String -> Either String Int
readLine ('R':rest) = Right $ read rest
readLine ('L':rest) = Right $ - (read rest)
readLine other = Left $ "Can’t read " ++ other

countZeros :: [Int] -> Int
countZeros list = length $ filter (== 0) $ scanl rotate 50 list

rotate :: Int -> Int -> Int
rotate x y = (x + y) `mod` 100
