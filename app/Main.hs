module Main where

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

day1 :: String -> IO ()
day1 file = do
  input <- readInput file
  let parsed = mapM readLine input
  either putStrLn (print . countZeros) parsed

day1part2 :: String -> IO ()
day1part2 file = do
  input <- readInput file
  let parsed = mapM readLine input
  either putStrLn (print . countZeros2) parsed

readInput :: String -> IO [String]
readInput file = fmap lines $ readFile file

readLine :: String -> Either String Int
readLine ('R':rest) = Right $ read rest
readLine ('L':rest) = Right $ - (read rest)
readLine other = Left $ "Can’t read " ++ other

countZeros :: [Int] -> Int
countZeros list = length $ filter (== 0) $ scanl rotate 50 list

countZeros2 list = zeros $ foldl rotate2 (Rotations 50 0) list

rotate :: Int -> Int -> Int
rotate x y = (x + y) `mod` 100

data Rotations = Rotations { current :: Int , zeros :: Int }

crossZero :: Int -> Int -> Int
crossZero x 0 = 0
crossZero x y | y > 0 = (x + y) `div` 100
crossZero x y | y < 0 = (((100 - x) `mod` 100)  - y) `div` 100

rotate2 :: Rotations -> Int -> Rotations
rotate2 (Rotations current zeros) x = Rotations (rotate current x) (zeros + (crossZero current x))
