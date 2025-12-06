module Day6 where

import Text.Parsec
import Text.Parsec.Char
import Data.Functor
import Data.List

data Input = Input { l :: [[Int]], operations :: [[Int] -> Int] }

day6part1 :: String -> IO ()
day6part1 file = do
  input <- readFile file
  let result = fmap solve $ parseInput input
  either print print result

number :: Parsec String () Int
number = parsecMap read (many1 digit)

operation :: Parsec String () ([Int] -> Int)
operation = (char '+' $> sum) <|> (char '*' $> product)

intLine :: Parsec String () [Int]
intLine = skipMany (char ' ') *> many (number <* skipMany (char ' '))

opLine :: Parsec String () [[Int] -> Int]
opLine = skipMany (char ' ') *> many (operation <* skipMany (char ' '))

inputParser :: Parsec String () Input
inputParser =
  do
    lines <- intLine `endBy1` endOfLine
    operations <- opLine
    _ <- endOfLine
    _ <- eof
    return $ Input lines operations

parseInput :: String -> Either ParseError Input
parseInput input = parse inputParser "" input

solve :: Input -> Int
solve (Input lines operations) = sum $ zipWith ($) operations (transpose lines)

---

day6part2 :: String -> IO ()
day6part2 file = do
  input <- readFile file
  let result = parseInput2 (unlines . transpose . (map reverse) $ lines input)
  either print print result

parseInput2 :: String -> Either ParseError Int
parseInput2 input = parse cephMathFile "" input

cephMathLine :: Parsec String () Int
cephMathLine =
  do
    _ <- spaces
    nums <- number `endBy1` spaces
    op <- operation
    _ <- endOfLine
    return $ op nums

cephMathFile :: Parsec String () Int
cephMathFile =
  do
    result <- sum <$> many1 cephMathLine
    _ <- eof
    return result
