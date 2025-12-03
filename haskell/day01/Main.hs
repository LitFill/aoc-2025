{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Main where

import Control.Monad (when)
import Text.Printf (printf)
import Data.List (scanl', mapAccumL)

testFile, inputFile :: FilePath
testFile  = "./day01/test.txt"
inputFile = "./day01/input.txt"

addMod :: Int -> Int -> Int
addMod a b = (a + b) `mod` dialCount

parseInput :: String -> [Int]
parseInput = map parseLine . filter (not . null) . lines
  where
    parseLine = \case
        'L' : n -> negate $ read n
        'R' : n -> read n
        _ -> error "Input tidak valid"

answer1 :: [Int] -> Int
answer1 deltas = length . filter (== 0) $
    scanl' (\acc d -> (acc + d) `mod` dialCount) initial deltas

answer2 :: [Int] -> Int
answer2 deltas = sum crossings
  where
    (_, crossings) = mapAccumL step initial deltas

    step current delta = (next, cross)
      where
        next = current + delta
        cross
            | delta >= 0 =
                next `div` dialCount - current `div` dialCount
            | otherwise  =
                (current - 1) `div` dialCount - (next - 1) `div` dialCount

initial :: Int
initial = 50

dialCount :: Int
dialCount = 100

main :: IO ()
main = do
    tests <- parseInput <$> readFile testFile

    when (answer1 tests /= 3) do
        error "algorithm is wrong in answer1"

    when (answer2 tests /= 6) do
        error "algorithm is wrong in answer2"

    inputs <- parseInput <$> readFile inputFile
    let answer01 = answer1 inputs
    let answer02 = answer2 inputs

    printf """
        ====================
        DAY 01
        Answer 1 = %d
        Answer 2 = %d
        ====================
        """
        answer01 answer02
