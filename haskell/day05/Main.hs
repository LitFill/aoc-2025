{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad  (when)
import Data.Bifunctor (bimap)
import Data.Function  ((&))
import Data.List      (sort)
import Text.Printf    (printf)

testFile, inputFile :: FilePath
testFile  = "./day05/test.txt"
inputFile = "./day05/input.txt"

type Range = (Int, Int)

parseInput :: String -> ([Range], [Int])
parseInput input = (map parseRange ranges, map read ids)
  where
    (ranges, drop 1 -> ids) =
        input &lines &break null
    parseRange s =
        s &break (== '-')
          &bimap read (read . drop 1)

isInRange :: Int -> Range -> Bool
isInRange n (s, e) =
    -- n `elem` [s..e]
    n >= s && n <= e

normalize :: [Range] -> [Range]
normalize []  = []
normalize [r] = [r]
normalize (r1@(s1, e1) : r2@(s2, e2) : rest)
    | e1 >= s2 - 1 = normalize  (merged : rest)
    | otherwise    = r1 : normalize (r2 : rest)
  where
    merged = (min s1 s2, max e1 e2)

rangeSize :: Num a => (a, a) -> a
rangeSize (s, e) = e - s + 1

solvePart1 :: String -> Int
solvePart1 input =
    ids &map inRanges
        &filter id
        &length
  where
    (ranges, ids) = parseInput input
    inRanges n    = any (n `isInRange`) ranges

solvePart2 :: String -> Int
solvePart2 input =
    ranges
        &sort
        &normalize
        &map rangeSize
        &sum
  where
    ranges = input &parseInput &fst

main :: IO ()
main = do
    tests <- readFile testFile
    when (solvePart1 tests /= 3) do
        error "wrong algorithm in answer 1"
    when (solvePart2 tests /= 14) do
        error "wrong algorithm in answer 2"

    inputs <- readFile inputFile
    let answer01 = solvePart1 inputs
    let answer02 = solvePart2 inputs

    printf """
        ====================
        DAY 05
        Answer 1 = %d
        Answer 2 = %d
        ====================

        """
        answer01 answer02
