{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (when)
import Data.Char     (isDigit)
import Data.Function ((&))
import Text.Printf   (printf)

testFile, inputFile :: FilePath
testFile  = "./day03/test.txt"
inputFile = "./day03/input.txt"

biggestNDigitNumber :: Int -> [Char] -> Int
biggestNDigitNumber n cs = read res
  where
    ds  = filter isDigit cs
    res = go n ds

    go 0 _  = []
    go k xs = maxDgt : go (k - 1) suffix
      where
        slack  = length xs - k
        window = take (slack + 1) xs
        maxDgt = maximum window
        suffix =
            xs &break (== maxDgt)
               &snd
               &drop 1

biggest2DigitNumber :: [Char] -> Int
biggest2DigitNumber cs = read [d1, d2]
  where
    ds     = filter isDigit cs
    maxDgt = maximum ds

    (prefix, drop 1 -> suffix) =
        break (== maxDgt) ds

    (d1, d2)
        | null suffix = (maximum prefix, maxDgt)
        | otherwise   = (maxDgt, maximum suffix)

solve :: String -> Int -> Int
solve input n =
    input
        &lines
        &map (biggestNDigitNumber n)
        &sum

solvePart1 :: String -> Int
solvePart1 input = solve input 2

solvePart2 :: String -> Int
solvePart2 input = solve input 12

main :: IO ()
main = do
    tests <- readFile testFile
    when (solvePart1 tests /= 357) do
        error "wrong algorithm in answer 1"
    when (solvePart2 tests /= 3121910778619) do
        error "wrong algorithm in answer 2"

    inputs <- readFile inputFile
    let answer01 = solvePart1 inputs
    let answer02 = solvePart2 inputs

    printf """
        ====================
        DAY 03
        Answer 1 = %d
        Answer 2 = %d
        ====================
        """
        answer01 answer02
