{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad (when)
import Text.Printf   (printf)

testFile, inputFile :: FilePath
testFile  = "./day04/test.txt"
inputFile = "./day04/input.txt"

solvePart1 :: String -> Int
solvePart1 _input = error "Not implemented yet"

solvePart2 :: String -> Int
solvePart2 _input = error "Not implemented yet"

main :: IO ()
main = do
    tests <- readFile testFile
    when (solvePart1 tests /= undefined) do
        error "wrong algorithm in answer 1"
    when (solvePart2 tests /= undefined) do
        error "wrong algorithm in answer 2"

    inputs <- readFile inputFile
    let answer01 = solvePart1 inputs
    let answer02 = solvePart2 inputs

    printf """
        ====================
        DAY day04
        Answer 1 = %d
        Answer 2 = %d
        ====================
        """
        answer01 answer02
