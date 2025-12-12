{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BlockArguments   #-}

module Main where

import Control.Monad   (when)
import Data.List.Split (splitOn)
import Text.Printf     (printf)

data Range = Range
    { rangeStart :: Int
    , rangeEnd   :: Int
    } deriving Show

rangeToList :: Range -> [Int]
rangeToList (Range s e)
    | s <= e    = [s..e]
    | otherwise = [e..s]

testFile, inputFile :: FilePath
testFile  = "./day02/test.txt"
inputFile = "./day02/input.txt"

parseInput :: String -> [Range]
parseInput = map parseRange . splitOn ","

parseRange :: String -> Range
parseRange s = case map read $ splitOn "-" s of
    [start, end] -> Range start end
    _            -> error "invalid range input"

-- Aggressive Optimization: Integer Arithmetic Instead of String/Text --

powersOf10Integer :: [Integer]
powersOf10Integer = take 20 $ iterate (*10) 1

numDigits :: Int -> Int
numDigits n
    | n < 10 = 1
    | n < 100 = 2
    | n < 1000 = 3
    | n < 10000 = 4
    | n < 100000 = 5
    | n < 1000000 = 6
    | n < 10000000 = 7
    | n < 100000000 = 8
    | n < 1000000000 = 9
    | n < 10000000000 = 10
    | n < 100000000000 = 11
    | n < 1000000000000 = 12
    | n < 10000000000000 = 13
    | n < 100000000000000 = 14
    | n < 1000000000000000 = 15
    | n < 10000000000000000 = 16
    | n < 100000000000000000 = 17
    | n < 1000000000000000000 = 18
    | otherwise = 19

isInvalidId :: Int -> Bool
isInvalidId n_int
    | d < 2 || odd d = False
    | otherwise =
        let h           = d `div` 2
            n           = fromIntegral n_int :: Integer
            powerOf10_h = powersOf10Integer !! h
            divisor     = powerOf10_h + 1
            (q, r)      = n `quotRem` divisor
            q_int       = fromIntegral q :: Int
        in r == 0 && q_int > 0 && numDigits q_int == h
  where
    d = numDigits n_int

properDivisors :: Int -> [Int]
properDivisors n =
    [ i
    | i <- [1..n `div` 2]
    , n `rem` i == 0
    ]

isInvalidId2 :: Int -> Bool
isInvalidId2 n_int
    | d < 2 = False
    | otherwise = any isRepeatingWithPeriod (properDivisors d)
  where
    n = fromIntegral n_int
    d = numDigits n_int
    powerOf10_d = powersOf10Integer !! d
    isRepeatingWithPeriod p =
        let powerOf10_p = powersOf10Integer !! p
            prefix      = n `quot` (powersOf10Integer !! (d - p))
        in n * (powerOf10_p - 1) == prefix * (powerOf10_d - 1)

-- End of Aggressive Optimization --

solvePart1 :: String -> Int
solvePart1 = sum . map (sum . filter isInvalidId . rangeToList) . parseInput

solvePart2 :: String -> Int
solvePart2 = sum . map (sum . filter isInvalidId2 . rangeToList) . parseInput

main :: IO ()
main = do
    tests <- readFile testFile
    when (solvePart1 tests /= 1227775554) do
        error "wrong algorithm in answer 1"
    when (solvePart2 tests /= 4174379265) do
        error "wrong algorithm in answer 2"

    inputs <- readFile inputFile
    let answer01 = solvePart1 inputs
    let answer02 = solvePart2 inputs

    printf """
        ====================
        DAY 02
        Answer 1 = %d
        Answer 2 = %d
        ====================

        """
        answer01 answer02
