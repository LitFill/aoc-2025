{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE BlockArguments   #-}

module Main where

import Control.Monad   (when)
import Data.Function   ((&))
import Data.List.Split (splitOn)
import Text.Printf     (printf)

import Data.Text qualified as T

data Range = Range
    { rangeStart :: Int
    , rangeEnd   :: Int
    } deriving Show

rangeToList :: Range -> [Int]
rangeToList (Range s e) | s <= e = [s..e]
rangeToList (Range s e) = [e..s]

testFile, inputFile :: FilePath
testFile  = "./day02/test.txt"
inputFile = "./day02/input.txt"

parseInput :: String -> [Range]
parseInput s = s &splitOn "," &map parseRange

parseRange :: String -> Range
parseRange (map read . splitOn "-" -> [start, end]) =
    Range start end
parseRange _ = error "invalid range input"

(%%) :: Integral a => a -> a -> Bool
a %% b = a `mod` b == 0

isInvalidId :: Int -> Bool
isInvalidId (show -> n)
    | length n %% 2 =
        let half = length n `div` 2
         in take half n == drop half n
isInvalidId _ = False

solvePart1 :: String -> Int
solvePart1 (parseInput -> input) =
    input
    & map (sum . filter isInvalidId . rangeToList)
    & sum

isInvalidId2 :: Int -> Bool
isInvalidId2 (show -> [_]) = False
isInvalidId2 (T.pack . show -> n) =
    let s  = n <> n
        s' = T.drop 1 $ T.dropEnd 1 s
     in n `T.isInfixOf` s'

solvePart2 :: String -> Int
solvePart2 (parseInput -> input) =
    input
    & map (sum . filter isInvalidId2 . rangeToList)
    & sum

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
        DAY 01
        Answer 1 = %d
        Answer 2 = %d
        ====================
        """
        answer01 answer02
