{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (when)
import Data.Function ((&))
import Data.Map      (Map)
import Text.Printf   (printf)

import Data.Map qualified as Map
import Data.Set qualified as Set

testFile, inputFile :: FilePath
testFile  = "./day04/test.txt"
inputFile = "./day04/input.txt"

type Koord     = (Int, Int)
type JmlhTtngg = Int
type Peta      = Map Koord JmlhTtngg

parseInput :: String -> Peta
parseInput (lines -> input) = jumlahTetangga
  where
    koord =
        [ (r, c)
        | (r, line) <- zip [0..] input
        , (c, char) <- zip [0..] line
        , char == '@'
        ]
    koordSet =
        Set.fromList koord
    tetangga (r, c) =
        length [ ()
               | (dr, dc) <- deltas
               , Set.member (r + dr, c + dc) koordSet
               ]
    jumlahTetangga =
        Map.fromList [(p, tetangga p) | p <- koord]

deltas :: [Koord]
deltas = [ (r, c)
         | r <- [-1..1]
         , c <- [-1..1]
         , (r, c) /= (0, 0)
         ]

solvePart1 :: String -> Int
solvePart1 (parseInput -> grid) =
    grid
        &Map.filter (< 4)
        &Map.size

solvePart2 :: String -> Int
solvePart2 (parseInput -> grid) =
    processGrid grid
  where
    processGrid (Map.partition (< 4) -> (removable, remaining))
        | Map.null removable = 0
        | otherwise =
            let removeCount      = Map.size removable
                affectedTetangga =
                    [ tetangga
                    | r <- Map.keys removable
                    , (dr, dc) <- deltas
                    , let tetangga = (fst r + dr, snd r + dc)
                    , Map.member tetangga remaining
                    ]
                newCounts = foldl'
                    (flip (Map.adjust (\x -> x - 1)))
                    remaining
                    affectedTetangga
             in removeCount + processGrid newCounts

main :: IO ()
main = do
    tests <- readFile testFile
    when (solvePart1 tests /= 13) do
        error "wrong algorithm in answer 1"
    when (solvePart2 tests /= 43) do
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
