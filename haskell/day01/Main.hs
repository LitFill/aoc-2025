{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}

module Main where

import Control.Monad (when)
import Text.Printf (printf)
import Control.Monad.State

testFile :: FilePath
testFile = "./day01/test.txt"

inputFile :: FilePath
inputFile = "./day01/input.txt"

type Step = Either Int Int
type Dial = State Int Int

parseStep :: String -> Step
parseStep = \case
    'L' : n -> Left  $ read n
    'R' : n -> Right $ read n
    _ -> error "invalid input"

step2int :: Step -> Int
step2int (Left  n) = -n
step2int (Right n) =  n

first :: (a -> a') -> (a, b) -> (a', b)
first f (a, b) = (f a, b)

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

addMod100 :: Integral a => a -> a -> a
addMod100 a b = (a + b) `mod` 100

parseInput :: FilePath -> IO [Int]
parseInput fpath =
    map (step2int . parseStep)
    . filter (not . null)
    . lines
    <$> readFile fpath

answer1 :: [Int] -> Int
answer1 xs = length . filter (== 0) $ scanl addMod100 initial xs

count :: Step -> Dial
count step = do
    current <- get
    let step' = step2int step
    let new = current + step'
    let cond =
            if current == 0
                then new <= (-100) || new >= 100
                else new <= 0      || new >= 99
    put $ new `mod` 100
    let
        one :: Float
        one = fromIntegral new / 100
    if cond
        then return . abs $ floor one
        else return 0

count2 :: Step -> Dial
count2 (Right step) = do
    current <- get
    let new = current + step
    put $ new `mod` 100
    if (step + current) >= 100
        then return $ (step + current) `div` 100
        else return 0
count2 (Left step) = do
    current <- get
    let new = current - step
    put $ new `mod` 100
    let cond = if current == 0 then step >= 100 else step >= current
    if cond
        then return $ (step - current) `div` 100 + 1
        else return 0

count2' :: Step -> Dial
count2' step = do
    current <- get
    let
        delta = step2int step
        next = current + delta
        cross = if delta >= 0
                then next `div` 100 - current `div` 100
                else (current - 1) `div` 100 - (next - 1) `div` 100
    put next
    return cross

answer2 :: [Step] -> Int
answer2 input = sum $ evalState (mapM count2' input) initial

stepping :: Step -> Dial
stepping step = do
    current <- get
    let new = current + step2int step
    put new
    return . abs $
        new `div` 100 - current `div` 100

initial :: Int
initial = 50

main :: IO ()
main = do
    tests <- parseInput testFile
    when (answer1 tests /= 3) do
        error "algorithm is wrong in answer1"

    inputs <- parseInput inputFile
    let answer01 = answer1 inputs

    tests2 <- map parseStep . lines <$> readFile testFile
    when (answer2 tests2 /= 6) do
        error "algorithm is wrong in answer2"

    inputs2 <- map parseStep . filter (not . null) . lines <$> readFile inputFile
    let answer02 = answer2 inputs2

    printf """
        ====================
        DAY 01
        Answer 1 = %d
        Answer 2 = %d
        ====================
        """
        answer01
        answer02
