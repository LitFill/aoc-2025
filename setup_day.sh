#!/bin/bash

# Script to set up the directory structure for a new Advent of Code day.

set -e # Exit immediately if a command exits with a non-zero status.

# 1. Determine the next day number
LATEST_DAY_DIR=$(ls -d [0-9][0-9] | sort -r | head -n 1)
if [ -z "$LATEST_DAY_DIR" ]; then
  NEXT_DAY=1
else
  NEXT_DAY=$((10#$LATEST_DAY_DIR + 1))
fi

DAY_NUM_STR=$(printf "%02d" $NEXT_DAY)
DAY_NAME="day${DAY_NUM_STR}"

echo "Setting up Day $DAY_NUM_STR..."

# --- Check if day already exists ---
if [ -d "$DAY_NUM_STR" ]; then
  echo "Error: Directory '$DAY_NUM_STR' already exists."
  exit 1
fi
if [ -d "haskell/$DAY_NAME" ]; then
  echo "Error: Directory 'haskell/$DAY_NAME' already exists."
  exit 1
fi
if [ -d "rust/src/$DAY_NAME" ]; then
  echo "Error: Directory 'rust/src/$DAY_NAME' already exists."
  exit 1
fi


# 2. Create new directories
echo "Creating directories..."
mkdir -p "$DAY_NUM_STR"
mkdir -p "haskell/$DAY_NAME"
mkdir -p "rust/src/$DAY_NAME"

# 3. Create empty files
echo "Creating input files..."
touch "$DAY_NUM_STR/input.txt"
touch "$DAY_NUM_STR/test.txt"

# 4. Create template source files
echo "Creating source file templates..."
# Haskell template
cat > "haskell/$DAY_NAME/Main.hs" << EOL
module Main where

main :: IO ()
main = do
  -- Read input file
  input <- readFile "input.txt"
  
  -- Part 1
  putStrLn $ "Part 1: " ++ solvePart1 input
  
  -- Part 2
  putStrLn $ "Part 2: " ++ solvePart2 input

solvePart1 :: String -> String
solvePart1 input = "Not implemented yet"

solvePart2 :: String -> String
solvePart2 input = "Not implemented yet"
EOL

# Rust template
cat > "rust/src/$DAY_NAME/main.rs" << EOL
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Should have been able to read the file");

    println!("Part 1: {}", solve_part1(&input));
    println!("Part 2: {}", solve_part2(&input));
}

fn solve_part1(input: &str) -> String {
    "Not implemented yet".to_string()
}

fn solve_part2(input: &str) -> String {
    "Not implemented yet".to_string()
}
EOL

# 5. Create symlinks
echo "Creating symlinks..."
(cd "haskell/$DAY_NAME" && ln -s "../../$DAY_NUM_STR/input.txt" "input.txt")
(cd "haskell/$DAY_NAME" && ln -s "../../$DAY_NUM_STR/test.txt" "test.txt")
(cd "rust/src/$DAY_NAME" && ln -s "../../../$DAY_NUM_STR/input.txt" "input.txt")
(cd "rust/src/$DAY_NAME" && ln -s "../../../$DAY_NUM_STR/test.txt" "test.txt")

# 6. Update build configurations
echo "Updating build configurations..."

# Update Haskell .cabal file
echo "" >> haskell/aoc25litfill.cabal
echo "executable $DAY_NAME" >> haskell/aoc25litfill.cabal
echo "    import:           warnings" >> haskell/aoc25litfill.cabal
echo "    main-is:          Main.hs" >> haskell/aoc25litfill.cabal
echo "    build-depends:    base ^>=4.21.0.0, mtl" >> haskell/aoc25litfill.cabal
echo "    hs-source-dirs:   $DAY_NAME" >> haskell/aoc25litfill.cabal
echo "    default-language: GHC2024" >> haskell/aoc25litfill.cabal

# Update Rust Cargo.toml file
echo "" >> rust/Cargo.toml
echo "[[bin]]" >> rust/Cargo.toml
echo "name = \"$DAY_NAME\"" >> rust/Cargo.toml
echo "path = \"src/$DAY_NAME/main.rs\"" >> rust/Cargo.toml

echo "Day $DAY_NUM_STR has been set up successfully!"
echo "Remember to 'git add .' to track the new files."
