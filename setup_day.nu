# Script to set up the directory structure for a new Advent of Code day in Nushell.

def main [] {
    # 1. Determine the next day number
    # We filter for directories that look like digits
    let latest_day_str = (ls | where type == dir and name =~ '^\d{2}$' | get name | sort | last)

    let next_day_num = if ($latest_day_str | is-empty) {
        1
    } else {
        ($latest_day_str | into int) + 1
    }

    # FIX: Use 'fill' to zero-pad the number
    let day_num_str = ($next_day_num | into string | fill -a r -c '0' -w 2)
    # CLEANUP: Use string interpolation $"..."
    let day_name = $"day($day_num_str)"

    print $"Setting up Day ($day_num_str)..."

    # --- Check if day already exists ---
    # FIX: Use 'path exists' instead of 'file-exists'
    if ($day_num_str | path exists) {
        error make {msg: $"Directory '($day_num_str)' already exists."}
    }
    if ($"haskell/($day_name)" | path exists) {
        error make {msg: $"Directory 'haskell/($day_name)' already exists."}
    }
    if ($"rust/src/($day_name)" | path exists) {
        error make {msg: $"Directory 'rust/src/($day_name)' already exists."}
    }

    # 2. Create new directories
    print "Creating directories..."
    mkdir $day_num_str
    mkdir $"haskell/($day_name)"
    mkdir $"rust/src/($day_name)"

    # 3. Create empty files
    print "Creating input files..."
    touch $"($day_num_str)/input.txt"
    touch $"($day_num_str)/test.txt"

    # 4. Create template source files
    print "Creating source file templates..."
    # Haskell template
    let hs_template = 'module Main where

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
'
    $hs_template | save $"haskell/($day_name)/Main.hs"

    # Rust template
    let rs_template = 'use std::fs;

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
'
    $rs_template | save $"rust/src/($day_name)/main.rs"

    # 5. Create symlinks
    print "Creating symlinks..."
    # Note: relative paths for symlinks depend on where the command is run vs where the link is.
    # The original script logic for paths seems correct assuming you run from root.
    ^ln -s $"../../($day_num_str)/input.txt" $"haskell/($day_name)/input.txt"
    ^ln -s $"../../($day_num_str)/test.txt" $"haskell/($day_name)/test.txt"
    ^ln -s $"../../../($day_num_str)/input.txt" $"rust/src/($day_name)/input.txt"
    ^ln -s $"../../../($day_num_str)/test.txt" $"rust/src/($day_name)/test.txt"


    # 6. Update build configurations
    print "Updating build configurations..."

    # Update Haskell .cabal file
    let cabal_path = "haskell/aoc25litfill.cabal"
    let new_cabal_entry = $"\n\nexecutable ($day_name)\n    import:           warnings\n    main-is:          Main.hs\n    build-depends:    base ^>=4.21.0.0, mtl\n    hs-source-dirs:   ($day_name)\n    default-language: GHC2024"

    # FIX: Use 'save --append' directly.
    # Opening normally parses the file (if recognized) or reads all text,
    # but specific file formats like TOML are parsed to tables, making append fail.
    $new_cabal_entry | save --append $cabal_path

    # Update Rust Cargo.toml file
    let cargo_path = "rust/Cargo.toml"
    let new_cargo_entry = $"\n\n[[bin]]\nname = \"($day_name)\"\npath = \"src/($day_name)/main.rs\""
    $new_cargo_entry | save --append $cargo_path


    print $"Day ($day_num_str) has been set up successfully!"
    print "Remember to 'git add .' to track the new files."
}
