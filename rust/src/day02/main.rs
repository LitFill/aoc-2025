use std::fs;

const INPUT_FILE: &str = "src/day02/input.txt";
const TEST_FILE:  &str = "src/day02/test.txt";

fn solve_part1(input: &str) -> String {
    "Not implemented yet".to_string()
}

fn solve_part2(input: &str) -> String {
    "Not implemented yet".to_string()
}

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("Should have been able to read the file");

    println!("Part 1: {}", solve_part1(&input));
    println!("Part 2: {}", solve_part2(&input));
}
