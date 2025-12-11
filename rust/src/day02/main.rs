use std::{error::Error, fs, str::FromStr};

const INPUT_FILE: &str = "src/day02/input.txt";
const TEST_FILE: &str = "src/day02/test.txt";

#[derive(Debug)]
struct Range {
    start : usize,
    end   : usize,
}

impl Range {
    fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl FromStr for Range {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splitted : Vec<_> = s.split("-").collect();

        if splitted.len() != 2 {
            return Err("invalid range input".to_string())
        }

        if let [start, end] = splitted.iter()
            .map(|s| s.parse().expect("Bukan angka"))
            .collect::<Vec<_>>()[..]
        {
            Ok(Self { start, end })
        } else {
            Err("invalid range input".to_string())
        }

    }
}

fn parse_input(input: &str) -> Result<Vec<Range>, Box<dyn Error>> {
    let mut ret = Vec::new();
    for rs in input.split(",") {
        if rs.is_empty() {
            continue;
        }
        match rs.parse::<Range>() {
            Ok(r) => ret.push(r),
            Err(e) => return Err(e.into()),
        }
    }
    Ok(ret)
}

fn solve_part1(input: &str) -> usize {
    match parse_input(input) {
        Ok(ranges) => {
            for range in ranges {
                println!("range: {range:?}");
            }
            0
        }
        Err(_) => 0,
    }
}

fn solve_part2(input: &str) -> String {
    "Not implemented yet".to_string()
}

fn main() {
    let tests = fs::read_to_string(TEST_FILE).expect("Should have been able to read the file");
    let input = fs::read_to_string(INPUT_FILE).expect("Should have been able to read the file");
    println!("{tests}");

    let answer01 = solve_part1(&input);
    let answer02 = solve_part2(&input);
    println!(
        "==================== \n\
        Day 01                \n\
        Answer 1 = {answer01} \n\
        Answer 2 = {answer02} \n\
        ===================="
    );
}
