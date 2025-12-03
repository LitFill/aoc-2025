use std::{error::Error, fs, str::FromStr};

const INPUT_FILE: &str = "src/day01/input.txt";
const TEST_FILE: &str = "src/day01/test.txt";
const DIAL_MODULO: i64 = 100;

#[derive(Debug)]
enum Step {
    R(i64),
    L(i64),
}

impl Step {
    /// Mengembalikan nilai delta (positif atau negatif)
    fn as_delta(&self) -> i64 {
        match self {
            Self::R(n) => *n,
            Step::L(n) => -n,
        }
    }
}

impl FromStr for Step {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let dir = chars.next().ok_or("Input kosong")?;
        let num_str = chars.as_str();

        let num: i64 = num_str
            .parse()
            .map_err(|_| format!("Bukan angka: {num_str}"))?;

        match dir {
            'R' => Ok(Self::R(num)),
            'L' => Ok(Self::L(num)),
            c => Err(format!("arah tidak valid: {c}")),
        }
    }
}

fn parse_input(input: &str) -> Result<Vec<Step>, Box<dyn Error>> {
    input
        .lines()
        .map(|l| l.parse::<Step>().map_err(|e| e.into()))
        .collect()
}

fn answer1(steps: &[Step], initial: i64) -> i64 {
    let mut current = initial;
    let mut counter = 0;

    for step in steps {
        current = (current + step.as_delta()).rem_euclid(DIAL_MODULO);

        if current == 0 {
            counter += 1;
        }
    }

    counter
}

/// gemini's
fn answer2(steps: &[Step], initial: i64) -> i64 {
    let mut current_abs: i64 = initial;
    let mut counter: i64 = 0;

    for step in steps {
        let delta = step.as_delta() as i64;
        let next_abs = current_abs + delta;

        let cross = if delta >= 0 {
            next_abs.div_euclid(DIAL_MODULO) - current_abs.div_euclid(DIAL_MODULO)
        } else {
            (current_abs - 1).div_euclid(DIAL_MODULO) - (next_abs - 1).div_euclid(DIAL_MODULO)
        };

        counter += cross;
        current_abs = next_abs;
    }

    counter
}

// fn answer2(steps: &[Step], initial: i32) -> i32 {
//     let mut dial = Dial::new(initial);
//     let mut counter = 0;
//
//     for step in steps {
//         let overflow = dial.add_overflow(step);
//         match overflow {
//             Some(o) => counter += o,
//             None => {
//                 if dial.arrow == 0 {
//                     counter += 1;
//                 }
//             },
//         }
//
//     }
//
//     counter
// }

fn main() -> Result<(), Box<dyn Error>> {
    let tests_str = fs::read_to_string(TEST_FILE)?;
    let inputs_str = fs::read_to_string(INPUT_FILE)?;

    let tests = parse_input(&tests_str)?;
    let inputs = parse_input(&inputs_str)?;

    let test = answer1(&tests, 50);
    assert_eq!(test, 3, "algorithm is wrong in answer 1");

    let answer01 = answer1(&inputs, 50);

    let test = answer2(&tests, 50);
    assert_eq!(test, 6, "algorithm is wrong in answer 2");

    let answer02 = answer2(&inputs, 50);

    println!(
        "==================== \n\
        Day 01                \n\
        Answer 1 = {answer01} \n\
        Answer 2 = {answer02} \n\
        ===================="
    );

    Ok(())
}
