use std::env;
use std::fs;
use std::str;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let total_score = calculate_score(&input);
    println!("{}", total_score);

    let new_total_score = calculate_new_score(&input);
    println!("{}", new_total_score);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

fn load_input(filename: &str) -> Vec<String> {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");

    contents.lines().map(|s| s.to_string()).collect()
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum HandShape {
    Rock,
    Paper,
    Scissors,
}

impl HandShape {
    fn to_ordinal(&self) -> u32 {
        match self {
            HandShape::Rock => 0,
            HandShape::Paper => 1,
            HandShape::Scissors => 2,
        }
    }

    fn score(&self) -> u32 {
        self.to_ordinal() + 1
    }

    fn parse_opponent_hand(hand: &str) -> Self {
        match hand {
            "A" => HandShape::Rock,
            "B" => HandShape::Paper,
            "C" => HandShape::Scissors,
            _ => panic!("Invalid hand shape: {}", hand),
        }
    }

    fn parse_your_hand(hand: &str) -> Self {
        match hand {
            "X" => HandShape::Rock,
            "Y" => HandShape::Paper,
            "Z" => HandShape::Scissors,
            _ => panic!("Invalid hand shape: {}", hand),
        }
    }
}

fn parse_hand_shapes(input: &Vec<String>) -> Vec<(HandShape, HandShape)> {
    let hand_shapes: Vec<Vec<&str>> = input
        .iter()
        .map(|line| line.split(" ").take(2).collect())
        .collect();

    hand_shapes
        .iter()
        .map(|pair| {
            (
                HandShape::parse_opponent_hand(pair[0]),
                HandShape::parse_your_hand(pair[1]),
            )
        })
        .collect()
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum HandResult {
    Win,
    Lose,
    Draw,
}

impl HandResult {
    fn to_delta(&self) -> u32 {
        match self {
            Self::Draw => 0,
            Self::Win => 1,
            Self::Lose => 2,
        }
    }

    fn from_delta(delta: u32) -> Self {
        match delta {
            0 => Self::Draw,
            1 => Self::Win,
            2 => Self::Lose,
            _ => panic!("Invalid delta: {}", delta),
        }
    }

    fn score(&self) -> u32 {
        match self {
            Self::Lose => 0,
            Self::Draw => 3,
            Self::Win => 6,
        }
    }
}

impl str::FromStr for HandResult {
    type Err = ();

    fn from_str(s: &str) -> Result<HandResult, ()> {
        match s {
            "X" => Ok(Self::Lose),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Win),
            _ => Err(()),
        }
    }
}

fn parse_shapes_and_results(input: &Vec<String>) -> Vec<(HandShape, HandResult)> {
    let hand_shapes: Vec<Vec<&str>> = input
        .iter()
        .map(|line| line.split(" ").take(2).collect())
        .collect();

    hand_shapes
        .iter()
        .map(|pair| {
            (
                HandShape::parse_opponent_hand(pair[0]),
                pair[1].parse().unwrap(),
            )
        })
        .collect()
}

fn calculate_score(input: &Vec<String>) -> u32 {
    let pairs = parse_hand_shapes(input);

    pairs
        .iter()
        .map(|pair| calculate_score_for_pair(pair))
        .sum()
}

fn calculate_score_for_pair(pair: &(HandShape, HandShape)) -> u32 {
    calculate_result_score(pair) + calculate_shape_score(pair)
}

fn calculate_shape_score(&(_, b): &(HandShape, HandShape)) -> u32 {
    b.score()
}

fn calculate_result_score(&(a, b): &(HandShape, HandShape)) -> u32 {
    let result_delta = (3 + b.to_ordinal() - a.to_ordinal()) % 3;
    let result = HandResult::from_delta(result_delta);
    result.score()
}

fn calculate_new_score(input: &Vec<String>) -> u32 {
    let pairs = parse_shapes_and_results(input);

    pairs
        .iter()
        .map(|pair| calculate_new_score_for_pair(pair))
        .sum()
}

fn calculate_new_score_for_pair(&(shape, result): &(HandShape, HandResult)) -> u32 {
    let shape_ordinal = shape.to_ordinal();
    let result_ordinal = (shape_ordinal + result.to_delta()) % 3;
    let shape_score = result_ordinal + 1;

    shape_score + result.score()
}
