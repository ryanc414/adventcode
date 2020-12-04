use std::collections::HashMap;
use std::env;
use std::fs;

const REQUIRED_KEYS: [&str; 7] = [
    "byr", // (Birth Year)
    "iyr", // (Issue Year)
    "eyr", // (Expiration Year)
    "hgt", // (Height)
    "hcl", // (Hair Color)
    "ecl", // (Eye Color)
    "pid", // (Passport ID)
];

fn main() {
    let input = load_input();
    let count = count_valid(&input);
    println!("counted {} valid passports", count);
}

fn load_input() -> Vec<HashMap<String, String>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split("\n\n")
        .filter(|line| !line.is_empty())
        .map(parse_line)
        .collect()
}

fn parse_line(line: &str) -> HashMap<String, String> {
    line.split_ascii_whitespace()
        .map(|word| {
            let pair: Vec<&str> = word.split(':').collect();
            if pair.len() != 2 {
                panic!("unexpected length for pair: {}", pair.len())
            }
            (pair[0].to_owned(), pair[1].to_owned())
        })
        .collect()
}

fn count_valid(input: &[HashMap<String, String>]) -> usize {
    input
        .iter()
        .filter(|&passport| validate_passport(passport))
        .count()
}

fn validate_passport(passport: &HashMap<String, String>) -> bool {
    REQUIRED_KEYS.iter().all(|&key| match passport.get(key) {
        Some(val) => !val.is_empty(),
        None => false,
    })
}
