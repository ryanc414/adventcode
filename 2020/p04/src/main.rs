use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;

type ValidatorFn = Box<dyn Fn(&str) -> bool>;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let [simple_validation_rules, full_validation_rules] = build_validation_rules();

    let count_1 = count_valid(&input, &simple_validation_rules);
    println!("counted {} valid passports with simple scheme", count_1);

    let count_2 = count_valid(&input, &full_validation_rules);
    println!("counted {} valid passports with full scheme", count_2);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    args.remove(1)
}

fn load_input(filename: &str) -> Vec<HashMap<String, String>> {
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split("\n\n")
        .filter(|line| !line.is_empty())
        .map(parse_line)
        .collect()
}

fn build_validation_rules() -> [HashMap<&'static str, ValidatorFn>; 2] {
    let hair_colour_re = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    let passport_id_re = Regex::new(r"^\d{9}$").unwrap();

    let full_validation_rules: HashMap<&str, ValidatorFn> = vec![
        (
            "byr",
            Box::new(|val: &str| -> bool { validate_year(val, 1920, 2002) }) as ValidatorFn,
        ),
        ("iyr", Box::new(|val| validate_year(val, 2010, 2020))),
        ("eyr", Box::new(|val| validate_year(val, 2020, 2030))),
        ("hgt", Box::new(validate_height)),
        ("hcl", Box::new(move |val| hair_colour_re.is_match(val))),
        ("ecl", Box::new(validate_eye_colour)),
        ("pid", Box::new(move |val| passport_id_re.is_match(val))),
    ]
    .into_iter()
    .collect();

    let simple_validation_rules: HashMap<&str, ValidatorFn> = full_validation_rules
        .iter()
        .map(|(&key, _)| -> (&str, ValidatorFn) { (key, Box::new(|val| !val.is_empty())) })
        .collect();

    [simple_validation_rules, full_validation_rules]
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

fn count_valid(
    input: &[HashMap<String, String>],
    validation_rules: &HashMap<&str, ValidatorFn>,
) -> usize {
    input
        .iter()
        .filter(|&passport| validate_passport(passport, validation_rules))
        .count()
}

fn validate_passport(
    passport: &HashMap<String, String>,
    rules: &HashMap<&str, ValidatorFn>,
) -> bool {
    rules
        .iter()
        .all(|(&key, validate)| match passport.get(key) {
            Some(val) => validate(val),
            None => false,
        })
}

fn validate_year(input: &str, min: u32, max: u32) -> bool {
    if input.len() != 4 {
        return false;
    }

    let year: u32 = match input.parse() {
        Ok(year) => year,
        Err(_) => return false,
    };

    year >= min && year <= max
}

enum Unit {
    CM,
    Inches,
}

fn validate_height(input: &str) -> bool {
    let re = Regex::new(r"^(\d+)(cm|in)$").unwrap();
    let (raw_height, raw_unit) = match re.captures(input) {
        Some(cap) => (cap[1].to_owned(), cap[2].to_owned()),
        None => return false,
    };

    let height: u32 = match raw_height.parse() {
        Ok(num) => num,
        Err(_) => return false,
    };

    let unit = match raw_unit.as_str() {
        "cm" => Unit::CM,
        "in" => Unit::Inches,
        _ => panic!("how did this happen"),
    };

    match unit {
        Unit::CM => height >= 150 && height <= 193,
        Unit::Inches => height >= 59 && height <= 76,
    }
}

fn validate_eye_colour(input: &str) -> bool {
    matches!(input, "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let [simple_validation_rules, full_validation_rules] = build_validation_rules();

        let res = count_valid(&input, &simple_validation_rules);
        assert_eq!(res, 2);

        let res = count_valid(&input, &full_validation_rules);
        assert_eq!(res, 2);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let [simple_validation_rules, full_validation_rules] = build_validation_rules();

        let res = count_valid(&input, &simple_validation_rules);
        assert_eq!(res, 228);

        let res = count_valid(&input, &full_validation_rules);
        assert_eq!(res, 175);
    }
}
