use regex::Regex;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let result = count_valid(&input);
    println!("there are {} valid passwords", result);
}

struct PasswordInput {
    password: String,
    policy_letter: char,
    min_letter_count: usize,
    max_letter_count: usize,
}

fn load_input() -> Vec<PasswordInput> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Error reading the file");
    let line_re = Regex::new(r"^(\d+)-(\d+) (.): (.+)$").unwrap();

    contents
        .split('\n')
        .filter(|line| line.len() > 0)
        .map(|line| parse_line(line, &line_re))
        .collect()
}

fn parse_line(line: &str, re: &Regex) -> PasswordInput {
    let cap = re.captures(line).unwrap();

    PasswordInput {
        min_letter_count: cap[1].parse().unwrap(),
        max_letter_count: cap[2].parse().unwrap(),
        policy_letter: cap[3].chars().next().unwrap(),
        password: cap[4].to_owned(),
    }
}

fn count_valid(input: &Vec<PasswordInput>) -> usize {
    input
        .iter()
        .filter(|&pass_input| validate_password(pass_input))
        .count()
}

fn validate_password(pass_input: &PasswordInput) -> bool {
    let letter_count = pass_input
        .password
        .chars()
        .filter(|&letter| letter == pass_input.policy_letter)
        .count();

    letter_count >= pass_input.min_letter_count && letter_count <= pass_input.max_letter_count
}
