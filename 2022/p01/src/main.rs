use std::env;
use std::fs;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);
    let elf_calories = group_calories(&input);

    let max_calories = find_max_total_cals(&elf_calories);
    println!("Max calories: {}", max_calories);

    let calories_sum = find_top_calories_sum(&elf_calories);
    println!("Max calories sum: {}", calories_sum);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

fn load_input(filename: &str) -> Vec<String> {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    contents.lines().map(|s| s.to_string()).collect()
}

fn find_max_total_cals(elf_calories: &[Vec<u32>])-> u32 {
    let total_calories = elf_calories.iter().map(|g| g.iter().sum());
    total_calories.max().unwrap()
}

fn group_calories(input: &[String]) -> Vec<Vec<u32>> {
    let mut groups = Vec::new();
    groups.push(Vec::new());

    for line in input {
        if line.is_empty() {
            groups.push(Vec::new());
        } else {
            groups.last_mut().unwrap().push(line.parse().expect("failed to parse line"));
        }
    }

    groups
}

fn find_top_calories_sum(elf_calories: &[Vec<u32>]) -> u32 {
    let mut total_calories: Vec<u32> = elf_calories.iter().map(|g| g.iter().sum()).collect();

    total_calories.sort();
    total_calories.reverse();

    total_calories[0..3].iter().sum()
}
