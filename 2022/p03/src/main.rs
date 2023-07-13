use std::env;
use std::fs;
use std::str;
use std::collections::HashSet;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let sum = sum_priorities(&input);
    println!("Sum of priorities: {}", sum);

    let badge_sum = sum_badge_priorities(&input);
    println!("Sum of badge priorities: {}", badge_sum);
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

fn sum_priorities(input: &Vec<String>) -> u32 {
    input.iter().map(|s| calculate_shared_item_priority(s).unwrap()).sum()
}


fn calculate_shared_item_priority(line: &str) -> Result<u32, &str> {
    let compartment_size = line.len() / 2;

    let compartments: (HashSet<char>, HashSet<char>) = (
        HashSet::from_iter(line[0..compartment_size].chars()),
        HashSet::from_iter(line[compartment_size..].chars()),
    );

    let intersection: Vec<char> = compartments.0.intersection(&compartments.1).cloned().collect();
    if intersection.len() != 1 {
        return Err("unexpected intersection size");
    }

    calculate_item_priority(intersection[0])
}



fn calculate_item_priority(c: char) -> Result<u32, &'static str> {
    let val = c as u32;

    if val >= 'A' as u32 && val <= 'Z' as u32 {
        return Ok(val - 'A' as u32 + 27);
    }

    if val >= 'a' as u32 && val <= 'z' as u32 {
        return Ok(val - 'a' as u32 + 1);
    }

    Err("unexpected character")
}

fn sum_badge_priorities(input: &Vec<String>) -> u32 {
    input.chunks(3).map(|chunk| calculate_badge_priority(chunk).unwrap()).sum()
}

fn calculate_badge_priority(group: &[String]) -> Result<u32, &str> {
    let mut intersection: HashSet<char> = HashSet::from_iter(group[0].chars());

    for line in group.iter().skip(1) {
        intersection = intersection.intersection(&HashSet::from_iter(line.chars())).cloned().collect();
    }

    if intersection.len() != 1 {
        return Err("unexpected intersection size");
    }

    calculate_item_priority(intersection.iter().next().unwrap().clone())
}
