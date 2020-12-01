use std::collections::HashSet;
use std::env;
use std::fs;

const TARGET: i64 = 2020;

fn main() {
    let input = load_input();

    let result = find_multiple(input);
    match result {
        Some(x) => println!("result: {}", x),
        None => println!("no result found"),
    }
}

fn load_input() -> Vec<i64> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Error reading the file");

    contents
        .split('\n')
        .filter(|line| line.len() > 0)
        .map(|line| line.parse().unwrap())
        .collect()
}

fn find_multiple(input: Vec<i64>) -> Option<i64> {
    let input_set: HashSet<i64> = input.iter().cloned().collect();

    for x in input {
        if x > TARGET {
            continue;
        }

        let y = TARGET - x;
        if input_set.contains(&y) {
            return Some(x * y);
        }
    }

    None
}
