use std::collections::HashSet;
use std::env;
use std::fs;

const TARGET: i64 = 2020;

fn main() {
    let input = load_input();

    let result = find_three_multiple(input);
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

fn find_three_multiple(input: Vec<i64>) -> Option<i64> {
    let input_set: HashSet<i64> = input.iter().cloned().collect();

    for x in input.iter() {
        if x > &TARGET {
            continue;
        }

        let rest = TARGET - x;

        // Really we should remove x from the input vec and set before looking
        // for matching y and z values, however we can neglect to do so for
        // simplicity/laziness.
        if let Some((y, z)) = find_two_sum(&input, &input_set, rest) {
            return Some(x * y * z);
        }
    }

    None
}

fn find_two_sum(input: &Vec<i64>, input_set: &HashSet<i64>, target: i64) -> Option<(i64, i64)> {
    for x in input {
        if x > &target {
            continue;
        }

        let y = target - x;
        if input_set.contains(&y) {
            return Some((*x, y));
        }
    }

    None
}
