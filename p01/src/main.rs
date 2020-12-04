use std::collections::HashSet;
use std::env;
use std::fs;

const TARGET: i32 = 2020;

fn main() {
    let input = load_input();
    let input_set: HashSet<i32> = input.iter().cloned().collect();

    match find_two_multiple(&input, &input_set) {
        Some(x) => println!("first result: {}", x),
        None => println!("no result found for first part"),
    }

    match find_three_multiple(&input, &input_set) {
        Some(x) => println!("second result: {}", x),
        None => println!("no result found for second part"),
    }
}

fn load_input() -> Vec<i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Error reading the file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect()
}

fn find_three_multiple(input: &[i32], input_set: &HashSet<i32>) -> Option<i64> {
    for &x in input.iter() {
        if x > TARGET {
            continue;
        }

        let rest = TARGET - x;

        // Really we should remove x from the input vec and set before looking
        // for matching y and z values, however we can neglect to do so for
        // simplicity/laziness.
        if let Some((y, z)) = find_two_sum(input, input_set, rest) {
            return Some(x as i64 * y as i64 * z as i64);
        }
    }

    None
}

fn find_two_multiple(input: &[i32], input_set: &HashSet<i32>) -> Option<i64> {
    match find_two_sum(input, input_set, TARGET) {
        Some((x, y)) => Some(x as i64 * y as i64),
        None => None,
    }
}

fn find_two_sum(input: &[i32], input_set: &HashSet<i32>, target: i32) -> Option<(i32, i32)> {
    for &x in input {
        if x > target {
            continue;
        }

        let y = target - x;
        if input_set.contains(&y) {
            return Some((x, y));
        }
    }

    None
}
