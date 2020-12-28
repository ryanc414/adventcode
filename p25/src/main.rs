use std::env;
use std::fs;

const INITIAL_VALUE: u64 = 1;
const INITIAL_SUBJECT: u64 = 7;
const DIVISOR: u64 = 20201227;

fn main() {
    let input = load_input();

    let encryption_key = find_encryption_key(&input);
    println!("encryption key is {}", encryption_key);
}

fn load_input() -> [u64; 2] {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read input file");
    let lines: Vec<u64> = contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect();

    if lines.len() != 2 {
        panic!("unexpected input length");
    }

    [lines[0], lines[1]]
}

fn find_encryption_key(public_keys: &[u64; 2]) -> u64 {
    let (loop_size, device) = find_first_loop_size(public_keys);
    calculate_encryption_key(loop_size, public_keys[(device + 1) % 2])
}

fn find_first_loop_size(public_keys: &[u64; 2]) -> (u64, usize) {
    let mut value = INITIAL_VALUE;
    let mut loop_size = 1;

    loop {
        value = transform_value(value, INITIAL_SUBJECT);
        if value == public_keys[0] {
            return (loop_size, 0);
        } else if value == public_keys[1] {
            return (loop_size, 1);
        }
        loop_size += 1;
    }
}

fn transform_value(value: u64, subject: u64) -> u64 {
    let value = value * subject;
    value % DIVISOR
}

fn calculate_encryption_key(loop_size: u64, public_key: u64) -> u64 {
    let mut value = 1;

    for _ in 0..loop_size {
        value = transform_value(value, public_key);
    }
    value
}
