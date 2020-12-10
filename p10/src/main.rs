use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let result = multiply_jolt_differences(&input);
    println!("multiple of jolt differences is {}", result);
}

fn load_input() -> Vec<u32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading input file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect()
}

fn multiply_jolt_differences(input: &[u32]) -> u64 {
    let differences = find_jolt_differences(input);
    println!("differences: {:?}", differences);
    differences[0] * differences[2]
}

fn find_jolt_differences(input: &[u32]) -> [u64; 3] {
    let mut differences = [0, 0, 1];
    let mut vals = input.to_vec();
    vals.sort_unstable();
    let mut prev = 0;

    for j in vals {
        let diff = (j - prev) as usize;
        differences[diff - 1] += 1;
        prev = j;
    }

    differences
}
