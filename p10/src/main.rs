use std::env;
use std::fs;

fn main() {
    let mut input = load_input();
    input.sort_unstable();

    let result = multiply_jolt_differences(&input);
    println!("multiple of jolt differences is {}", result);

    let num_arrangements = find_num_arrangements(&input);
    println!("there are {} possible arrangements", num_arrangements);
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
    let mut prev = 0;

    for &j in input {
        let diff = (j - prev) as usize;
        differences[diff - 1] += 1;
        prev = j;
    }

    differences
}

fn find_num_arrangements(input: &[u32]) -> u64 {
    let mut arrangements_to: Vec<u64> = Vec::with_capacity(input.len() + 1);
    arrangements_to.push(1);

    let jolts: Vec<u32> = [0].iter().chain(input.iter()).cloned().collect();

    for i in 1..jolts.len() {
        let total_arrangements = count_arrangements_to(&jolts, i, &arrangements_to);
        arrangements_to.push(total_arrangements);
    }

    arrangements_to[arrangements_to.len() - 1]
}

fn count_arrangements_to(jolts: &[u32], i: usize, arrangements_to: &[u64]) -> u64 {
    let mut count = 0;
    let mut j = i - 1;

    while jolts[i] - jolts[j] <= 3 {
        count += arrangements_to[j];
        if j == 0 {
            break;
        }
        j -= 1;
    }

    count
}
