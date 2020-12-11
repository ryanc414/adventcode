use std::collections::VecDeque;
use std::env;
use std::fs;

const ALLOWED_JOLT_DIFF: u32 = 3;

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
    // Assuming each adaptor has a unique joltage, we only need to remember how
    // many ways we could connect to the adapter at most 3 back from the current
    // adaptor being processed. Use a VecDeque to achieve this.
    let mut arrangements_to: VecDeque<u64> = VecDeque::with_capacity(ALLOWED_JOLT_DIFF as usize);

    for i in 0..input.len() {
        let total_arrangements = count_arrangements_to(&input, i, &arrangements_to);
        if arrangements_to.len() == 3 {
            arrangements_to.pop_back();
        }
        arrangements_to.push_front(total_arrangements);
    }

    arrangements_to.pop_front().unwrap()
}

fn count_arrangements_to(jolts: &[u32], i: usize, arrangements_to: &VecDeque<u64>) -> u64 {
    let min_ix = if i > (ALLOWED_JOLT_DIFF as usize) {
        i - (ALLOWED_JOLT_DIFF as usize)
    } else {
        0
    };

    // Count an extra 1 if the current adapter can connect to the device at 0 jolts.
    let initial_count = if jolts[i] > ALLOWED_JOLT_DIFF { 0 } else { 1 };

    (min_ix..i).fold(initial_count, |count, j| {
        if jolts[i] - jolts[j] > ALLOWED_JOLT_DIFF {
            count
        } else {
            count + arrangements_to[i - j - 1]
        }
    })
}
