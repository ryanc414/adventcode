use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let (input, preamble_len) = load_input();
    let num = find_first_rulebreaker(&input, preamble_len)
        .expect("could not find any numbers that break the rules");
    println!("first number to break the rules is {}", num)
}

fn load_input() -> (Vec<u64>, usize) {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("please specify input filename and preamble length");
    }

    let filename = &args[1];
    let preamble: usize = args[2].parse().unwrap();

    let contents = fs::read_to_string(filename).expect("error reading the file");

    let input: Vec<u64> = contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect();

    (input, preamble)
}

fn find_first_rulebreaker(input: &[u64], preamble_len: usize) -> Option<u64> {
    for i in preamble_len..input.len() {
        if !validate(input[i], &input[i - preamble_len..i]) {
            return Some(input[i]);
        }
    }
    None
}

fn validate(num: u64, previous: &[u64]) -> bool {
    let mut prev = collect_previous(previous);

    for &val in previous {
        if val > num {
            continue;
        }

        *prev.get_mut(&val).unwrap() -= 1;

        let diff = num - val;
        if let Some(&val) = prev.get(&diff) {
            if val > 0 {
                return true;
            }
        }

        *prev.get_mut(&val).unwrap() += 1;
    }

    false
}

fn collect_previous(previous: &[u64]) -> HashMap<u64, usize> {
    let mut map: HashMap<u64, usize> = HashMap::new();

    for &val in previous {
        let count = match map.get(&val) {
            Some(val) => val + 1,
            None => 1,
        };
        map.insert(val, count);
    }

    map
}
