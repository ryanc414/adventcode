use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let (filename, preamble_len) = parse_args();
    let input = load_input(&filename);

    let num = find_first_rulebreaker(&input, preamble_len)
        .expect("could not find any numbers that break the rules");
    println!("first number to break the rules is {}", num);

    let weakness = find_weakness(&input, num).expect("could not find encryption weakness");
    println!("encryption weakness is {}", weakness);
}

fn parse_args() -> (String, usize) {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("please specify input filename and preamble size");
    }

    let preamble: usize = args[2].parse().unwrap();
    (args.remove(1), preamble)
}

fn load_input(filename: &str) -> Vec<u64> {
    let contents = fs::read_to_string(filename).expect("error reading the file");

    let input: Vec<u64> = contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect();

    input
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
        *map.entry(val).or_default() += 1;
    }

    map
}

fn find_weakness(input: &[u64], target: u64) -> Option<u64> {
    for i in 0..input.len() {
        if let Some(range) = find_contiguous_sum(input, target, i) {
            let min = range.iter().min().unwrap();
            let max = range.iter().max().unwrap();
            return Some(min + max);
        }
    }
    None
}

fn find_contiguous_sum(input: &[u64], target: u64, start_ix: usize) -> Option<&[u64]> {
    let mut sum = 0;

    for i in start_ix..input.len() {
        sum += input[i];
        if sum == target {
            return Some(&input[start_ix..i + 1]);
        }
        if sum > target {
            return None;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let num = find_first_rulebreaker(&input, 5)
            .expect("could not find any numbers that break the rules");
        assert_eq!(num, 127);

        let weakness = find_weakness(&input, num).expect("could not find encryption weakness");
        assert_eq!(weakness, 62);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let num = find_first_rulebreaker(&input, 25)
            .expect("could not find any numbers that break the rules");
        assert_eq!(num, 14144619);

        let weakness = find_weakness(&input, num).expect("could not find encryption weakness");
        assert_eq!(weakness, 1766397);
    }
}
