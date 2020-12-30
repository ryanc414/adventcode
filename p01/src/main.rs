use std::collections::HashMap;
use std::env;
use std::fs;

const TARGET: i32 = 2020;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    match find_two_multiple(input.clone()) {
        Some(x) => println!("first result: {}", x),
        None => println!("no result found for first part"),
    }

    match find_three_multiple(input) {
        Some(x) => println!("second result: {}", x),
        None => println!("no result found for second part"),
    }
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    args.remove(1)
}

fn load_input(filename: &str) -> HashMap<i32, usize> {
    let contents = fs::read_to_string(filename).expect("error reading the file");

    let numbers = contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().unwrap())
        .collect();

    input_to_map(numbers)
}

fn input_to_map(input: Vec<i32>) -> HashMap<i32, usize> {
    let mut counts: HashMap<i32, usize> = HashMap::new();

    for n in input {
        let counter = counts.entry(n).or_default();
        *counter += 1;
    }

    counts
}

fn find_three_multiple(mut input: HashMap<i32, usize>) -> Option<i64> {
    let vals: Vec<i32> = input.keys().cloned().collect();

    for x in vals {
        if x > TARGET {
            continue;
        }

        // Temporarily remove x from the hashmap so that we don't double count
        // it.
        update_counter(&mut input, x, false);

        let rest = TARGET - x;

        if let Some((y, z)) = find_two_sum(&mut input, rest) {
            return Some(x as i64 * y as i64 * z as i64);
        }

        update_counter(&mut input, x, true);
    }

    None
}

fn find_two_multiple(mut input: HashMap<i32, usize>) -> Option<i64> {
    match find_two_sum(&mut input, TARGET) {
        Some((x, y)) => Some(x as i64 * y as i64),
        None => None,
    }
}

fn find_two_sum(input: &mut HashMap<i32, usize>, target: i32) -> Option<(i32, i32)> {
    let vals: Vec<i32> = input.keys().cloned().collect();

    for x in vals {
        if x > target {
            continue;
        }

        // We should skip this value if its counter is 0.
        if *input.get(&x).unwrap() == 0 {
            continue;
        }

        // Temporarily remove x from the hashmap so that we don't double count
        // it.
        update_counter(input, x, false);

        let y = target - x;
        if let Some(&count) = input.get(&y) {
            if count > 0 {
                return Some((x, y));
            }
        }

        update_counter(input, x, true);
    }

    None
}

fn update_counter(input_map: &mut HashMap<i32, usize>, val: i32, increment: bool) {
    let counter = input_map.get_mut(&val).unwrap();
    if increment {
        *counter += 1;
    } else {
        *counter -= 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let res = find_two_multiple(input.clone());
        assert_eq!(res, Some(514579));

        let res = find_three_multiple(input);
        assert_eq!(res, Some(241861950));
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let res = find_two_multiple(input.clone());
        assert_eq!(res, Some(913824));

        let res = find_three_multiple(input);
        assert_eq!(res, Some(240889536));
    }
}
