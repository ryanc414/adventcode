use std::collections::HashMap;
use std::env;

fn main() {
    let input = parse_args();

    let number_1 = get_nth_number(&input, 2020);
    println!("2020th spoken number is {}", number_1);

    let number_2 = get_nth_number(&input, 30000000);
    println!("30000000th spoken number is {}", number_2);
}

fn parse_args() -> Vec<usize> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify comma-separated list of starter numbers");
    }

    args[1].split(',').map(|num| num.parse().unwrap()).collect()
}

// Possibly there is a smarter solution for large target_n e.g. identifying
// and exploiting loops. But the dumb brute-force approach runs in just a few
// seconds even for the larger part 2 input, when compiled in release mode.
fn get_nth_number(starter_nums: &[usize], target_n: usize) -> usize {
    let mut last_seen: HashMap<usize, usize> = HashMap::new();
    let mut last_num: usize = *starter_nums.first().unwrap();

    let mut n = 1;
    for &num in &starter_nums[1..] {
        last_seen.insert(last_num, n);
        last_num = num;
        n += 1;
    }

    while n < target_n {
        let next_num = match last_seen.get(&last_num) {
            Some(i) => n - i,
            None => 0,
        };
        last_seen.insert(last_num, n);
        last_num = next_num;
        n += 1;
    }

    last_num
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = vec![1, 3, 2];

        let num = get_nth_number(&input, 2020);
        assert_eq!(num, 1);

        let num = get_nth_number(&input, 30000000);
        assert_eq!(num, 2578);
    }

    #[test]
    fn test_full_input() {
        let input = vec![13, 16, 0, 12, 15, 1];

        let num = get_nth_number(&input, 2020);
        assert_eq!(num, 319);

        let num = get_nth_number(&input, 30000000);
        assert_eq!(num, 2424);
    }
}
