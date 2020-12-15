use std::collections::HashMap;
use std::env;

fn main() {
    let input = load_input();

    let number = get_nth_number(&input, 2020);
    println!("2020th spoken number is {}", number);
}

fn load_input() -> Vec<usize> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify comma-separated list of starter numbers");
    }

    args[1].split(',').map(|num| num.parse().unwrap()).collect()
}

fn get_nth_number(starter_nums: &[usize], target_n: usize) -> usize {
    let mut last_seen: HashMap<usize, usize> = HashMap::new();
    let mut last_num: usize = *starter_nums.first().unwrap();

    let mut n = 2;
    for &num in &starter_nums[1..] {
        last_seen.insert(last_num, n - 1);
        last_num = num;
        n += 1;
    }

    while n <= target_n {
        let next_num = match last_seen.get(&last_num) {
            Some(i) => n - 1 - i,
            None => 0,
        };
        last_seen.insert(last_num, n - 1);
        last_num = next_num;
        n += 1;
    }

    last_num
}
