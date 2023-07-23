use std::env;
use std::fs;
use std::collections;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename).unwrap();

    let count = find_marker(&input, 4).unwrap();
    println!("packet marker: {}", count);

    let count = find_marker(&input, 14).unwrap();
    println!("message marker: {}", count);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

fn load_input(filename: &str) -> Result<Vec<char>, String> {
    let contents = fs::read_to_string(filename).map_err(|e| e.to_string())?;
    Ok(contents.trim().chars().collect())
}

fn find_marker(input: &[char], len: usize) -> Result<usize, String> {
    let mut last = collections::VecDeque::new();

    for (i, &c) in input.iter().enumerate() {
        last.push_back(c);
        if last.len() > len {
            last.pop_front();
        }

        if last.len() == len && all_unique(&last) {
            return Ok(i + 1)
        }
    }

    Err("not found".to_string())
}

fn all_unique(last: &collections::VecDeque<char>) -> bool {
    let set: collections::HashSet<char> = collections::HashSet::from_iter(last.iter().cloned());
    return set.len() == last.len()
}
