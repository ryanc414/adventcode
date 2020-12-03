use std::env;
use std::fs;

const TREE: char = '#';

fn main() {
    let input = load_input();
    let num_trees = count_path_trees(&input);
    println!("encountered {} trees", num_trees)
}

fn load_input() -> Vec<Vec<char>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split('\n')
        .filter(|line| line.len() > 0)
        .map(|line| line.chars().collect())
        .collect()
}

fn count_path_trees(input: &Vec<Vec<char>>) -> i32 {
    let mut count: i32 = 0;
    let mut col: usize = 0;

    for row in input {
        if row[col] == TREE {
            count += 1;
        }
        col = (col + 3) % row.len();
    }

    return count;
}
