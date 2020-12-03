use std::env;
use std::fs;

const TREE: char = '#';

const PATHS: [Path; 5] = [
    Path { right: 1, down: 1 },
    Path { right: 3, down: 1 },
    Path { right: 5, down: 1 },
    Path { right: 7, down: 1 },
    Path { right: 1, down: 2 },
];

fn main() {
    let input = load_input();
    let num_trees = count_path_trees(&input, &Path { right: 3, down: 1 });
    println!("encountered {} trees on default path", num_trees);

    let path_multiple = get_path_multiples(&input);
    println!(
        "multiple of number of trees on all paths is {}",
        path_multiple
    );
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

struct Path {
    right: usize,
    down: usize,
}

fn count_path_trees(input: &Vec<Vec<char>>, path: &Path) -> u64 {
    let mut count: u64 = 0;
    let mut col_ix: usize = 0;

    for row_ix in (0..input.len()).step_by(path.down) {
        let row = &input[row_ix];
        if row[col_ix] == TREE {
            count += 1;
        }
        col_ix = (col_ix + path.right) % row.len();
    }

    count
}

fn get_path_multiples(input: &Vec<Vec<char>>) -> u64 {
    PATHS
        .iter()
        .map(|path| count_path_trees(input, path))
        .product()
}
