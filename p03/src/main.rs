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
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect())
        .collect()
}

struct Path {
    right: usize,
    down: usize,
}

fn count_path_trees(input: &[Vec<char>], path: &Path) -> u64 {
    let (count, _) = input
        .iter()
        .step_by(path.down)
        .fold((0, 0), |(count, col_ix), row| {
            let next_col_ix = (col_ix + path.right) % row.len();
            if row[col_ix] == TREE {
                (count + 1, next_col_ix)
            } else {
                (count, next_col_ix)
            }
        });
    count
}

fn get_path_multiples(input: &[Vec<char>]) -> u64 {
    PATHS
        .iter()
        .map(|path| count_path_trees(input, path))
        .product()
}
