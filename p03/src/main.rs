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
    let filename = parse_args();
    let input = load_input(&filename);

    let num_trees = count_path_trees(&input, &Path { right: 3, down: 1 });
    println!("encountered {} trees on default path", num_trees);

    let path_multiple = get_path_multiples(&input);
    println!(
        "multiple of number of trees on all paths is {}",
        path_multiple
    );
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    args.remove(1)
}

fn load_input(filename: &str) -> Vec<Vec<char>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let res = count_path_trees(&input, &Path { right: 3, down: 1 });
        assert_eq!(res, 7);

        let res = get_path_multiples(&input);
        assert_eq!(res, 336);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let res = count_path_trees(&input, &Path { right: 3, down: 1 });
        assert_eq!(res, 178);

        let res = get_path_multiples(&input);
        assert_eq!(res, 3492520200);
    }
}
