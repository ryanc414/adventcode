use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let sum = sum_group_counts(&input);
    println!("sum of group counts is {}", sum);
}

fn load_input() -> Vec<Vec<Vec<char>>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split("\n\n")
        .filter(|group| !group.is_empty())
        .map(|group: &str| -> Vec<Vec<char>> {
            group
                .split('\n')
                .filter(|line| !line.is_empty())
                .map(|line| line.chars().collect())
                .collect()
        })
        .collect()
}

fn sum_group_counts(groups: &[Vec<Vec<char>>]) -> usize {
    groups.iter().map(|group| count_unique(group)).sum()
}

fn count_unique(group: &[Vec<char>]) -> usize {
    let mut unique_elements = HashSet::new();

    for person in group {
        for answer in person {
            unique_elements.insert(answer);
        }
    }

    unique_elements.len()
}
