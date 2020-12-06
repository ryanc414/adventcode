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

// Instead of using a general-purpose HashSet, we can implement a set based on
// a 26-element array since we know that all answers will be lowercase letters
// 'a' to 'z'. This saves on unnecessary allocations and hashing.
struct AlphabetSet([u8; 26]);

impl AlphabetSet {
    fn new() -> AlphabetSet {
        AlphabetSet([0; 26])
    }

    fn insert(&mut self, element: char) {
        if element < 'a' || element > 'z' {
            panic!("cannot insert element {}", element);
        }

        let ix: usize = element as usize - 'a' as usize;
        self.0[ix] = 1;
    }

    fn len(&self) -> usize {
        self.0.iter().sum::<u8>() as usize
    }
}

fn sum_group_counts(groups: &[Vec<Vec<char>>]) -> usize {
    groups.iter().map(|group| count_unique(group)).sum()
}

fn count_unique(group: &[Vec<char>]) -> usize {
    let mut unique_elements = AlphabetSet::new();

    for person in group {
        for &answer in person {
            unique_elements.insert(answer);
        }
    }

    unique_elements.len()
}
