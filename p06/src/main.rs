use std::env;
use std::fs;
use std::iter::FromIterator;
use std::ops::{BitAnd, BitOr};

fn main() {
    let input = load_input();
    let sum_1 = sum_all_counts(&input);
    println!("sum of all group counts is {}", sum_1);

    let sum_2 = sum_shared_counts(&input);
    println!("sum of shared group counts is {}", sum_2);
}

fn load_input() -> Vec<Vec<AlphabetSet>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split("\n\n")
        .filter(|group| !group.is_empty())
        .map(|group: &str| -> Vec<AlphabetSet> {
            group
                .split('\n')
                .filter(|line| !line.is_empty())
                .map(|line| line.chars().collect())
                .collect()
        })
        .collect()
}

// Instead of using a general-purpose HashSet, we can implement a set based on
// a u32 whose 26 least significant bits represent the lowercase letters 'a' to
// 'z'. This saves on unnecessary allocations and hashing and makes calculations
// of unions and intersections cheap.
#[derive(Copy, Clone, Debug)]
struct AlphabetSet(u32);

const NUM_LETTERS: usize = 26;

impl AlphabetSet {
    fn new() -> AlphabetSet {
        AlphabetSet(0)
    }

    fn full() -> AlphabetSet {
        AlphabetSet((1 << NUM_LETTERS) - 1)
    }

    fn insert(&mut self, element: char) {
        if element < 'a' || element > 'z' {
            panic!("cannot insert element {}", element);
        }

        let ix: usize = element as usize - 'a' as usize;
        self.0 |= 1 << ix;
    }

    fn len(&self) -> usize {
        let mut count: usize = 0;

        for i in 0..NUM_LETTERS {
            if self.0 & (1 << i) != 0 {
                count += 1;
            }
        }

        count
    }

    fn is_empty(&self) -> bool {
        self.0 == 0
    }
}

impl FromIterator<char> for AlphabetSet {
    fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> Self {
        let mut set = AlphabetSet::new();

        for c in iter {
            set.insert(c);
        }

        set
    }
}

impl BitAnd for AlphabetSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitOr for AlphabetSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

fn sum_all_counts(groups: &[Vec<AlphabetSet>]) -> usize {
    groups.iter().map(|group| count_unique(group)).sum()
}

fn count_unique(group: &[AlphabetSet]) -> usize {
    let mut unique_answers = AlphabetSet::new();

    for &person in group {
        unique_answers = unique_answers | person;
    }

    unique_answers.len()
}

fn sum_shared_counts(groups: &[Vec<AlphabetSet>]) -> usize {
    groups.iter().map(|group| count_shared(group)).sum()
}

fn count_shared(group: &[AlphabetSet]) -> usize {
    let mut shared_answers = AlphabetSet::full();

    for &person in group {
        shared_answers = shared_answers & person;

        // Short circuit if there are no shared answers - no point checking the
        // other answers from the group.
        if shared_answers.is_empty() {
            return 0;
        }
    }

    shared_answers.len()
}
