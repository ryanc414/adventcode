use std::env;
use std::fs;
use std::iter::FromIterator;
use std::ops::{BitAnd, BitOr};

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let sum_1 = sum_all_counts(&input);
    println!("sum of all group counts is {}", sum_1);

    let sum_2 = sum_shared_counts(&input);
    println!("sum of shared group counts is {}", sum_2);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    args.remove(1)
}

fn load_input(filename: &str) -> Vec<Vec<AlphabetSet>> {
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
        (0..NUM_LETTERS).filter(|i| self.0 & (1 << i) != 0).count()
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
    group
        .iter()
        .fold(AlphabetSet::new(), |unique_answers, &person| {
            unique_answers | person
        })
        .len()
}

fn sum_shared_counts(groups: &[Vec<AlphabetSet>]) -> usize {
    groups.iter().map(|group| count_shared(group)).sum()
}

fn count_shared(group: &[AlphabetSet]) -> usize {
    group
        .iter()
        .fold(AlphabetSet::full(), |shared_answers, &person| {
            shared_answers & person
        })
        .len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let sum = sum_all_counts(&input);
        assert_eq!(sum, 11);

        let sum = sum_shared_counts(&input);
        assert_eq!(sum, 6);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let sum = sum_all_counts(&input);
        assert_eq!(sum, 6249);

        let sum = sum_shared_counts(&input);
        assert_eq!(sum, 3103);
    }
}
