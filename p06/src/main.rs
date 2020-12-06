use std::env;
use std::fs;
use std::iter::FromIterator;

fn main() {
    let input = load_input();
    let sum_1 = sum_all_counts(&input);
    println!("sum of all group counts is {}", sum_1);

    let sum_2 = sum_shared_counts(&input);
    println!("sum of shared group counts is {}", sum_2);
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

    fn inplace_intersection(&mut self, other: &AlphabetSet) {
        for i in 0..26 {
            if self.0[i] == 1 && other.0[i] == 0 {
                self.0[i] = 0;
            }
        }
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

fn sum_all_counts(groups: &[Vec<Vec<char>>]) -> usize {
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

fn sum_shared_counts(groups: &[Vec<Vec<char>>]) -> usize {
    groups.iter().map(|group| count_shared(group)).sum()
}

fn count_shared(group: &[Vec<char>]) -> usize {
    let mut shared_answers: AlphabetSet = group[0].iter().cloned().collect();

    for person in &group[1..] {
        let answers: AlphabetSet = person.iter().cloned().collect();
        shared_answers.inplace_intersection(&answers);

        // Short circuit if there are no shared answers - no point checking the
        // other answers from the group.
        if shared_answers.len() == 0 {
            return 0;
        }
    }

    shared_answers.len()
}
