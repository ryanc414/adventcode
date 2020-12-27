use std::collections::{HashMap, HashSet};
use std::env;

fn main() {
    let cups = load_input();

    let final_labels = find_final_labels(&cups, 100);
    println!("after 100 moves the final cup labels are {}", final_labels);

    let product = find_final_product(&cups, 1000000, 10000000);
    println!(
        "after 10,000,000 moves, the product of the labels after 1 is {}",
        product
    );
}

#[derive(Debug, Clone)]
struct CupCircle {
    cups: HashMap<u32, u32>,
    curr_cup: u32,
    range: (u32, u32),
}

impl CupCircle {
    fn from_labels(labels: &[u32]) -> Self {
        let cups: HashMap<u32, u32> = labels
            .iter()
            .enumerate()
            .map(|(i, &l)| (l, labels[(i + 1) % labels.len()]))
            .collect();

        let min_cup = *cups.keys().min().unwrap();
        let max_cup = *cups.keys().max().unwrap();

        Self {
            cups,
            curr_cup: labels[0],
            range: (min_cup, max_cup),
        }
    }

    fn start_from_labels(labels: &[u32], limit: u32) -> Self {
        let mut cups: HashMap<u32, u32> = labels
            .iter()
            .take(labels.len() - 1)
            .enumerate()
            .map(|(i, &l)| (l, labels[i + 1]))
            .collect();

        let min_cup = *cups.keys().min().unwrap();
        let max_cup = *cups.keys().max().unwrap();
        cups.insert(labels[labels.len() - 1], max_cup + 1);

        for i in (max_cup + 1)..limit {
            cups.insert(i, i + 1);
        }
        cups.insert(limit, labels[0]);

        Self {
            cups,
            curr_cup: labels[0],
            range: (min_cup, limit),
        }
    }

    fn simulate_move(&mut self) {
        let (picked_cups, next_cup) = self.pop_next_cups();
        self.cups.insert(self.curr_cup, next_cup);

        let destination_cup = self.find_destination_cup(self.curr_cup, &picked_cups);
        self.insert_after(destination_cup, picked_cups);

        self.curr_cup = *self.cups.get(&self.curr_cup).unwrap();
        // self.check_integrity();
    }

    fn pop_next_cups(&mut self) -> (Vec<u32>, u32) {
        let mut picked_cups: Vec<u32> = Vec::new();
        let mut cup = *self.cups.get(&self.curr_cup).unwrap();

        for _ in 0..3 {
            picked_cups.push(cup);
            cup = *self.cups.get(&cup).unwrap();
        }

        (picked_cups, cup)
    }

    fn check_integrity(&self) {
        let mut cup = self.curr_cup;
        let mut seen_cups: HashSet<u32> = HashSet::new();

        while !seen_cups.contains(&cup) {
            seen_cups.insert(cup);
            cup = *self.cups.get(&cup).unwrap();
        }

        assert_eq!(seen_cups.len(), self.cups.len());
    }

    fn find_destination_cup(&self, curr_cup: u32, picked_cups: &[u32]) -> u32 {
        let target = if curr_cup > self.range.0 {
            curr_cup - 1
        } else {
            self.range.1
        };

        if picked_cups.contains(&target) {
            self.find_destination_cup(target, picked_cups)
        } else {
            target
        }
    }

    fn insert_after(&mut self, destination_cup: u32, insert_cups: Vec<u32>) {
        let after_insert = *self.cups.get(&destination_cup).unwrap();

        let mut curr_cup = destination_cup;
        for next_insert in insert_cups {
            self.cups.insert(curr_cup, next_insert);
            curr_cup = next_insert;
        }

        self.cups.insert(curr_cup, after_insert);
    }

    fn collect_labels(&self) -> String {
        let mut cup = *self.cups.get(&1).unwrap();
        let mut cup_labels = String::new();

        while cup != 1 {
            cup_labels.push(((cup + ('0' as u32)) as u8) as char);
            cup = *self.cups.get(&cup).unwrap();
        }

        cup_labels
    }

    fn neighbours_product(&self) -> u64 {
        let cup_1 = *self.cups.get(&1).unwrap();
        let cup_2 = *self.cups.get(&cup_1).unwrap();
        println!("neighbours: {}, {}", cup_1, cup_2);

        (cup_1 as u64) * (cup_2 as u64)
    }
}

fn load_input() -> Vec<u32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input cup labels and number of moves")
    }

    args[1].chars().map(|c| (c as u32) - ('0' as u32)).collect()
}

fn find_final_labels(cup_labels: &[u32], num_moves: usize) -> String {
    let mut cups = CupCircle::from_labels(cup_labels);

    for _ in 0..num_moves {
        cups.simulate_move();
    }

    cups.collect_labels()
}

fn find_final_product(cup_labels: &[u32], num_cups: u32, num_moves: usize) -> u64 {
    let mut cups = CupCircle::start_from_labels(cup_labels, num_cups);

    for _ in 0..num_moves {
        cups.simulate_move();
    }

    cups.neighbours_product()
}
