use std::collections::HashMap;
use std::env;

fn main() {
    let (cups, num_moves) = load_input();

    let final_labels = find_final_labels(&cups, num_moves);
    println!(
        "after {} moves the final cup labels are {}",
        num_moves, final_labels
    );
}

#[derive(Debug, Clone)]
struct CupCircle {
    cups: HashMap<u32, u32>,
    curr_cup: u32,
    range: (u32, u32),
}

impl CupCircle {
    fn parse(input: &str) -> Self {
        let labels: Vec<u32> = input.chars().map(|c| (c as u32) - ('0' as u32)).collect();
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

    fn simulate_move(&mut self) {
        let (picked_cups, next_cup) = self.pop_next_cups();
        self.cups.insert(self.curr_cup, next_cup);

        let destination_cup = self.find_destination_cup(self.curr_cup, &picked_cups);
        self.insert_after(destination_cup, picked_cups);

        self.curr_cup = *self.cups.get(&self.curr_cup).unwrap();
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
}

fn load_input() -> (CupCircle, usize) {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("please specify input cup labels and number of moves")
    }

    (CupCircle::parse(&args[1]), args[2].parse().unwrap())
}

fn find_final_labels(initial_cups: &CupCircle, num_moves: usize) -> String {
    let mut cups: CupCircle = initial_cups.clone();

    for _ in 0..num_moves {
        cups.simulate_move();
    }

    cups.collect_labels()
}
