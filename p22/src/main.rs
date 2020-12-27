use regex::Regex;
use std::collections::{vec_deque, VecDeque};
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let score = find_winner_score(&input);
    println!("winner score is {}", score);
}

#[derive(Clone)]
struct Deck(VecDeque<u64>);

impl Deck {
    fn parse(input: &str) -> Self {
        let lines: Vec<&str> = input.split('\n').filter(|line| !line.is_empty()).collect();
        let re = Regex::new(r"^Player 1|2:$").unwrap();
        if !re.is_match(&lines[0]) {
            panic!("cannot parse input {}", &lines[0]);
        }

        let starter_deck: VecDeque<u64> = lines[1..]
            .iter()
            .map(|line| line.parse().unwrap())
            .collect();

        Self(starter_deck)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn play_card(&mut self) -> u64 {
        self.0.pop_front().unwrap()
    }

    fn push_card(&mut self, card: u64) {
        self.0.push_back(card);
    }

    fn iter(&self) -> vec_deque::Iter<u64> {
        self.0.iter()
    }
}

fn load_input() -> [Deck; 2] {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read input file");
    let sections: Vec<&str> = contents
        .split("\n\n")
        .filter(|section| !section.is_empty())
        .collect();

    if sections.len() != 2 {
        panic!("unexpected number of sections");
    }

    [Deck::parse(&sections[0]), Deck::parse(&sections[1])]
}

fn find_winner_score(starter_decks: &[Deck]) -> u64 {
    let mut decks: [Deck; 2] = [starter_decks[0].clone(), starter_decks[1].clone()];

    while !decks[0].is_empty() && !decks[1].is_empty() {
        play_round(&mut decks);
    }

    calculate_winner_score(&decks)
}

fn play_round(decks: &mut [Deck]) {
    let card_1 = decks[0].play_card();
    let card_2 = decks[1].play_card();

    if card_1 > card_2 {
        decks[0].push_card(card_1);
        decks[0].push_card(card_2);
    } else {
        decks[1].push_card(card_2);
        decks[1].push_card(card_1);
    }
}

fn calculate_winner_score(decks: &[Deck]) -> u64 {
    let winner_deck = if decks[0].is_empty() {
        &decks[1]
    } else {
        &decks[0]
    };

    winner_deck
        .iter()
        .rev()
        .enumerate()
        .map(|(i, &card)| ((i as u64) + 1) * card)
        .sum()
}
