use regex::Regex;
use std::collections::{vec_deque, HashSet, VecDeque};
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let basic_score = find_winner_score(&input, Box::new(play_round_basic));
    println!("winner score for basic combat is {}", basic_score);

    let mut deck_history = HashSet::new();
    let recursive_score = find_winner_score(
        &input,
        Box::new(move |decks| play_round_recursive(decks, &mut deck_history)),
    );
    println!("winner score for recursive combat is {}", recursive_score);
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Deck(VecDeque<usize>);

impl Deck {
    fn parse(input: &str) -> Self {
        let lines: Vec<&str> = input.split('\n').filter(|line| !line.is_empty()).collect();
        let re = Regex::new(r"^Player 1|2:$").unwrap();
        if !re.is_match(&lines[0]) {
            panic!("cannot parse input {}", &lines[0]);
        }

        let starter_deck: VecDeque<usize> = lines[1..]
            .iter()
            .map(|line| line.parse().unwrap())
            .collect();

        Self(starter_deck)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn play_card(&mut self) -> usize {
        self.0.pop_front().unwrap()
    }

    fn push_card(&mut self, card: usize) {
        self.0.push_back(card);
    }

    fn iter(&self) -> vec_deque::Iter<usize> {
        self.0.iter()
    }

    fn num_cards(&self) -> usize {
        self.0.len()
    }

    fn subdeck(&self, deck_size: usize) -> Self {
        let cards: VecDeque<usize> = self.0.iter().take(deck_size).cloned().collect();
        Self(cards)
    }
}

type PlayRoundFn = Box<dyn FnMut(&mut [Deck; 2]) -> Option<usize>>;

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

fn find_winner_score(starter_decks: &[Deck], play_round: PlayRoundFn) -> usize {
    let mut decks: [Deck; 2] = [starter_decks[0].clone(), starter_decks[1].clone()];
    let winner = play_game(&mut decks, play_round);
    calculate_winner_score(&decks[winner])
}

fn play_game(decks: &mut [Deck; 2], mut play_round: PlayRoundFn) -> usize {
    loop {
        if let Some(winner) = play_round(decks) {
            return winner;
        }
    }
}

fn play_round_basic(decks: &mut [Deck; 2]) -> Option<usize> {
    let cards = [decks[0].play_card(), decks[1].play_card()];
    let round_winner = if cards[0] > cards[1] { 0 } else { 1 };
    let round_loser = (round_winner + 1) % 2;

    decks[round_winner].push_card(cards[round_winner]);
    decks[round_winner].push_card(cards[round_loser]);

    if decks[round_loser].is_empty() {
        Some(round_winner)
    } else {
        None
    }
}

fn play_round_recursive(
    decks: &mut [Deck; 2],
    previous_decks: &mut HashSet<[Deck; 2]>,
) -> Option<usize> {
    if previous_decks.contains(decks) {
        return Some(0);
    }
    previous_decks.insert(decks.clone());

    let cards = [decks[0].play_card(), decks[1].play_card()];

    let round_winner = if decks[0].num_cards() >= cards[0] && decks[1].num_cards() >= cards[1] {
        // Recurse
        let mut subdecks = [decks[0].subdeck(cards[0]), decks[1].subdeck(cards[1])];
        let mut subgame_history = HashSet::new();

        play_game(
            &mut subdecks,
            Box::new(move |decks| play_round_recursive(decks, &mut subgame_history)),
        )
    } else if cards[0] > cards[1] {
        0
    } else {
        1
    };

    let round_loser = (1 + round_winner) % 2;

    decks[round_winner].push_card(cards[round_winner]);
    decks[round_winner].push_card(cards[round_loser]);

    if decks[round_loser].is_empty() {
        Some(round_winner)
    } else {
        None
    }
}

fn calculate_winner_score(winner_deck: &Deck) -> usize {
    winner_deck
        .iter()
        .rev()
        .enumerate()
        .map(|(i, &card)| ((i as usize) + 1) * card)
        .sum()
}
