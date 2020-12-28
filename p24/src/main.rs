use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let count = count_black_tiles(&input);
    println!("{} black tiles are flipped", count);
}

enum Direction {
    East,
    SouthEast,
    SouthWest,
    West,
    NorthWest,
    NorthEast,
}

fn load_input() -> Vec<Vec<Direction>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read from file");
    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parse_directions(line))
        .collect()
}

fn parse_directions(line: &str) -> Vec<Direction> {
    let mut it = line.chars();
    let mut directions = Vec::new();

    loop {
        let c = it.next();
        if c.is_none() {
            return directions;
        }

        match c.unwrap() {
            'e' => directions.push(Direction::East),
            'w' => directions.push(Direction::West),
            's' => match it.next().unwrap() {
                'e' => directions.push(Direction::SouthEast),
                'w' => directions.push(Direction::SouthWest),
                _ => panic!("cannot parse line {}", line),
            },
            'n' => match it.next().unwrap() {
                'e' => directions.push(Direction::NorthEast),
                'w' => directions.push(Direction::NorthWest),
                _ => panic!("cannot parse line {}", line),
            },
            _ => panic!("cannot parse line {}", line),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct NormalisedDirections {
    north_east: isize,
    east: isize,
}

impl NormalisedDirections {
    fn convert(dirs: &[Direction]) -> Self {
        let mut normalised = NormalisedDirections {
            east: 0,
            north_east: 0,
        };

        for dir in dirs {
            match dir {
                Direction::East => normalised.east += 1,
                Direction::West => normalised.east -= 1,
                Direction::NorthEast => normalised.north_east += 1,
                Direction::SouthWest => normalised.north_east -= 1,
                Direction::NorthWest => {
                    normalised.north_east += 1;
                    normalised.east -= 1;
                }
                Direction::SouthEast => {
                    normalised.north_east -= 1;
                    normalised.east += 1;
                }
            }
        }

        normalised
    }
}

fn normalise_directions(directions: &[Vec<Direction>]) -> Vec<NormalisedDirections> {
    directions
        .iter()
        .map(|dirs| NormalisedDirections::convert(dirs))
        .collect()
}

fn count_black_tiles(directions: &[Vec<Direction>]) -> usize {
    let normalised = normalise_directions(directions);

    let mut flip_counts: HashMap<NormalisedDirections, usize> = HashMap::new();
    for dir in normalised {
        let counter = flip_counts.entry(dir).or_default();
        *counter += 1;
    }

    flip_counts.values().filter(|&count| count % 2 == 1).count()
}
