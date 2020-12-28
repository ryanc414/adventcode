use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let black_tiles = initial_black_tiles(&input);
    println!("{} black tiles are flipped", black_tiles.len());

    let final_tiles = simulate_days(black_tiles, 100);
    println!(
        "{} black tiles are flipped after 100 days",
        final_tiles.len()
    );
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    fn neighbours(&self) -> [Self; 6] {
        [
            Self {
                north_east: self.north_east + 1,
                east: self.east,
            },
            Self {
                north_east: self.north_east - 1,
                east: self.east,
            },
            Self {
                north_east: self.north_east,
                east: self.east + 1,
            },
            Self {
                north_east: self.north_east,
                east: self.east - 1,
            },
            Self {
                north_east: self.north_east + 1,
                east: self.east - 1,
            },
            Self {
                north_east: self.north_east - 1,
                east: self.east + 1,
            },
        ]
    }

    fn black_neighbours_count(&self, black_tiles: &HashSet<Self>) -> usize {
        self.neighbours()
            .iter()
            .filter(|neighbour| black_tiles.contains(neighbour))
            .count()
    }
}

fn normalise_directions(directions: &[Vec<Direction>]) -> Vec<NormalisedDirections> {
    directions
        .iter()
        .map(|dirs| NormalisedDirections::convert(dirs))
        .collect()
}

fn initial_black_tiles(directions: &[Vec<Direction>]) -> HashSet<NormalisedDirections> {
    let normalised = normalise_directions(directions);

    let mut black_tiles: HashSet<NormalisedDirections> = HashSet::new();
    for dir in normalised {
        if black_tiles.contains(&dir) {
            black_tiles.remove(&dir);
        } else {
            black_tiles.insert(dir);
        }
    }

    black_tiles
}

fn simulate_days(
    mut black_tiles: HashSet<NormalisedDirections>,
    num_days: usize,
) -> HashSet<NormalisedDirections> {
    for _ in 0..num_days {
        black_tiles = simulate_day(black_tiles);
    }

    black_tiles
}

fn simulate_day(black_tiles: HashSet<NormalisedDirections>) -> HashSet<NormalisedDirections> {
    let mut updated_tiles = black_tiles.clone();

    for tile in black_tiles.iter() {
        for neighbour in tile.neighbours().iter() {
            if !black_tiles.contains(neighbour)
                && neighbour.black_neighbours_count(&black_tiles) == 2
            {
                updated_tiles.insert(*neighbour);
            }
        }

        let count = tile.black_neighbours_count(&black_tiles);
        if count == 0 || count > 2 {
            updated_tiles.remove(&tile);
        }
    }

    updated_tiles
}
