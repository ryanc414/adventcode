use std::env;
use std::fs;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let final_distance_1 = find_final_manhattan_distance(&input, State1::new());
    println!("final distance is {}", final_distance_1);

    let final_distance_2 = find_final_manhattan_distance(&input, State2::new());
    println!("final distance is {}", final_distance_2);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    args.remove(1)
}
enum Instruction {
    North(i64),
    South(i64),
    East(i64),
    West(i64),
    Left(usize),
    Right(usize),
    Forward(i64),
}

impl Instruction {
    fn parse(line: &str) -> Self {
        let val: i64 = line[1..].parse().unwrap();

        match line.chars().next().unwrap() {
            'N' => Instruction::North(val),
            'S' => Instruction::South(val),
            'E' => Instruction::East(val),
            'W' => Instruction::West(val),
            'L' => Instruction::Left(Self::convert_turn_val(val)),
            'R' => Instruction::Right(Self::convert_turn_val(val)),
            'F' => Instruction::Forward(val),
            c => panic!("unexpected Instruction input {}", c),
        }
    }

    fn convert_turn_val(val: i64) -> usize {
        if val < 0 || val % 90 != 0 {
            panic!("unexpected valut for Left turn")
        }
        ((val as usize) / 90) % 360
    }
}

fn load_input(filename: &str) -> Vec<Instruction> {
    let contents = fs::read_to_string(filename).expect("error reading input file");
    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(Instruction::parse)
        .collect()
}

trait State {
    fn process_instruction(&self, instr: &Instruction) -> Self;
    fn get_position(&self) -> (i64, i64);

    fn manhattan_distance(&self) -> i64 {
        let position = self.get_position();
        position.0.abs() + position.1.abs()
    }
}

#[derive(Debug)]
struct State1 {
    position: (i64, i64),
    facing: usize,
}

const DIRECTIONS: [(i64, i64); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

impl State1 {
    fn new() -> Self {
        Self {
            position: (0, 0),
            facing: 0,
        }
    }

    fn turn(&self, turns: usize, right: bool) -> usize {
        if !right {
            (self.facing + turns) % 4
        } else {
            (self.facing + 4 - turns) % 4
        }
    }
}

impl State for State1 {
    fn process_instruction(&self, instr: &Instruction) -> Self {
        match instr {
            Instruction::North(val) => Self {
                position: (self.position.0, self.position.1 + val),
                facing: self.facing,
            },
            Instruction::South(val) => Self {
                position: (self.position.0, self.position.1 - val),
                facing: self.facing,
            },
            Instruction::East(val) => Self {
                position: (self.position.0 + val, self.position.1),
                facing: self.facing,
            },
            Instruction::West(val) => Self {
                position: (self.position.0 - val, self.position.1),
                facing: self.facing,
            },
            Instruction::Left(val) => Self {
                position: self.position,
                facing: self.turn(*val, false),
            },
            Instruction::Right(val) => Self {
                position: self.position,
                facing: self.turn(*val, true),
            },
            Instruction::Forward(val) => Self {
                position: (
                    self.position.0 + (DIRECTIONS[self.facing].0 * val),
                    self.position.1 + (DIRECTIONS[self.facing].1 * val),
                ),
                facing: self.facing,
            },
        }
    }

    fn get_position(&self) -> (i64, i64) {
        self.position
    }
}

struct State2 {
    position: (i64, i64),
    waypoint: (i64, i64),
}

impl State2 {
    fn new() -> Self {
        Self {
            position: (0, 0),
            waypoint: (10, 1),
        }
    }

    fn turn(&self, turns: usize, right: bool) -> (i64, i64) {
        let mat = if !right {
            ROTATION_MATRICES[turns]
        } else {
            ROTATION_MATRICES[(4 - turns) % 4]
        };

        (
            (self.waypoint.0 * mat[0][0]) + (self.waypoint.1 * mat[0][1]),
            (self.waypoint.0 * mat[1][0]) + (self.waypoint.1 * mat[1][1]),
        )
    }
}

const ROTATION_MATRICES: [[[i64; 2]; 2]; 4] = [
    [[1, 0], [0, 1]],   // 0 degrees
    [[0, -1], [1, 0]],  // 90 degrees
    [[-1, 0], [0, -1]], // 180 degrees
    [[0, 1], [-1, 0]],  // 270 degrees
];

impl State for State2 {
    fn process_instruction(&self, instr: &Instruction) -> Self {
        match instr {
            Instruction::North(val) => Self {
                position: self.position,
                waypoint: (self.waypoint.0, self.waypoint.1 + val),
            },

            Instruction::South(val) => Self {
                position: self.position,
                waypoint: (self.waypoint.0, self.waypoint.1 - val),
            },

            Instruction::East(val) => Self {
                position: self.position,
                waypoint: (self.waypoint.0 + val, self.waypoint.1),
            },

            Instruction::West(val) => Self {
                position: self.position,
                waypoint: (self.waypoint.0 - val, self.waypoint.1),
            },

            Instruction::Left(val) => Self {
                position: self.position,
                waypoint: self.turn(*val, false),
            },

            Instruction::Right(val) => Self {
                position: self.position,
                waypoint: self.turn(*val, true),
            },

            Instruction::Forward(val) => Self {
                position: (
                    self.position.0 + (self.waypoint.0 * val),
                    self.position.1 + (self.waypoint.1 * val),
                ),
                waypoint: self.waypoint,
            },
        }
    }

    fn get_position(&self) -> (i64, i64) {
        self.position
    }
}

fn find_final_manhattan_distance<S: State>(input: &[Instruction], mut state: S) -> i64 {
    for instr in input {
        state = state.process_instruction(instr);
    }

    state.manhattan_distance()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let distance = find_final_manhattan_distance(&input, State1::new());
        assert_eq!(distance, 25);

        let distance = find_final_manhattan_distance(&input, State2::new());
        assert_eq!(distance, 286);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let distance = find_final_manhattan_distance(&input, State1::new());
        assert_eq!(distance, 439);

        let distance = find_final_manhattan_distance(&input, State2::new());
        assert_eq!(distance, 12385);
    }
}
