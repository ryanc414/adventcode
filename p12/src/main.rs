use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let final_distance = find_final_manhattan_distance(&input);
    println!("final distance is {}", final_distance);
}

struct Instruction {
    action: Action,
    value: i64,
}

impl Instruction {
    fn parse(line: &str) -> Self {
        Self {
            action: Action::parse(line.chars().next().unwrap()),
            value: line[1..].parse().unwrap(),
        }
    }
}

#[derive(Debug)]
enum Action {
    North,
    South,
    East,
    West,
    Left,
    Right,
    Forward,
}

impl Action {
    fn parse(c: char) -> Self {
        match c {
            'N' => Action::North,
            'S' => Action::South,
            'E' => Action::East,
            'W' => Action::West,
            'L' => Action::Left,
            'R' => Action::Right,
            'F' => Action::Forward,
            _ => panic!("unexpected action input {}", c),
        }
    }
}

fn load_input() -> Vec<Instruction> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("error reading input file");
    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(Instruction::parse)
        .collect()
}

#[derive(Debug)]
struct State {
    position: (i64, i64),
    facing: usize,
}

const DIRECTIONS: [(i64, i64); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

impl State {
    fn new() -> Self {
        State {
            position: (0, 0),
            facing: 0,
        }
    }

    fn process_instruction(&self, instr: &Instruction) -> Self {
        match instr.action {
            Action::North => Self {
                position: (self.position.0, self.position.1 + instr.value),
                facing: self.facing,
            },
            Action::South => Self {
                position: (self.position.0, self.position.1 - instr.value),
                facing: self.facing,
            },
            Action::East => Self {
                position: (self.position.0 + instr.value, self.position.1),
                facing: self.facing,
            },
            Action::West => Self {
                position: (self.position.0 - instr.value, self.position.1),
                facing: self.facing,
            },
            Action::Left => Self {
                position: self.position,
                facing: self.turn(instr.value, false),
            },
            Action::Right => Self {
                position: self.position,
                facing: self.turn(instr.value, true),
            },
            Action::Forward => Self {
                position: (
                    self.position.0 + (DIRECTIONS[self.facing].0 * instr.value),
                    self.position.1 + (DIRECTIONS[self.facing].1 * instr.value),
                ),
                facing: self.facing,
            },
        }
    }

    fn turn(&self, degrees: i64, right: bool) -> usize {
        if degrees % 90 != 0 {
            panic!("unexpected turn value {}", degrees);
        }

        if !right {
            ((self.facing as i64 + (degrees / 90)) % 4) as usize
        } else {
            let a = self.facing as i64 - ((degrees % 360) / 90);
            ((a + 4) % 4) as usize
        }
    }

    fn manhattan_distance(&self) -> i64 {
        self.position.0.abs() + self.position.1.abs()
    }
}

fn find_final_manhattan_distance(input: &[Instruction]) -> i64 {
    let mut state = State::new();

    for instr in input {
        state = state.process_instruction(instr);
    }

    state.manhattan_distance()
}
