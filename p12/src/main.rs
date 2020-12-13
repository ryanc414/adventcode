use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let final_distance_1 = find_final_manhattan_distance(&input, State1::new());
    println!("final distance is {}", final_distance_1);

    let final_distance_2 = find_final_manhattan_distance(&input, State2::new());
    println!("final distance is {}", final_distance_2);
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

trait State {
    fn process_instruction(&self, instr: &Instruction) -> Self;
    fn manhattan_distance(&self) -> i64;
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
}

impl State for State1 {
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

    fn manhattan_distance(&self) -> i64 {
        self.position.0.abs() + self.position.1.abs()
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

    fn turn(&self, degrees: i64, right: bool) -> (i64, i64) {
        if degrees % 90 != 0 {
            panic!("unexpected degrees value");
        }
        let turns = (degrees % 360) / 90;
        let mat = if !right {
            // println!(
            //     "turning left by {} degrees -> rotation matrix {}",
            //     degrees, turns
            // );
            ROTATION_MATRICES[turns as usize]
        } else {
            let i = ((4 - turns) % 4) as usize;
            // println!(
            //     "turning right by {} degrees -> rotation matrix {}",
            //     degrees, i
            // );
            ROTATION_MATRICES[i]
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
        match instr.action {
            Action::North => Self {
                position: self.position,
                waypoint: (self.waypoint.0, self.waypoint.1 + instr.value),
            },

            Action::South => Self {
                position: self.position,
                waypoint: (self.waypoint.0, self.waypoint.1 - instr.value),
            },

            Action::East => Self {
                position: self.position,
                waypoint: (self.waypoint.0 + instr.value, self.waypoint.1),
            },

            Action::West => Self {
                position: self.position,
                waypoint: (self.waypoint.0 - instr.value, self.waypoint.1),
            },

            Action::Left => Self {
                position: self.position,
                waypoint: self.turn(instr.value, false),
            },

            Action::Right => Self {
                position: self.position,
                waypoint: self.turn(instr.value, true),
            },

            Action::Forward => Self {
                position: (
                    self.position.0 + (self.waypoint.0 * instr.value),
                    self.position.1 + (self.waypoint.1 * instr.value),
                ),
                waypoint: self.waypoint,
            },
        }
    }

    fn manhattan_distance(&self) -> i64 {
        self.position.0.abs() + self.position.1.abs()
    }
}

fn find_final_manhattan_distance<S: State>(input: &[Instruction], mut state: S) -> i64 {
    for instr in input {
        state = state.process_instruction(instr);
    }

    state.manhattan_distance()
}
