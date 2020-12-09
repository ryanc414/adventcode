use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let acc_1 = find_acc_at_loop(&input);
    println!("just before loop begins, acc was {}", acc_1);

    let acc_2 = find_acc_at_termination(&input);
    println!("at termination, acc was {}", acc_2);
}

struct Instruction {
    cmd: Command,
    val: i32,
}

impl Instruction {
    fn parse(line: &str) -> Self {
        let parts: Vec<_> = line.split(' ').collect();
        if parts.len() != 2 {
            panic!("cannot parse line {}", line);
        }

        let cmd = match parts[0] {
            "nop" => Command::Nop,
            "acc" => Command::Acc,
            "jmp" => Command::Jmp,
            other => panic!("cannot parse command {}", other),
        };

        let val: i32 = parts[1].parse().unwrap();
        Self { cmd, val }
    }
}

enum Command {
    Nop,
    Jmp,
    Acc,
}

fn load_input() -> Vec<Instruction> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| Instruction::parse(line))
        .collect()
}

#[derive(Copy, Clone, Debug)]
struct ExecutionState {
    acc: i32,
    next_instr: usize,
}

impl ExecutionState {
    fn process_next_instr(&self, instructions: &[Instruction]) -> Self {
        let next_instr = &instructions[self.next_instr];
        self.process_instruction(next_instr)
    }

    fn process_instruction(&self, instruction: &Instruction) -> Self {
        match instruction.cmd {
            Command::Nop => Self {
                acc: self.acc,
                next_instr: self.next_instr + 1,
            },
            Command::Acc => Self {
                acc: self.acc + instruction.val,
                next_instr: self.next_instr + 1,
            },
            Command::Jmp => Self {
                acc: self.acc,
                next_instr: ((self.next_instr as i64) + (instruction.val as i64)) as usize,
            },
        }
    }
}

fn find_acc_at_loop(instructions: &[Instruction]) -> i32 {
    let mut state = ExecutionState {
        acc: 0,
        next_instr: 0,
    };
    let mut visited_instructions: HashSet<usize> = HashSet::new();

    while !visited_instructions.contains(&state.next_instr) {
        visited_instructions.insert(state.next_instr);
        state = state.process_next_instr(instructions);
    }

    state.acc
}

fn find_acc_at_termination(instructions: &[Instruction]) -> i32 {
    let state = ExecutionState {
        acc: 0,
        next_instr: 0,
    };
    let mut visited = HashSet::new();
    let state = find_termination_state(instructions, &state, &mut visited).unwrap();

    state.acc
}

fn find_termination_state(
    instructions: &[Instruction],
    state: &ExecutionState,
    visited: &mut HashSet<usize>,
) -> Option<ExecutionState> {
    if visited.contains(&state.next_instr) {
        return None;
    }
    visited.insert(state.next_instr);

    let next_instr = &instructions[state.next_instr];

    // First try to process the next instruction without change - if a termination
    // state is found we return it.
    let next_state = state.process_instruction(next_instr);
    if let Some(state) = find_termination_state(instructions, &next_state, visited) {
        return Some(state);
    }

    // If the next instruction is a Nop or Jmp, try swapping it to see if that
    // leads to a valid termination.
    match next_instr.cmd {
        Command::Nop => {
            let next_state = state.process_instruction(&Instruction {
                cmd: Command::Jmp,
                val: next_instr.val,
            });
            run_to_term_or_loop(instructions, &next_state, visited)
        }
        Command::Jmp => {
            let next_state = state.process_instruction(&Instruction {
                cmd: Command::Nop,
                val: next_instr.val,
            });
            run_to_term_or_loop(instructions, &next_state, visited)
        }
        Command::Acc => None,
    }
}

fn run_to_term_or_loop(
    instructions: &[Instruction],
    initial_state: &ExecutionState,
    visited: &mut HashSet<usize>,
) -> Option<ExecutionState> {
    let mut state = *initial_state;

    while !visited.contains(&state.next_instr) && state.next_instr != instructions.len() {
        visited.insert(state.next_instr);
        state = state.process_next_instr(instructions);
    }

    // Did we terminate successfully or hit a loop?
    if state.next_instr == instructions.len() {
        Some(state)
    } else {
        None
    }
}
