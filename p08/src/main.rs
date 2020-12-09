use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let acc = find_acc_at_loop(&input);
    println!("just before loop begins, acc was {}", acc);
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

struct ExecutionState {
    acc: i32,
    next_instr: usize,
}

impl ExecutionState {
    fn process_next_instr(&self, instructions: &[Instruction]) -> Self {
        let next_instr = &instructions[self.next_instr];
        match next_instr.cmd {
            Command::Nop => Self {
                acc: self.acc,
                next_instr: self.next_instr + 1,
            },
            Command::Acc => Self {
                acc: self.acc + next_instr.val,
                next_instr: self.next_instr + 1,
            },
            Command::Jmp => Self {
                acc: self.acc,
                next_instr: ((self.next_instr as i64) + (next_instr.val as i64)) as usize,
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
