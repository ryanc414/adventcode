use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let mem_sum = find_final_mem_sum(&input);
    println!("final sum of memory values is {}", mem_sum);
}

struct InstructionParser {
    instruction_re: Regex,
    mem_re: Regex,
}

impl InstructionParser {
    fn new() -> Self {
        let instruction_re = Regex::new(r"^(mask|mem\[\d+\]) = ([0-9X]+)$").unwrap();
        let mem_re = Regex::new(r"^mem\[(\d+)\]$").unwrap();

        Self {
            instruction_re,
            mem_re,
        }
    }

    fn parse_line(&self, line: &str) -> Instruction {
        let caps = self
            .instruction_re
            .captures(line)
            .unwrap_or_else(|| panic!("failed to parse line {}", line));
        if &caps[1] == "mask" {
            Instruction::MaskSet(self.parse_mask(&caps[2]))
        } else {
            Instruction::MemSet(self.parse_memset(&caps[1], &caps[2]))
        }
    }

    fn parse_memset(&self, mem: &str, val: &str) -> MemSet {
        let address: usize = self.mem_re.captures(mem).unwrap()[1].parse().unwrap();
        let value: u64 = val.parse().unwrap();
        MemSet { address, value }
    }

    fn parse_mask(&self, input: &str) -> Mask {
        let mut ones = 0;
        let mut zeroes = 0;

        for c in input.chars() {
            match c {
                '1' => {
                    ones = (ones << 1) | 1;
                    zeroes = (zeroes << 1) | 1;
                }

                '0' => {
                    ones <<= 1;
                    zeroes <<= 1;
                }

                'X' => {
                    ones <<= 1;
                    zeroes = (zeroes << 1) | 1;
                }

                _ => panic!("unexpected mask character {}", c),
            };
        }

        Mask { ones, zeroes }
    }
}

enum Instruction {
    MaskSet(Mask),
    MemSet(MemSet),
}

#[derive(Clone, Copy, Debug)]
struct Mask {
    ones: u64,
    zeroes: u64,
}

impl Mask {
    fn apply(&self, val: u64) -> u64 {
        (val | self.ones) & self.zeroes
    }
}

struct MemSet {
    address: usize,
    value: u64,
}

fn load_input() -> Vec<Instruction> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("error reading input file");
    let parser = InstructionParser::new();

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parser.parse_line(line))
        .collect()
}

struct State {
    mask: Mask,
    memory: HashMap<usize, u64>,
}

impl State {
    fn new() -> Self {
        Self {
            mask: Mask { ones: 0, zeroes: 0 },
            memory: HashMap::new(),
        }
    }

    fn process_instruction(&mut self, instr: &Instruction) {
        match instr {
            &Instruction::MaskSet(mask) => {
                self.mask = mask;
            }
            Instruction::MemSet(mem_set) => {
                self.apply_memset(mem_set);
            }
        }
    }

    fn apply_memset(&mut self, mem_set: &MemSet) {
        self.memory
            .insert(mem_set.address, self.mask.apply(mem_set.value));
    }

    fn sum_memory_vals(&self) -> u64 {
        self.memory.values().sum()
    }
}

fn find_final_mem_sum(instructions: &[Instruction]) -> u64 {
    let mut state = State::new();

    for instr in instructions {
        state.process_instruction(&instr);
    }

    state.sum_memory_vals()
}
