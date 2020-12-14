use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let mem_sum_1 = find_final_mem_sum(&input, StateVersion::V1);
    println!("final sum of memory values for V1 is {}", mem_sum_1);

    let mem_sum_2 = find_final_mem_sum(&input, StateVersion::V2);
    println!("final sum of memory values for V2 is {}", mem_sum_2);
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
        let address: u64 = self.mem_re.captures(mem).unwrap()[1].parse().unwrap();
        let value: u64 = val.parse().unwrap();
        MemSet { address, value }
    }

    fn parse_mask(&self, input: &str) -> Mask {
        let mut ones = 0;
        let mut zeroes = 0;
        let mut wildcards = Vec::new();

        for (i, c) in input.chars().enumerate() {
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
                    wildcards.push(input.len() - 1 - i);
                }

                _ => panic!("unexpected mask character {}", c),
            };
        }

        Mask {
            ones,
            zeroes,
            wildcards,
        }
    }
}

enum Instruction {
    MaskSet(Mask),
    MemSet(MemSet),
}

#[derive(Clone, Debug)]
struct Mask {
    ones: u64,
    zeroes: u64,
    wildcards: Vec<usize>,
}

impl Mask {
    fn mask_value(&self, val: u64) -> u64 {
        (val | self.ones) & self.zeroes
    }

    fn mask_address(&self, addr: u64) -> Vec<u64> {
        Self::mask_address_recur(addr | self.ones, &self.wildcards)
    }

    fn mask_address_recur(addr: u64, wildcards: &[usize]) -> Vec<u64> {
        if wildcards.is_empty() {
            return vec![addr];
        }

        let wildcard_pos = wildcards[0];

        let mut addrs = Self::mask_address_recur(addr | (1 << wildcard_pos), &wildcards[1..]);
        let mut other_addrs =
            Self::mask_address_recur(addr & (!(1 << wildcard_pos)), &wildcards[1..]);
        addrs.append(&mut other_addrs);

        addrs
    }
}

struct MemSet {
    address: u64,
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

enum StateVersion {
    V1,
    V2,
}

struct State {
    mask: Mask,
    memory: HashMap<u64, u64>, // Use a HashMap to avoid allocating an 8 GB vector...
    version: StateVersion,
}

impl State {
    fn new(version: StateVersion) -> Self {
        Self {
            mask: Mask {
                ones: 0,
                zeroes: 0,
                wildcards: Vec::new(),
            },
            memory: HashMap::new(),
            version,
        }
    }

    fn process_instruction(&mut self, instr: &Instruction) {
        match instr {
            Instruction::MaskSet(mask) => {
                self.mask = mask.clone();
            }
            Instruction::MemSet(mem_set) => {
                self.apply_memset(mem_set);
            }
        }
    }

    fn apply_memset(&mut self, mem_set: &MemSet) {
        match self.version {
            StateVersion::V1 => {
                self.memory
                    .insert(mem_set.address, self.mask.mask_value(mem_set.value));
            }
            StateVersion::V2 => {
                for addr in self.mask.mask_address(mem_set.address) {
                    self.memory.insert(addr, mem_set.value);
                }
            }
        };
    }

    fn sum_memory_vals(&self) -> u64 {
        self.memory.values().sum()
    }
}

fn find_final_mem_sum(instructions: &[Instruction], version: StateVersion) -> u64 {
    let mut state = State::new(version);

    for instr in instructions {
        state.process_instruction(&instr);
    }

    state.sum_memory_vals()
}
