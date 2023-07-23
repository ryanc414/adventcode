use std::env;
use std::fs;
use std::str::FromStr;
use regex::Regex;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename).unwrap();

    let message = find_message(input.clone());
    println!("{}", message);

    let message = find_message_2(input);
    println!("{}", message);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

#[derive(Clone)]
struct PuzzleInput {
    stacks: Vec<Vec<char>>,
    instructions: Vec<Instruction>,
}

#[derive(Clone)]
struct Instruction {
    count: u32,
    from: usize,
    to: usize,
}

impl Instruction {
    fn execute(&self, stacks: &mut Vec<Vec<char>>) {
        for _ in 0..self.count {
            let c = stacks[self.from - 1].pop().unwrap();
            stacks[self.to - 1].push(c);
        }
    }

    fn execute_2(&self, stacks: &mut Vec<Vec<char>>) {
        let mut tmp = Vec::new();

        for _ in 0..self.count {
            let c = stacks[self.from - 1].pop().unwrap();
            tmp.push(c);
        }

        for c in tmp.into_iter().rev() {
            stacks[self.to - 1].push(c);
        }
    }
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let instructions_re: Regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();

        let caps = instructions_re.captures(s).ok_or("Invalid instruction")?;
        let count = caps[1].parse().map_err(|_| "Invalid count")?;
        let from = caps[2].parse().map_err(|_| "invalid from")?;
        let to = caps[3].parse().map_err(|_| "invalid to")?;

        Ok(Instruction { count, from, to })
    }
}

fn load_input(filename: &str) -> Result<PuzzleInput, String> {
    let contents = fs::read_to_string(filename).map_err(|e| e.to_string())?;

    let parts: Vec<&str> = contents.split("\n\n").collect();
    if parts.len() != 2 {
        return Err("Invalid input".to_string());
    }

    let stacks = parse_stacks(parts[0])?;
    let instructions = parse_instructions(parts[1])?;

    Ok(PuzzleInput { stacks, instructions })
}

fn parse_stacks(input: &str) -> Result<Vec<Vec<char>>, String> {
    let mut lines: Vec<String> = input
        .lines()
        .map(|line| line.chars().enumerate().filter(|(i, _)| i % 4 == 1).map(|(_, c)| c).collect())
        .collect();
    lines.pop();

    let num_stacks = lines.iter().map(|line| line.len()).max().ok_or("no stacks")?;

    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _ in 0..num_stacks {
        stacks.push(Vec::new());
    }

    lines.reverse();

    for line in lines {
        for (i, c) in line.chars().enumerate() {
            if c != ' ' {
                stacks[i].push(c);
            }
        }
    }

    Ok(stacks)
}

fn parse_instructions(input: &str) -> Result<Vec<Instruction>, String> {
    input.lines().map(|line| line.parse()).collect()
}


fn find_message(mut input: PuzzleInput) -> String {
    for instruction in input.instructions.iter() {
        instruction.execute(&mut input.stacks);
    }

    input.stacks.into_iter().map(|mut stack: Vec<char>| stack.pop().unwrap()).collect()
}

fn find_message_2(mut input: PuzzleInput) -> String {
    for instruction in input.instructions.iter() {
        instruction.execute_2(&mut input.stacks);
    }

    input.stacks.into_iter().map(|mut stack: Vec<char>| stack.pop().unwrap()).collect()
}
