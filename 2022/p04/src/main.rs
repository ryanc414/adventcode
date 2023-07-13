use std::env;
use std::fs;
use std::str::FromStr;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let assignments = parse_assignments(&input).unwrap();

    let count = count_contained_pairs(&assignments);
    println!("Part 1: {}", count);

    let count = count_overlapping_pairs(&assignments);
    println!("Part 2: {}", count);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input file>", args[0]);
    }

    args.remove(1)
}

fn load_input(filename: &str) -> Vec<String> {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");

    contents.lines().map(|s| s.to_string()).collect()
}

#[derive(Debug, Copy, Clone)]
struct Assignment {
    start: u32,
    end: u32,
}

impl Assignment {
    fn contains(&self, other: Assignment) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps(&self, other: Assignment) -> bool {
        self.start <= other.end && self.end >= other.start
    }
}

impl FromStr for Assignment {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split("-").collect();
        if parts.len() != 2 {
            return Err("wrong number of parts");
        }

        let start = parts[0].parse().or(Err("failed to parse part 1"))?;
        let end = parts[1].parse().or(Err("failed to parse part 2"))?;

        Ok(Assignment { start, end })
    }
}


fn parse_assignments(input: &[String]) -> Result<Vec<(Assignment, Assignment)>, &str> {
    input.iter().map(|s| parse_assignments_line(s)).collect()
}

fn parse_assignments_line(line: &str) -> Result<(Assignment, Assignment), &str> {
    let results: Result<Vec<Assignment>, &str> = line.split(",").map(|s| s.parse()).collect();
    let results = results?;

    if results.len() != 2 {
        return Err("wrong number of assignments");
    }

    Ok((results[0], results[1]))
}

fn count_contained_pairs(assignments: &[(Assignment, Assignment)]) -> u32 {
    assignments.iter().map(|pair| is_contained(pair)).filter(|&b| b).count() as u32
}

fn is_contained(&(a, b): &(Assignment, Assignment)) -> bool {
    a.contains(b) || b.contains(a)
}

fn count_overlapping_pairs(assignments: &[(Assignment, Assignment)]) -> u32 {
    assignments.iter().map(|pair| is_overlapping(pair)).filter(|&b| b).count() as u32
}

fn is_overlapping(&(a, b): &(Assignment, Assignment)) -> bool {
    a.overlaps(b)
}
