use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let result = find_earliest_bus(&input);
    println!("result is {}", result);
}

struct BusTimetable {
    earliest_timestamp: u64,
    bus_ids: Vec<u64>,
}

impl BusTimetable {
    fn parse(input: &str) -> Self {
        let lines: Vec<&str> = input.split('\n').collect();
        let earliest_timestamp: u64 = lines[0].parse().unwrap();
        let bus_ids = lines[1]
            .split(',')
            .filter(|&id| id != "x")
            .map(|id| id.parse().unwrap())
            .collect();

        Self {
            earliest_timestamp,
            bus_ids,
        }
    }
}

fn load_input() -> BusTimetable {
    let args: Vec<String> = env::args().collect();
    let contents = fs::read_to_string(&args[1]).expect("error reading input file");
    BusTimetable::parse(&contents)
}

fn find_earliest_bus(input: &BusTimetable) -> u64 {
    let (min_wait, id) = input
        .bus_ids
        .iter()
        .fold(
            None,
            |current_min: Option<(u64, u64)>, id: &u64| -> Option<(u64, u64)> {
                let wait = id - (input.earliest_timestamp % id);
                if current_min.is_none() || wait < current_min.unwrap().0 {
                    Some((wait, *id))
                } else {
                    current_min
                }
            },
        )
        .unwrap();

    min_wait * id
}
