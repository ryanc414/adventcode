use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let result = find_earliest_bus(&input);
    println!("result is {}", result);

    let timestamp = find_earliest_timestamp(&input);
    println!("earliest timestamp that matches rules is {}", timestamp);
}

struct BusTimetable {
    earliest_timestamp: u64,
    buses: Vec<BusInfo>,
}

#[derive(Clone, Copy, Debug)]
struct BusInfo {
    id: u64,
    list_position: u64,
}

impl BusTimetable {
    fn parse(input: &str) -> Self {
        let lines: Vec<&str> = input.split('\n').collect();
        let earliest_timestamp: u64 = lines[0].parse().unwrap();
        let buses = lines[1]
            .split(',')
            .enumerate()
            .filter(|&(_, id)| id != "x")
            .map(|(i, id)| BusInfo {
                id: id.parse().unwrap(),
                list_position: i as u64,
            })
            .collect();

        Self {
            earliest_timestamp,
            buses,
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
        .buses
        .iter()
        .fold(
            None,
            |current_min: Option<(u64, u64)>, bus: &BusInfo| -> Option<(u64, u64)> {
                let wait = bus.id - (input.earliest_timestamp % bus.id);
                if current_min.is_none() || wait < current_min.unwrap().0 {
                    Some((wait, bus.id))
                } else {
                    current_min
                }
            },
        )
        .unwrap();

    min_wait * id
}

fn find_earliest_timestamp(input: &BusTimetable) -> u64 {
    let mut bus_info = input.buses.to_vec();
    bus_info.sort_unstable_by(|bus_a, bus_b| bus_b.id.cmp(&bus_a.id));

    find_earliest_timestamp_from(
        &bus_info[1..],
        bus_info[0].id - bus_info[0].list_position,
        bus_info[0].id,
    )
}

fn find_earliest_timestamp_from(buses: &[BusInfo], start_timestamp: u64, increment: u64) -> u64 {
    if buses.is_empty() {
        return start_timestamp;
    }

    let head_bus = buses[0];
    let tail_buses = &buses[1..];
    let mut t = start_timestamp;

    loop {
        if (t + head_bus.list_position) % head_bus.id == 0 {
            let next_increment = lowest_common_multiple(increment, head_bus.id);
            return find_earliest_timestamp_from(tail_buses, t, next_increment);
        }

        t += increment;
    }
}

fn lowest_common_multiple(x: u64, y: u64) -> u64 {
    let mut m = x;

    loop {
        if m % y == 0 {
            return m;
        }
        m += x;
    }
}
