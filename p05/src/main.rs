use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let highest_id = find_highest_seat_id(&input);
    println!("highest seat ID is {}", highest_id);

    let my_seat = find_my_seat(&input).unwrap();
    println!("my seat ID is {}", my_seat.id());
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct Seat {
    row: u8,
    col: u8,
}

impl Seat {
    fn id(&self) -> u32 {
        (self.row as u32) * 8 + (self.col as u32)
    }
}

fn load_input() -> Vec<Seat> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parse_seat(line).unwrap())
        .collect()
}

fn parse_seat(input: &str) -> Result<Seat, String> {
    if input.len() != 10 {
        return Err(String::from("unexpected input length"));
    }

    let input_chars: Vec<char> = input.chars().collect();
    let row = binary_to_u8(&input_chars[0..7], 'F', 'B')?;
    let col = binary_to_u8(&input_chars[7..10], 'L', 'R')?;

    Ok(Seat { row, col })
}

fn binary_to_u8(bin: &[char], zero: char, one: char) -> Result<u8, String> {
    let mut val: u8 = 0;

    for &c in bin.iter() {
        match c {
            _ if c == zero => val <<= 1,
            _ if c == one => val = (val << 1) ^ 1,
            _ => return Err(format!("unexpected letter {}", c)),
        }
    }

    Ok(val)
}

fn find_highest_seat_id(input: &[Seat]) -> u32 {
    input.iter().map(|seat| seat.id()).max().unwrap()
}

fn find_my_seat(input: &[Seat]) -> Result<Seat, String> {
    let lowest_row = input.iter().map(|seat| seat.row).min().unwrap();
    let highest_row = input.iter().map(|seat| seat.row).max().unwrap();

    let possible_seats = generate_possible_seats(lowest_row, highest_row);
    let input_seats: HashSet<Seat> = input.iter().cloned().collect();

    let unaccounted_seats: Vec<&Seat> = possible_seats.difference(&input_seats).collect();
    if unaccounted_seats.len() != 1 {
        return Err(format!(
            "unexpected number of unaccounted seats: {:?}",
            unaccounted_seats
        ));
    }

    Ok(unaccounted_seats[0].clone())
}

fn generate_possible_seats(lowest_row: u8, highest_row: u8) -> HashSet<Seat> {
    let mut seats: HashSet<Seat> = HashSet::new();

    // Don't include first and last rows as we know that the missing seat isn't
    // in either of them.
    for row in (lowest_row + 1)..highest_row {
        for col in 0..8 {
            seats.insert(Seat { row, col });
        }
    }

    seats
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_seat() {
        let seat = parse_seat("FBFBBFFRLR").unwrap();
        assert_eq!(seat.row, 44);
        assert_eq!(seat.col, 5);
        assert_eq!(seat.id(), 357);
    }
}
