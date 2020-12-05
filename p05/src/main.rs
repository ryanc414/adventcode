use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let highest_id = find_highest_seat_id(&input);
    println!("highest seat ID is {}", highest_id);
}

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

    let mut it = input.chars();
    let mut row: u8 = 0;

    // Iterate over the first 7 chars. Note that the .take() method consumes
    // the iterator, so we have to do it this way to be able to re-use the
    // same iterator later!
    for _ in 0..7 {
        let c = it.next().unwrap();
        match c {
            'F' => row <<= 1,
            'B' => row = (row << 1) ^ 1,
            _ => return Err(String::from("unexpected letter in first 6 chars")),
        };
    }

    let mut col: u8 = 0;

    // Now we can consume the final 3 chars.
    for c in it {
        match c {
            'L' => col <<= 1,
            'R' => col = (col << 1) ^ 1,
            _ => {
                return Err(format!(
                    "unexpected letter {} in last 3 chars of {}",
                    c, input
                ))
            }
        };
    }

    Ok(Seat { row, col })
}

fn find_highest_seat_id(input: &[Seat]) -> u32 {
    input.iter().map(|seat| seat.id()).max().unwrap()
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
