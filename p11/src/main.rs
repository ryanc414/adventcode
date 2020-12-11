use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let occupied_count = count_stable_occupied(&input);
    println!(
        "there are {} occupied seats in the stable arrangement",
        occupied_count
    );
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Position {
    Floor,
    EmptySeat,
    OccupiedSeat,
}

fn load_input() -> Vec<Vec<Position>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("error reading file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parse_row(line))
        .collect()
}

fn parse_row(line: &str) -> Vec<Position> {
    line.chars()
        .map(|pos| match pos {
            '.' => Position::Floor,
            'L' => Position::EmptySeat,
            '#' => Position::OccupiedSeat,
            _ => panic!("unexpected input char {}", pos),
        })
        .collect()
}

fn count_stable_occupied(input: &[Vec<Position>]) -> usize {
    let mut states = (input.to_vec(), input.to_vec());
    let mut flip_states = false;
    let mut iter_count = 0;

    while iter_count == 0 || states.0 != states.1 {
        if !flip_states {
            calculate_next_state(&states.0, &mut states.1);
        } else {
            calculate_next_state(&states.1, &mut states.0);
        }
        iter_count += 1;
        flip_states = !flip_states;
    }

    println!(
        "reached stable configuration after {} iterations",
        iter_count
    );

    if !flip_states {
        count_total_occupied(&states.0)
    } else {
        count_total_occupied(&states.1)
    }
}

fn calculate_next_state(state: &[Vec<Position>], next_state: &mut [Vec<Position>]) {
    for row in 0..state.len() {
        for col in 0..state[row].len() {
            next_state[row][col] = next_state_for(state, row, col);
        }
    }
}

fn next_state_for(state: &[Vec<Position>], row: usize, col: usize) -> Position {
    match state[row][col] {
        Position::EmptySeat => {
            let occupied_neighbours = count_occupied_neighbours(state, row, col);
            if occupied_neighbours == 0 {
                Position::OccupiedSeat
            } else {
                Position::EmptySeat
            }
        }

        Position::OccupiedSeat => {
            let occupied_neighbours = count_occupied_neighbours(state, row, col);
            if occupied_neighbours >= 4 {
                Position::EmptySeat
            } else {
                Position::OccupiedSeat
            }
        }

        Position::Floor => Position::Floor,
    }
}

fn count_occupied_neighbours(state: &[Vec<Position>], row: usize, col: usize) -> usize {
    let mut count = 0;
    let min_row = if row > 0 { row - 1 } else { 0 };
    let max_row = if row < (state.len() - 1) {
        row + 1
    } else {
        row
    };
    let min_col = if col > 0 { col - 1 } else { 0 };
    let max_col = if col < (state[0].len() - 1) {
        col + 1
    } else {
        col
    };

    for i in min_row..(max_row + 1) {
        for j in min_col..(max_col + 1) {
            if i == row && j == col {
                continue;
            }
            if state[i][j] == Position::OccupiedSeat {
                count += 1;
            }
        }
    }

    count
}

fn count_total_occupied(state: &[Vec<Position>]) -> usize {
    state.iter().fold(0, |count, row| {
        count
            + row.iter().fold(0, |inner_count, &pos| {
                if pos == Position::OccupiedSeat {
                    inner_count + 1
                } else {
                    inner_count
                }
            })
    })
}
