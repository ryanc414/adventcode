use std::env;
use std::fs;

type NextStateFn = fn(&[Vec<Position>], (usize, usize)) -> Position;

fn main() {
    let input = load_input();

    let occupied_count_1 = count_stable_occupied(&input, next_state_for_part_1);
    println!(
        "there are {} occupied seats in the stable arrangement for part one",
        occupied_count_1
    );

    let occupied_count_2 = count_stable_occupied(&input, next_state_for_part_2);
    println!(
        "there are {} occupied seats in the stable arrangement for part two",
        occupied_count_2
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

fn count_stable_occupied(input: &[Vec<Position>], next_state_fn: NextStateFn) -> usize {
    let mut states = (input.to_vec(), input.to_vec());
    let mut flip_states = false;
    let mut iter_count = 0;

    while iter_count == 0 || states.0 != states.1 {
        if !flip_states {
            calculate_next_state(&states.0, &mut states.1, next_state_fn);
        } else {
            calculate_next_state(&states.1, &mut states.0, next_state_fn);
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

fn calculate_next_state(
    state: &[Vec<Position>],
    next_state: &mut [Vec<Position>],
    next_state_fn: NextStateFn,
) {
    for row in 0..state.len() {
        for col in 0..state[row].len() {
            next_state[row][col] = next_state_fn(state, (row, col));
        }
    }
}

fn next_state_for_part_1(state: &[Vec<Position>], (row, col): (usize, usize)) -> Position {
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

fn count_occupied_neighbours(state: &[Vec<Position>], row_ix: usize, col_ix: usize) -> usize {
    all_directions()
        .iter()
        .filter(|dir| {
            let neighbour_row = (row_ix as i64) + dir.0;
            let neighbour_col = (col_ix as i64) + dir.1;

            if !valid_position(state, (neighbour_row, neighbour_col)) {
                return false;
            }

            state[neighbour_row as usize][neighbour_col as usize] == Position::OccupiedSeat
        })
        .count()
}

fn valid_position(state: &[Vec<Position>], (row, col): (i64, i64)) -> bool {
    row >= 0 && (row as usize) < state.len() && col >= 0 && (col as usize) < state[0].len()
}

fn next_state_for_part_2(state: &[Vec<Position>], (row, col): (usize, usize)) -> Position {
    match state[row][col] {
        Position::EmptySeat => {
            let visible_occupied = count_visible_occupied(state, (row, col));
            if visible_occupied == 0 {
                Position::OccupiedSeat
            } else {
                Position::EmptySeat
            }
        }

        Position::OccupiedSeat => {
            let visible_occupied = count_visible_occupied(state, (row, col));
            if visible_occupied >= 5 {
                Position::EmptySeat
            } else {
                Position::OccupiedSeat
            }
        }

        Position::Floor => Position::Floor,
    }
}

fn count_visible_occupied(state: &[Vec<Position>], (row, col): (usize, usize)) -> usize {
    all_directions()
        .iter()
        .filter(|&&dir| occupied_is_visible(state, (row, col), dir))
        .count()
}

fn all_directions() -> [(i64, i64); 8] {
    let mut directions = [(0, 0); 8];
    let mut i = 0;

    for &x in &[-1, 0, 1] {
        for &y in &[-1, 0, 1] {
            if x == 0 && y == 0 {
                continue;
            }

            directions[i] = (x, y);
            i += 1;
        }
    }

    directions
}

fn occupied_is_visible(
    state: &[Vec<Position>],
    (row, col): (usize, usize),
    direction: (i64, i64),
) -> bool {
    let mut y = (row as i64) + direction.0;
    let mut x = (col as i64) + direction.1;

    while valid_position(state, (y, x)) {
        match state[y as usize][x as usize] {
            Position::OccupiedSeat => return true,
            Position::EmptySeat => return false,
            Position::Floor => {
                y += direction.0;
                x += direction.1;
            }
        }
    }

    false
}

fn count_total_occupied(state: &[Vec<Position>]) -> usize {
    state
        .iter()
        .map(|row| {
            row.iter()
                .filter(|&&pos| pos == Position::OccupiedSeat)
                .count()
        })
        .sum()
}
