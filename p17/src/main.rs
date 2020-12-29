use itertools::Itertools;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::ops::Range;

fn main() {
    let filename = parse_args();
    let input = fs::read_to_string(filename).expect("error reading input file");

    let active_count = find_active_count(&input, 6, 3);
    println!("after 6 cycles in 3D, {} cubes are active", active_count);

    let active_count = find_active_count(&input, 6, 4);
    println!("after 6 cycles in 4D, {} cubes are active", active_count);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    args.remove(1)
}

#[derive(Clone, Debug)]
struct Grid {
    active_cubes: HashSet<Vec<i64>>,
    num_dimensions: usize,
    ranges: Vec<Range<i64>>,
}

impl Grid {
    fn parse(input: &str, num_dimensions: usize) -> Self {
        let lines: Vec<&str> = input.split('\n').filter(|line| !line.is_empty()).collect();

        let active_cubes: HashSet<Vec<i64>> = lines
            .iter()
            .enumerate()
            .map(|(i, line): (usize, &&str)| -> Vec<Vec<i64>> {
                line.chars()
                    .enumerate()
                    .filter(|&(_, c)| c == '#')
                    .map(|(j, _): (usize, char)| -> Vec<i64> {
                        let y = lines.len() - i - 1;
                        let mut coords = vec![0; num_dimensions];

                        coords[0] = j as i64;
                        coords[1] = y as i64;

                        coords
                    })
                    .collect()
            })
            .flatten()
            .collect();

        Self::new(active_cubes, num_dimensions)
    }

    fn new(active_cubes: HashSet<Vec<i64>>, num_dimensions: usize) -> Self {
        let ranges = Self::extract_ranges(&active_cubes, num_dimensions);

        Self {
            active_cubes,
            num_dimensions,
            ranges,
        }
    }

    fn extract_ranges(active_cubes: &HashSet<Vec<i64>>, num_dimensions: usize) -> Vec<Range<i64>> {
        (0..num_dimensions)
            .map(|i| {
                let min = active_cubes.iter().map(|coords| coords[i]).min().unwrap();
                let max = active_cubes.iter().map(|coords| coords[i]).max().unwrap();
                (min - 1)..(max + 2)
            })
            .collect()
    }

    fn num_active(&self) -> usize {
        self.active_cubes.len()
    }

    fn next_cycle(&self) -> Self {
        // If a cube is active and exactly 2 or 3 of its neighbors are also
        // active, the cube remains active. Otherwise, the cube becomes inactive.
        //
        // If a cube is inactive but exactly 3 of its neighbors are active, the
        // cube becomes active. Otherwise, the cube remains inactive.
        let mut next_cubes: HashSet<Vec<i64>> = HashSet::new();

        for coords in self.ranges.iter().cloned().multi_cartesian_product() {
            if self.activate_next(&coords) {
                next_cubes.insert(coords);
            }
        }

        Self::new(next_cubes, self.num_dimensions)
    }

    fn activate_next(&self, coords: &[i64]) -> bool {
        let active_neighbours = self.active_neighbour_count(coords);

        if self.active_cubes.contains(coords) {
            active_neighbours == 2 || active_neighbours == 3
        } else {
            active_neighbours == 3
        }
    }

    fn active_neighbour_count(&self, coords: &[i64]) -> u32 {
        let mut count = 0;

        for offset in (0..coords.len()).map(|_| 0..3).multi_cartesian_product() {
            if offset.iter().all(|&i| i == 1) {
                continue;
            }
            let neighbour_coords: Vec<i64> =
                coords.iter().zip(offset).map(|(x, i)| x + i - 1).collect();
            if self.active_cubes.contains(&neighbour_coords) {
                count += 1;
            }
        }

        count
    }
}

fn find_active_count(input: &str, num_cycles: usize, num_dimensions: usize) -> usize {
    let mut state = Grid::parse(input, num_dimensions);

    for _ in 0..num_cycles {
        state = state.next_cycle();
    }

    state.num_active()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = fs::read_to_string("basic_input.txt").unwrap();

        let active_count = find_active_count(&input, 6, 3);
        assert_eq!(active_count, 112);

        let active_count = find_active_count(&input, 6, 4);
        assert_eq!(active_count, 848);
    }

    #[test]
    fn test_full_input() {
        let input = fs::read_to_string("full_input.txt").unwrap();

        let active_count = find_active_count(&input, 6, 3);
        assert_eq!(active_count, 291);

        let active_count = find_active_count(&input, 6, 4);
        assert_eq!(active_count, 1524);
    }
}
