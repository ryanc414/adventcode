use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let active_count = find_active_count(&input, 6);
    println!("after 6 cycles, {} cubes are active", active_count);
}

#[derive(Clone, Debug)]
struct Grid {
    active_cubes: HashSet<(i64, i64, i64)>,
    ranges: Ranges,
}

#[derive(Clone, Debug)]
struct Ranges {
    x: (i64, i64),
    y: (i64, i64),
    z: (i64, i64),
}

impl Grid {
    fn parse(input: &str) -> Self {
        let lines: Vec<&str> = input.split('\n').filter(|line| !line.is_empty()).collect();

        let active_cubes: HashSet<(i64, i64, i64)> = lines
            .iter()
            .enumerate()
            .map(|(i, line): (usize, &&str)| -> Vec<(i64, i64, i64)> {
                line.chars()
                    .enumerate()
                    .filter(|&(_, c)| c == '#')
                    .map(|(j, _): (usize, char)| -> (i64, i64, i64) {
                        let y = lines.len() - i - 1;
                        (j as i64, y as i64, 0)
                    })
                    .collect()
            })
            .flatten()
            .collect();

        Self::new(active_cubes)
    }

    fn new(active_cubes: HashSet<(i64, i64, i64)>) -> Self {
        let ranges = Self::extract_ranges(&active_cubes);

        Self {
            active_cubes,
            ranges,
        }
    }

    fn extract_ranges(active_cubes: &HashSet<(i64, i64, i64)>) -> Ranges {
        let x_max = active_cubes.iter().map(|&(x, _, _)| x).max().unwrap();
        let x_min = active_cubes.iter().map(|&(x, _, _)| x).min().unwrap();

        let y_max = active_cubes.iter().map(|&(_, y, _)| y).max().unwrap();
        let y_min = active_cubes.iter().map(|&(_, y, _)| y).min().unwrap();

        let z_max = active_cubes.iter().map(|&(_, _, z)| z).max().unwrap();
        let z_min = active_cubes.iter().map(|&(_, _, z)| z).min().unwrap();

        Ranges {
            x: (x_min, x_max),
            y: (y_min, y_max),
            z: (z_min, z_max),
        }
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

        let mut next_cubes: HashSet<(i64, i64, i64)> = HashSet::new();

        for x in (self.ranges.x.0 - 1)..(self.ranges.x.1 + 2) {
            for y in (self.ranges.y.0 - 1)..(self.ranges.y.1 + 2) {
                for z in (self.ranges.z.0 - 1)..(self.ranges.z.1 + 2) {
                    if self.activate_next((x, y, z)) {
                        next_cubes.insert((x, y, z));
                    }
                }
            }
        }

        Self::new(next_cubes)
    }

    fn activate_next(&self, coords: (i64, i64, i64)) -> bool {
        let active_neighbours = self.active_neighbour_count(coords);

        if self.active_cubes.contains(&coords) {
            active_neighbours == 2 || active_neighbours == 3
        } else {
            active_neighbours == 3
        }
    }

    fn active_neighbour_count(&self, (x, y, z): (i64, i64, i64)) -> u32 {
        let mut count = 0;

        for i in 0..3 {
            for j in 0..3 {
                for k in 0..3 {
                    if i == 1 && j == 1 && k == 1 {
                        continue;
                    }
                    let neighbour_coords = (x + i - 1, y + j - 1, z + k - 1);
                    if self.active_cubes.contains(&neighbour_coords) {
                        count += 1;
                    }
                }
            }
        }

        count
    }
}

fn load_input() -> Grid {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    let contents = fs::read_to_string(&args[1]).expect("error reading input file");
    Grid::parse(&contents)
}

fn find_active_count(input: &Grid, num_cycles: usize) -> usize {
    let mut state = input.clone();

    for _ in 0..num_cycles {
        state = state.next_cycle();
    }

    state.num_active()
}
