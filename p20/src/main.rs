use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::hash::Hash;

fn main() {
    let input = load_input();
    let arranged = arrange_tiles(&input);

    let corner_product = multiply_corner_ids(&arranged);
    println!("product of corners IDs is {}", corner_product);

    let roughness = find_water_roughness(arranged);
    println!("water roughness: {}", roughness);
}

fn multiply_corner_ids(tiles: &[Vec<Tile>]) -> u64 {
    let first_row = &tiles[0];
    let last_row = &tiles[tiles.len() - 1];

    first_row[0].id
        * first_row[first_row.len() - 1].id
        * last_row[0].id
        * last_row[last_row.len() - 1].id
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Pixel {
    One,
    Zero,
}

impl Pixel {
    fn parse(c: char) -> Self {
        match c {
            '#' => Self::One,
            '.' => Self::Zero,
            _ => panic!("cannot parse {} as a pixel", c),
        }
    }

    fn to_char(&self) -> char {
        match self {
            Self::One => '#',
            Self::Zero => '.',
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Edge(Vec<Pixel>);

impl Edge {
    fn new(mut pixels: Vec<Pixel>) -> Self {
        let middle = pixels.len() / 2;
        let first_half = pixels[..middle].to_vec();
        let second_half: Vec<Pixel> = pixels[middle..].iter().rev().cloned().collect();
        if first_half.len() != second_half.len() {
            panic!("unequal halves");
        }

        if first_half > second_half {
            pixels.reverse();
        }

        Self(pixels)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Tile {
    id: u64,
    contents: Vec<Vec<Pixel>>,
    edges: [Edge; 4],
    size: usize,
}

impl Tile {
    fn from_contents(contents: Vec<Vec<Pixel>>, id: u64) -> Self {
        let top = Self::get_edge(&contents, 0);
        let right = Self::get_edge(&contents, 1);
        let bottom = Self::get_edge(&contents, 2);
        let left = Self::get_edge(&contents, 3);

        let size = contents.len();

        Self {
            id,
            contents,
            edges: [
                Edge::new(top),
                Edge::new(right),
                Edge::new(bottom),
                Edge::new(left),
            ],
            size,
        }
    }

    fn get_edge(contents: &[Vec<Pixel>], edge: usize) -> Vec<Pixel> {
        match edge {
            0 => contents[0].to_vec(),                                       // top
            1 => contents.iter().map(|line| line[line.len() - 1]).collect(), // right
            2 => contents[contents.len() - 1].to_vec(),                      // bottom
            3 => contents.iter().map(|line| line[0]).collect(),              // left
            _ => panic!("unexpected edge index: {}", edge),
        }
    }

    fn rotate_starting_corner(&self, edge_shares: &HashMap<&Edge, Vec<(&Tile, usize)>>) -> Self {
        let unmatched_edges: Vec<usize> = self
            .edges
            .iter()
            .enumerate()
            .filter(|(_, edge)| edge_shares.get(edge).unwrap().len() == 1)
            .map(|(i, _)| i)
            .collect();
        assert_eq!(unmatched_edges.len(), 2);

        if unmatched_edges[0] == 0 && unmatched_edges[1] == 3 {
            return self.clone();
        }

        let rotations = (unmatched_edges[0] + 1) % 4;
        self.rotate_anticlockwise(rotations)
    }

    fn rotate_and_flip(&self, match_edge: usize, edge_ix: usize, matching_tile: &Self) -> Self {
        let required_edge = (match_edge + 2) % 4;
        let rotations = ((edge_ix + 4) - required_edge) % 4;
        let rotated = self.rotate_anticlockwise(rotations);
        rotated.flip_to_match(matching_tile, match_edge)
    }

    fn rotate_anticlockwise(&self, times: usize) -> Self {
        let rotated = rotate_anticlockwise(&self.contents, self.size, times);
        Self::from_contents(rotated, self.id)
    }

    fn flip_to_match(&self, other: &Self, other_edge_ix: usize) -> Self {
        let other_edge = Self::get_edge(&other.contents, other_edge_ix);
        let self_edge = Self::get_edge(&self.contents, (other_edge_ix + 2) % 4);

        if other_edge == self_edge {
            return self.clone();
        }

        let horizontal_flip = other_edge_ix % 2 == 0;

        let flipped = if horizontal_flip {
            flip_horizontal(&self.contents, self.size)
        } else {
            flip_vertical(&self.contents, self.size)
        };

        Self::from_contents(flipped, self.id)
    }

    fn strip_edges(&self) -> Self {
        let new_contents = self.contents[1..(self.size - 1)]
            .iter()
            .map(|row| row[1..(self.size - 1)].to_vec())
            .collect();

        Self::from_contents(new_contents, self.id)
    }
}

fn rotate_anticlockwise(pixels: &[Vec<Pixel>], size: usize, times: usize) -> Vec<Vec<Pixel>> {
    if times == 0 {
        return pixels.to_vec();
    }

    let mut new_contents: Vec<Vec<Pixel>> = Vec::new();

    for i in 0..size {
        let mut row: Vec<Pixel> = Vec::new();

        for j in 0..size {
            row.push(pixels[j][size - i - 1]);
        }

        new_contents.push(row);
    }

    rotate_anticlockwise(&new_contents, size, times - 1)
}

fn flip_vertical(pixels: &[Vec<Pixel>], size: usize) -> Vec<Vec<Pixel>> {
    let mut new_contents = Vec::new();

    for i in 0..size {
        new_contents.push(pixels[size - i - 1].clone());
    }

    new_contents
}

fn flip_horizontal(pixels: &[Vec<Pixel>], size: usize) -> Vec<Vec<Pixel>> {
    let mut new_contents = Vec::new();

    for i in 0..size {
        let mut row: Vec<Pixel> = Vec::new();
        for j in 0..size {
            row.push(pixels[i][size - j - 1]);
        }
        new_contents.push(row);
    }

    new_contents
}

struct TileParser {
    tile_re: Regex,
}

impl TileParser {
    fn new() -> Self {
        Self {
            tile_re: Regex::new(r"^Tile (\d+):$").unwrap(),
        }
    }

    fn parse(&self, input: &str) -> Tile {
        let mut it = input.split('\n');
        let caps = self.tile_re.captures(it.next().unwrap()).unwrap();
        let id: u64 = caps[1].parse().unwrap();
        let contents: Vec<Vec<Pixel>> = it
            .map(|line| line.chars().map(Pixel::parse).collect())
            .collect();

        Tile::from_contents(contents, id)
    }
}

fn load_input() -> Vec<Tile> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    let contents = fs::read_to_string(&args[1]).expect("could not read input file");
    let parser = TileParser::new();

    contents
        .split("\n\n")
        .filter(|section| !section.is_empty())
        .map(|section| parser.parse(section))
        .collect()
}

fn arrange_tiles(tiles: &[Tile]) -> Vec<Vec<Tile>> {
    let arrangement_size = get_arrangement_size(tiles.len());

    let edge_shares = find_edge_shares(tiles);
    let tile_match_counts = count_tile_unmatched_edges(&edge_shares);
    let corners = find_corners(&tile_match_counts);
    if corners.len() != 4 {
        panic!("expected 4 corners, got {}", corners.len())
    }

    let ordered_tiles = order_tiles(&edge_shares, corners, arrangement_size);
    collect_tiles(ordered_tiles, arrangement_size)
}

fn get_arrangement_size(num_tiles: usize) -> usize {
    ((num_tiles as f64).sqrt()) as usize
}

fn collect_tiles(
    tile_map: HashMap<(usize, usize), Tile>,
    arrangement_size: usize,
) -> Vec<Vec<Tile>> {
    let mut collected = Vec::new();

    for i in 0..arrangement_size {
        let mut row: Vec<Tile> = Vec::new();

        for j in 0..arrangement_size {
            let tile = tile_map.get(&(i, j)).unwrap();
            row.push(tile.clone());
        }

        collected.push(row);
    }

    collected
}

fn order_tiles(
    edge_shares: &HashMap<&Edge, Vec<(&Tile, usize)>>,
    corners: Vec<&Tile>,
    arrangement_size: usize,
) -> HashMap<(usize, usize), Tile> {
    // start withan aritrary corner.
    let start_tile = &corners[0];

    let mut tile_coords: HashMap<(usize, usize), Tile> = HashMap::new();
    let rotated_tile = start_tile.rotate_starting_corner(edge_shares);

    assert_eq!(edge_shares.get(&rotated_tile.edges[0]).unwrap().len(), 1);
    assert_eq!(edge_shares.get(&rotated_tile.edges[1]).unwrap().len(), 2);
    assert_eq!(edge_shares.get(&rotated_tile.edges[2]).unwrap().len(), 2);
    assert_eq!(edge_shares.get(&rotated_tile.edges[3]).unwrap().len(), 1);

    order_tiles_recur(
        &mut tile_coords,
        rotated_tile,
        (0, 0),
        edge_shares,
        arrangement_size,
    );
    tile_coords
}

fn order_tiles_recur(
    tile_coords: &mut HashMap<(usize, usize), Tile>,
    curr_tile: Tile,
    curr_coords: (usize, usize),
    edge_shares: &HashMap<&Edge, Vec<(&Tile, usize)>>,
    arrangement_size: usize,
) {
    tile_coords.insert(curr_coords, curr_tile.clone());

    for (i, edge) in curr_tile.edges.iter().enumerate() {
        let edge_tiles = edge_shares.get(edge).unwrap();

        let neighbour_coords = match calculate_neighbour_coords(curr_coords, i, arrangement_size) {
            Some(coords) => coords,
            None => {
                if edge_tiles.len() != 1 {
                    panic!(
                        "more than 1 tiles share edge: {} {} {:?}",
                        edge_tiles.len(),
                        i,
                        curr_coords
                    );
                }

                continue;
            }
        };

        if tile_coords.contains_key(&neighbour_coords) {
            continue;
        }

        if edge_tiles.len() != 2 {
            panic!(
                "only {} tiles share edge {} of tile {:?}",
                edge_tiles.len(),
                i,
                curr_coords
            );
        }

        let (next_tile, edge_ix) = edge_tiles
            .iter()
            .find(|&&(tile, _)| tile.id != curr_tile.id)
            .unwrap();

        let rotated_tile = next_tile.rotate_and_flip(i, *edge_ix, &curr_tile);

        order_tiles_recur(
            tile_coords,
            rotated_tile,
            neighbour_coords,
            edge_shares,
            arrangement_size,
        );
    }
}

fn in_range(coords: (usize, usize), size: usize) -> bool {
    coords.0 < size && coords.1 < size
}

const EDGE_DIRS: [(i64, i64); 4] = [
    (-1, 0), // top
    (0, 1),  // right
    (1, 0),  // down
    (0, -1), // left
];

fn calculate_neighbour_coords(
    curr_coords: (usize, usize),
    edge_ix: usize,
    size: usize,
) -> Option<(usize, usize)> {
    let dir = EDGE_DIRS[edge_ix];

    let coords = (
        ((curr_coords.0 as i64) + dir.0) as usize,
        ((curr_coords.1 as i64) + dir.1) as usize,
    );

    if in_range(coords, size) {
        Some(coords)
    } else {
        None
    }
}

fn find_edge_shares(tiles: &[Tile]) -> HashMap<&Edge, Vec<(&Tile, usize)>> {
    let mut edge_map: HashMap<&Edge, Vec<(&Tile, usize)>> = HashMap::new();

    for tile in tiles {
        for (i, edge) in tile.edges.iter().enumerate() {
            edge_map.entry(edge).or_default().push((tile, i));
        }
    }

    edge_map
}

fn count_tile_unmatched_edges<'a, 'b>(
    edge_shares: &'a HashMap<&'b Edge, Vec<(&'b Tile, usize)>>,
) -> HashMap<&'b Tile, usize> {
    let mut tiles_unmatched_edges: HashMap<&Tile, usize> = HashMap::new();

    for v in edge_shares.values() {
        if v.len() == 1 {
            let counter = tiles_unmatched_edges.entry(v[0].0).or_default();
            *counter += 1;
        }
    }

    tiles_unmatched_edges
}

fn find_corners<'a, 'b>(tiles_with_unmatched_edges: &'a HashMap<&'b Tile, usize>) -> Vec<&'b Tile> {
    tiles_with_unmatched_edges
        .iter()
        .filter(|&(_, &v)| v == 2)
        .map(|(&tile, _)| tile)
        .collect()
}

fn find_water_roughness(tiles: Vec<Vec<Tile>>) -> usize {
    let image = strip_edges_and_merge(tiles);
    let mut monsters_removed = image.clone();

    for rotation in 0..4 {
        for flip in 0..4 {
            let monster = rotate_flip_monster(rotation, flip);
            remove_monsters(&image, &mut monsters_removed, &monster);
        }
    }

    count_pixels(&monsters_removed)
}

// Monster looks like:
//
//                   #    18
// #    ##    ##    ###   0,5,6,11,12,17,18,19
//  #  #  #  #  #  #      1,4,7,10,13,16
const MONSTER_INDICES: [(isize, isize); 15] = [
    (0, 0),
    (-1, 18),
    (0, 5),
    (0, 6),
    (0, 11),
    (0, 12),
    (0, 17),
    (0, 18),
    (0, 19),
    (1, 1),
    (1, 4),
    (1, 7),
    (1, 10),
    (1, 13),
    (1, 16),
];

fn rotate_flip_monster(rotation: usize, flip: usize) -> Vec<(isize, isize)> {
    let monster = rotate_monster(rotation, MONSTER_INDICES.to_vec());

    match flip {
        0 => monster,
        1 => flip_monster_horizontal(monster),
        2 => flip_monster_vertical(monster),
        3 => {
            let flipped = flip_monster_horizontal(monster);
            flip_monster_vertical(flipped)
        }
        _ => panic!("unexpected flip index {}", flip),
    }
}

fn rotate_monster(rotation: usize, monster: Vec<(isize, isize)>) -> Vec<(isize, isize)> {
    if rotation == 0 {
        return monster;
    }

    let rotated = monster.into_iter().map(|(i, j)| (j, -i)).collect();
    rotate_monster(rotation - 1, rotated)
}

fn flip_monster_horizontal(monster: Vec<(isize, isize)>) -> Vec<(isize, isize)> {
    monster.into_iter().map(|(i, j)| (i, -j)).collect()
}

fn flip_monster_vertical(monster: Vec<(isize, isize)>) -> Vec<(isize, isize)> {
    monster.into_iter().map(|(i, j)| (-i, j)).collect()
}

fn remove_monsters(
    image: &[Vec<Pixel>],
    monsters_removed: &mut [Vec<Pixel>],
    monster: &[(isize, isize)],
) {
    for i in 0..image.len() {
        for j in 0..image[i].len() {
            if found_monster(image, (i, j), monster) {
                remove_monster(monsters_removed, (i, j), monster);
            }
        }
    }
}

fn found_monster(image: &[Vec<Pixel>], (i, j): (usize, usize), monster: &[(isize, isize)]) -> bool {
    for &(x, y) in monster.iter() {
        let u = ((i as isize) + x) as usize;
        let v = ((j as isize) + y) as usize;

        if !valid_coords((u, v), image.len()) {
            return false;
        }

        if image[u][v] != Pixel::One {
            return false;
        }
    }

    true
}

fn valid_coords((i, j): (usize, usize), size: usize) -> bool {
    i < size && j < size
}

fn remove_monster(
    monsters_removed: &mut [Vec<Pixel>],
    (i, j): (usize, usize),
    monster: &[(isize, isize)],
) {
    for &(x, y) in monster {
        let u = ((i as isize) + x) as usize;
        let v = ((j as isize) + y) as usize;

        monsters_removed[u][v] = Pixel::Zero;
    }
}

fn count_pixels(image: &[Vec<Pixel>]) -> usize {
    image
        .iter()
        .map(|row| row.iter().filter(|&&pixel| pixel == Pixel::One).count())
        .sum()
}

fn strip_edges_and_merge(tiles: Vec<Vec<Tile>>) -> Vec<Vec<Pixel>> {
    let stripped = strip_edges(tiles);
    merge_tiles(stripped)
}

fn strip_edges(tiles: Vec<Vec<Tile>>) -> Vec<Vec<Tile>> {
    tiles
        .into_iter()
        .map(|row| row.into_iter().map(|tile| tile.strip_edges()).collect())
        .collect()
}

fn merge_tiles(tiles: Vec<Vec<Tile>>) -> Vec<Vec<Pixel>> {
    let mut merged = Vec::new();

    for tile_row in tiles {
        for i in 0..tile_row[0].size {
            let mut next_pixel_row: Vec<Pixel> = Vec::new();
            for pixel_row in tile_row.iter().map(|tile| &tile.contents[i]) {
                next_pixel_row.append(&mut pixel_row.clone());
            }
            merged.push(next_pixel_row);
        }
    }

    merged
}

// fn pixels_to_string(pixels: Vec<Vec<Pixel>>) -> String {
//     let rows: Vec<String> = pixels
//         .into_iter()
//         .map(|row| row.into_iter().map(|pixel| pixel.to_char()).collect())
//         .collect();

//     rows.join("\n")
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rotate_tile() {
        let parser = TileParser::new();

        let tile = parser.parse(
            "Tile 1:
####
#...
#...
#...",
        );
        assert_eq!(tile.size, 4);

        let expected = parser.parse(
            "Tile 1:
#...
#...
#...
####",
        );
        assert_eq!(expected.size, 4);

        let rotated = tile.rotate_anticlockwise(1);
        assert_eq!(rotated, expected);
    }

    #[test]
    fn test_flip_vertical() {
        let parser = TileParser::new();

        let tile = parser.parse(
            "Tile 1:
####
#...
#...
#...",
        );
        assert_eq!(tile.size, 4);

        let expected = parser.parse(
            "Tile 1:
#...
#...
#...
####",
        );
        assert_eq!(expected.size, 4);

        let flipped = flip_vertical(&tile.contents, tile.size);
        assert_eq!(flipped, expected.contents);
    }

    #[test]
    fn test_flip_horizontal() {
        let parser = TileParser::new();

        let tile = parser.parse(
            "Tile 1:
####
#...
#...
#...",
        );
        assert_eq!(tile.size, 4);

        let expected = parser.parse(
            "Tile 1:
####
...#
...#
...#",
        );
        assert_eq!(expected.size, 4);

        let flipped = flip_horizontal(&tile.contents, tile.size);
        assert_eq!(flipped, expected.contents);
    }
}
