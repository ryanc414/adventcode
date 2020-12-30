use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::hash::Hash;
use std::iter::FromIterator;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let arranged = arrange_tiles(&input);

    let corner_product = multiply_corner_ids(&arranged);
    println!("product of corners IDs is {}", corner_product);

    let roughness = find_water_roughness(arranged);
    println!("water roughness: {}", roughness);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    args.remove(1)
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

impl Default for Pixel {
    fn default() -> Self {
        Self::Zero
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Grid<T> {
    size: usize,
    elements: Vec<T>,
}

impl Grid<Pixel> {
    fn new(size: usize) -> Self {
        Self {
            size,
            elements: vec![Default::default(); size * size],
        }
    }

    fn get(&self, (x, y): (usize, usize)) -> &Pixel {
        let ix = self.coords_ix((x, y));
        &self.elements[ix]
    }

    fn set(&mut self, (x, y): (usize, usize), val: Pixel) {
        let ix = self.coords_ix((x, y));
        self.elements[ix] = val;
    }

    fn coords_ix(&self, (x, y): (usize, usize)) -> usize {
        if !self.valid_coords((x, y)) {
            panic!("invalid coords: {:?}", (x, y));
        }
        (y * self.size) + x
    }

    fn get_edge(&self, edge: EdgePos) -> Vec<Pixel> {
        match edge {
            EdgePos::Top => self.elements[..self.size].iter().cloned().collect(),
            EdgePos::Right => self
                .elements
                .iter()
                .enumerate()
                .filter(|(i, _)| (i + 1) % self.size == 0)
                .map(|(_, el)| el)
                .cloned()
                .collect(),
            EdgePos::Bottom => self.elements[self.size * (self.size - 1)..]
                .iter()
                .cloned()
                .collect(),
            EdgePos::Left => self
                .elements
                .iter()
                .enumerate()
                .filter(|(i, _)| i % self.size == 0)
                .map(|(_, el)| el)
                .cloned()
                .collect(),
        }
    }

    fn rotate_anticlockwise(&self, times: usize) -> Self {
        if times == 0 {
            return self.clone();
        }

        let mut new_contents = Self::new(self.size);

        for i in 0..self.size {
            for j in 0..self.size {
                new_contents.set((i, j), *self.get((self.size - j - 1, i)));
            }
        }

        if times > 1 {
            new_contents.rotate_anticlockwise(times - 1)
        } else {
            new_contents
        }
    }

    fn flip_to_match(&self, other: &Self, other_edge_pos: EdgePos) -> Self {
        let other_edge = other.get_edge(other_edge_pos);
        let self_edge = self.get_edge(other_edge_pos.opposite());

        if other_edge == self_edge {
            return self.clone();
        }

        let horizontal_flip = other_edge_pos.is_horizontal();

        if horizontal_flip {
            self.flip_horizontal()
        } else {
            self.flip_vertical()
        }
    }

    fn flip_vertical(&self) -> Self {
        let mut new_contents = Self::new(self.size);

        for i in 0..self.size {
            for j in 0..self.size {
                new_contents.set((i, self.size - j - 1), *self.get((i, j)));
            }
        }

        new_contents
    }

    fn flip_horizontal(&self) -> Self {
        let mut new_contents = Self::new(self.size);

        for i in 0..self.size {
            for j in 0..self.size {
                new_contents.set((self.size - i - 1, j), *self.get((i, j)));
            }
        }

        new_contents
    }

    fn strip_edges(&self) -> Self {
        let mut new_contents = Self::new(self.size - 2);

        for i in 1..(self.size - 1) {
            for j in 1..(self.size - 1) {
                new_contents.set((i - 1, j - 1), *self.get((i, j)));
            }
        }

        new_contents
    }

    fn valid_coords(&self, (i, j): (usize, usize)) -> bool {
        i < self.size && j < self.size
    }

    fn to_string(&self) -> String {
        let mut res = String::new();

        for i in 0..self.size {
            for j in 0..self.size {
                let pixel = self.get((i, j));
                res.push(pixel.to_char());
            }
            res.push('\n');
        }

        res
    }
}

impl<'a> FromIterator<&'a str> for Grid<Pixel> {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
        let lines: Vec<&str> = iter.into_iter().collect();
        let size = lines.len();

        let mut contents = Self::new(size);

        for (i, l) in lines.into_iter().enumerate() {
            let pixels: Vec<Pixel> = l.chars().map(Pixel::parse).collect();
            if pixels.len() != size {
                panic!("cannot parse input as a grid");
            }

            for (j, p) in pixels.into_iter().enumerate() {
                contents.set((j, i), p);
            }
        }

        contents
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum EdgePos {
    Top,
    Right,
    Bottom,
    Left,
}

impl EdgePos {
    fn opposite(&self) -> Self {
        match self {
            Self::Top => Self::Bottom,
            Self::Right => Self::Left,
            Self::Bottom => Self::Top,
            Self::Left => Self::Right,
        }
    }

    fn is_horizontal(&self) -> bool {
        match self {
            Self::Top | Self::Bottom => true,
            Self::Right | Self::Left => false,
        }
    }

    fn to_num(&self) -> usize {
        match self {
            Self::Top => 0,
            Self::Right => 1,
            Self::Bottom => 2,
            Self::Left => 3,
        }
    }

    fn rotations_to(&self, other: &Self) -> usize {
        let edge_ix = self.to_num();
        let required_ix = other.to_num();
        ((edge_ix + 4) - required_ix) % 4
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct NormalisedEdge(Vec<Pixel>);

impl NormalisedEdge {
    fn new(mut pixels: Vec<Pixel>) -> Self {
        let middle = pixels.len() / 2;
        let first_half: Vec<Pixel> = pixels[..middle].to_vec();
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
    contents: Grid<Pixel>,
    normalised_edges: [(EdgePos, NormalisedEdge); 4],
}

impl Tile {
    fn from_contents(contents: Grid<Pixel>, id: u64) -> Self {
        let top = contents.get_edge(EdgePos::Top);
        let right = contents.get_edge(EdgePos::Right);
        let bottom = contents.get_edge(EdgePos::Bottom);
        let left = contents.get_edge(EdgePos::Left);

        Self {
            id,
            contents,
            normalised_edges: [
                (EdgePos::Top, NormalisedEdge::new(top)),
                (EdgePos::Right, NormalisedEdge::new(right)),
                (EdgePos::Bottom, NormalisedEdge::new(bottom)),
                (EdgePos::Left, NormalisedEdge::new(left)),
            ],
        }
    }

    fn rotate_starting_corner(
        &self,
        edge_shares: &HashMap<&NormalisedEdge, Vec<(&Tile, EdgePos)>>,
    ) -> Self {
        let unmatched_edges: Vec<EdgePos> = self
            .normalised_edges
            .iter()
            .filter(|(_, edge)| edge_shares.get(edge).unwrap().len() == 1)
            .map(|&(pos, _)| pos)
            .collect();
        assert_eq!(unmatched_edges.len(), 2);

        if unmatched_edges[0] == EdgePos::Top && unmatched_edges[1] == EdgePos::Left {
            return self.clone();
        }

        let rotations = (unmatched_edges[0].to_num() + 1) % 4;
        let rotated = self.contents.rotate_anticlockwise(rotations);
        Self::from_contents(rotated, self.id)
    }

    fn rotate_and_flip(
        &self,
        match_edge: EdgePos,
        self_edge: EdgePos,
        matching_tile: &Self,
    ) -> Self {
        let required_edge = match_edge.opposite();
        let rotations = self_edge.rotations_to(&required_edge);
        let rotated = self.contents.rotate_anticlockwise(rotations);
        let flipped = rotated.flip_to_match(&matching_tile.contents, match_edge);
        Self::from_contents(flipped, self.id)
    }
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
        // let contents: Vec<Vec<Pixel>> = it
        //     .map(|line| line.chars().map(Pixel::parse).collect())
        //     .collect();
        let contents: Grid<Pixel> = it.collect();
        Tile::from_contents(contents, id)
    }
}

fn load_input(filename: &str) -> Vec<Tile> {
    let contents = fs::read_to_string(filename).expect("could not read input file");
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
    edge_shares: &HashMap<&NormalisedEdge, Vec<(&Tile, EdgePos)>>,
    corners: Vec<&Tile>,
    arrangement_size: usize,
) -> HashMap<(usize, usize), Tile> {
    // start withan aritrary corner.
    let start_tile = &corners[0];

    let mut tile_coords: HashMap<(usize, usize), Tile> = HashMap::new();
    let rotated_tile = start_tile.rotate_starting_corner(edge_shares);

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
    edge_shares: &HashMap<&NormalisedEdge, Vec<(&Tile, EdgePos)>>,
    arrangement_size: usize,
) {
    tile_coords.insert(curr_coords, curr_tile.clone());

    for (pos, edge) in curr_tile.normalised_edges.iter() {
        let edge_tiles = edge_shares.get(edge).unwrap();

        let neighbour_coords = match calculate_neighbour_coords(curr_coords, *pos, arrangement_size)
        {
            Some(coords) => coords,
            None => {
                if edge_tiles.len() != 1 {
                    panic!(
                        "more than 1 tiles share edge: {} {:?} {:?}",
                        edge_tiles.len(),
                        pos,
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
                "only {} tiles share edge {:?} of tile {:?}",
                edge_tiles.len(),
                pos,
                curr_coords
            );
        }

        let (next_tile, edge_ix) = edge_tiles
            .iter()
            .find(|&&(tile, _)| tile.id != curr_tile.id)
            .unwrap();

        let rotated_tile = next_tile.rotate_and_flip(*pos, *edge_ix, &curr_tile);

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
    edge_pos: EdgePos,
    size: usize,
) -> Option<(usize, usize)> {
    let dir = EDGE_DIRS[edge_pos.to_num()];

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

fn find_edge_shares(tiles: &[Tile]) -> HashMap<&NormalisedEdge, Vec<(&Tile, EdgePos)>> {
    let mut edge_map: HashMap<&NormalisedEdge, Vec<(&Tile, EdgePos)>> = HashMap::new();

    for tile in tiles {
        for (pos, edge) in tile.normalised_edges.iter() {
            edge_map.entry(edge).or_default().push((tile, *pos));
        }
    }

    edge_map
}

fn count_tile_unmatched_edges<'a, 'b>(
    edge_shares: &'a HashMap<&'b NormalisedEdge, Vec<(&'b Tile, EdgePos)>>,
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
    image: &Grid<Pixel>,
    monsters_removed: &mut Grid<Pixel>,
    monster: &[(isize, isize)],
) {
    for i in 0..image.size {
        for j in 0..image.size {
            if found_monster(image, (i, j), monster) {
                remove_monster(monsters_removed, (i, j), monster);
            }
        }
    }
}

fn found_monster(image: &Grid<Pixel>, (i, j): (usize, usize), monster: &[(isize, isize)]) -> bool {
    for &(x, y) in monster.iter() {
        let u = ((i as isize) + x) as usize;
        let v = ((j as isize) + y) as usize;

        if !image.valid_coords((u, v)) {
            return false;
        }

        if *image.get((u, v)) != Pixel::One {
            return false;
        }
    }

    true
}

fn remove_monster(
    monsters_removed: &mut Grid<Pixel>,
    (i, j): (usize, usize),
    monster: &[(isize, isize)],
) {
    for &(x, y) in monster {
        let u = ((i as isize) + x) as usize;
        let v = ((j as isize) + y) as usize;

        monsters_removed.set((u, v), Pixel::Zero);
    }
}

fn count_pixels(image: &Grid<Pixel>) -> usize {
    image
        .elements
        .iter()
        .filter(|&&pixel| pixel == Pixel::One)
        .count()
}

fn strip_edges_and_merge(tiles: Vec<Vec<Tile>>) -> Grid<Pixel> {
    let stripped = strip_edges(tiles);
    merge_tiles(stripped)
}

fn strip_edges(tiles: Vec<Vec<Tile>>) -> Vec<Vec<Grid<Pixel>>> {
    tiles
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|tile| tile.contents.strip_edges())
                .collect()
        })
        .collect()
}

fn merge_tiles(tiles: Vec<Vec<Grid<Pixel>>>) -> Grid<Pixel> {
    let size = tiles[0][0].size * tiles.len();
    let mut merged: Grid<Pixel> = Grid::new(size);

    for (row_ix, tile_row) in tiles.iter().enumerate() {
        for (col_ix, tile) in tile_row.iter().enumerate() {
            for i in 0..tile.size {
                for j in 0..tile.size {
                    let pixel = tile.get((i, j));
                    let coords = (i + (col_ix * tile.size), j + (row_ix * tile.size));
                    merged.set(coords, *pixel);
                }
            }
        }
    }

    merged
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let arranged = arrange_tiles(&input);

        let corner_product = multiply_corner_ids(&arranged);
        assert_eq!(corner_product, 20899048083289);

        let roughness = find_water_roughness(arranged);
        assert_eq!(roughness, 273);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let arranged = arrange_tiles(&input);

        let corner_product = multiply_corner_ids(&arranged);
        assert_eq!(corner_product, 83775126454273);

        let roughness = find_water_roughness(arranged);
        assert_eq!(roughness, 1993);
    }

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
        assert_eq!(tile.contents.size, 4);

        let expected = parser.parse(
            "Tile 1:
#...
#...
#...
####",
        );
        assert_eq!(expected.contents.size, 4);

        let rotated = tile.contents.rotate_anticlockwise(1);
        assert_eq!(rotated, expected.contents);
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
        assert_eq!(tile.contents.size, 4);

        let expected = parser.parse(
            "Tile 1:
#...
#...
#...
####",
        );
        assert_eq!(expected.contents.size, 4);

        let flipped = tile.contents.flip_vertical();
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
        assert_eq!(tile.contents.size, 4);

        let expected = parser.parse(
            "Tile 1:
####
...#
...#
...#",
        );
        assert_eq!(expected.contents.size, 4);

        let flipped = tile.contents.flip_horizontal();
        assert_eq!(flipped, expected.contents);
    }

    #[test]
    fn test_strip_and_merge() {
        let parser = TileParser::new();

        let tile = parser.parse(
            "Tile 1:
####
##..
#.#.
#...",
        );

        let tiles = vec![vec![tile; 3]; 3];
        let result = strip_edges_and_merge(tiles);
        println!("result:\n{}", result.to_string());

        let expected: Grid<Pixel> = "#.#.#.
.#.#.#
#.#.#.
.#.#.#
#.#.#.
.#.#.#"
            .split('\n')
            .collect();

        assert_eq!(result, expected);
    }
}
