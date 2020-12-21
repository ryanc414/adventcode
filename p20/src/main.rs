use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::hash::Hash;

fn main() {
    let input = load_input();
    let corner_product = find_corner_tiles(&input);
    println!("product of corners IDs is {}", corner_product);
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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Edge(Vec<Pixel>);

impl Edge {
    fn new(mut pixels: Vec<Pixel>) -> Self {
        let middle = pixels.len() / 2;
        let first_half = pixels[..middle].iter();
        let second_half = pixels[middle..].iter().rev();
        if first_half.len() != second_half.len() {
            panic!("unequal halves");
        }

        if first_half.gt(second_half) {
            pixels.reverse();
        }

        Self(pixels)
    }
}

#[derive(Debug)]
struct Tile {
    id: u64,
    contents: Vec<Vec<Pixel>>,
    edges: [Edge; 4],
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

        let top: Vec<Pixel> = contents[0].to_vec();
        let bottom: Vec<Pixel> = contents[contents.len() - 1].to_vec();
        let left: Vec<Pixel> = contents.iter().map(|line| line[0]).collect();
        let right: Vec<Pixel> = contents.iter().map(|line| line[line.len() - 1]).collect();

        Tile {
            id,
            contents,
            edges: [
                Edge::new(top),
                Edge::new(right),
                Edge::new(bottom),
                Edge::new(left),
            ],
        }
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

fn find_corner_tiles(tiles: &[Tile]) -> u64 {
    let edge_shares = count_edge_shares(tiles);
    let tiles_with_unmatched_edges = find_tiles_with_unmatched_edges(&edge_shares);
    let corners = find_corners(&tiles_with_unmatched_edges, tiles);

    if corners.len() != 4 {
        panic!("expected 4 corners, got {}", corners.len())
    }

    corners.iter().map(|tile| tile.id).product()
}

fn count_edge_shares(tiles: &[Tile]) -> HashMap<&Edge, Vec<(usize, usize)>> {
    let mut edge_map: HashMap<&Edge, Vec<(usize, usize)>> = HashMap::new();

    for (i, tile) in tiles.iter().enumerate() {
        for (j, edge) in tile.edges.iter().enumerate() {
            edge_map.entry(edge).or_default().push((i, j));
        }
    }

    edge_map
}

fn find_tiles_with_unmatched_edges(
    edge_shares: &HashMap<&Edge, Vec<(usize, usize)>>,
) -> HashMap<usize, usize> {
    let mut tiles_unmatched_edges: HashMap<usize, usize> = HashMap::new();

    for v in edge_shares.values() {
        if v.len() == 1 {
            let counter = tiles_unmatched_edges.entry(v[0].0).or_default();
            *counter += 1;
        }
    }

    tiles_unmatched_edges
}

fn find_corners<'a, 'b>(
    tiles_with_unmatched_edges: &'a HashMap<usize, usize>,
    tiles: &'b [Tile],
) -> Vec<&'b Tile> {
    tiles_with_unmatched_edges
        .iter()
        .filter(|&(_, &v)| v == 2)
        .map(|(&ix, _)| &tiles[ix])
        .collect()
}
