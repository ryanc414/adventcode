use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::iter::FromIterator;

fn main() {
    let filename = parse_args();
    let input = load_input(&filename);

    let &root = input.bags_by_name.get("shiny gold").unwrap();

    let num_parents = input.find_parents(root).len();
    println!("shiny gold bag can be contained by {} others", num_parents);

    let num_children = input.count_children(root);
    println!("shiny gold bag must contain {} other bags", num_children);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }
    args.remove(1)
}

struct BagGraph {
    bags: Vec<Bag>,
    bags_by_name: HashMap<String, usize>,
}

impl BagGraph {
    fn find_parents(&self, node_ix: usize) -> HashSet<usize> {
        let mut parents: HashSet<usize> = self.bags[node_ix].parents.iter().cloned().collect();
        for &i in self.bags[node_ix].parents.iter() {
            parents.extend(self.find_parents(i));
        }
        parents
    }

    fn count_children(&self, node_ix: usize) -> usize {
        self.bags[node_ix].children.iter().fold(0, |count, child| {
            count + child.count * (1 + self.count_children(child.index))
        })
    }
}

impl<'a> FromIterator<&'a str> for BagGraph {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
        let line_re = Regex::new(r"^([a-z ]+) bags contain ([a-z0-9, ]+)\.$").unwrap();
        let (mut bags, child_text): (Vec<Bag>, Vec<String>) = iter
            .into_iter()
            .map(|line: &str| -> (Bag, String) {
                let caps = line_re.captures(line).unwrap();
                (
                    Bag {
                        name: caps[1].to_string(),
                        children: Vec::new(),
                        parents: Vec::new(),
                    },
                    caps[2].to_string(),
                )
            })
            .unzip();

        let bag_map: HashMap<String, usize> = bags
            .iter()
            .enumerate()
            .map(|(i, bag)| (bag.name.clone(), i))
            .collect();

        let children_re = Regex::new(r"^(\d+) ([a-z ]+) bags?$").unwrap();
        for (i, line) in child_text.iter().enumerate() {
            bags[i].parse_children(line, &children_re, &bag_map);
        }

        for i in 0..bags.len() {
            for j in 0..bags[i].children.len() {
                let child_ix = bags[i].children[j].index;
                bags[child_ix].parents.push(i);
            }
        }

        Self {
            bags,
            bags_by_name: bag_map,
        }
    }
}

#[derive(Debug, Clone)]
struct Bag {
    name: String,
    children: Vec<ChildInfo>,
    parents: Vec<usize>,
}

impl Bag {
    fn parse_children(
        &mut self,
        line: &str,
        children_re: &Regex,
        bag_names: &HashMap<String, usize>,
    ) {
        if line == "no other bags" {
            return;
        }

        self.children = line
            .split(", ")
            .map(|item: &str| -> ChildInfo {
                let caps = children_re.captures(item).unwrap();
                let num: usize = caps[1].parse().unwrap();
                let name = &caps[2];
                ChildInfo {
                    count: num,
                    index: *bag_names.get(name).unwrap(),
                }
            })
            .collect();
    }
}

#[derive(Debug, Clone)]
struct ChildInfo {
    count: usize,
    index: usize,
}

fn load_input(filename: &str) -> BagGraph {
    let contents = fs::read_to_string(filename).expect("error reading the file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let input = load_input("basic_input.txt");

        let &root = input.bags_by_name.get("shiny gold").unwrap();

        let num_parents = input.find_parents(root).len();
        assert_eq!(num_parents, 4);

        let num_children = input.count_children(root);
        assert_eq!(num_children, 32);
    }

    #[test]
    fn test_full_input() {
        let input = load_input("full_input.txt");

        let &root = input.bags_by_name.get("shiny gold").unwrap();

        let num_parents = input.find_parents(root).len();
        assert_eq!(num_parents, 164);

        let num_children = input.count_children(root);
        assert_eq!(num_children, 7872);
    }
}
