use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let mut allergen_possibles = find_allergen_possibles(&input);

    let count = count_non_allergen_ingredients(&input, &allergen_possibles);
    println!("non-allergenic ingredients appear {} times", count);

    let dangerous_ingredients = find_dangerous_ingredients(&mut allergen_possibles);
    println!(
        "canonical dangerous ingredients list is {}",
        dangerous_ingredients
    );
}

struct IngredientsList {
    ingredients: Vec<String>,
    allergens: Vec<String>,
}

struct IngredientsListParser {
    line_re: Regex,
}

impl IngredientsListParser {
    fn new() -> Self {
        let line_re = Regex::new(r"^([a-z ]+) \(contains ([a-z ,]+)\)$").unwrap();
        Self { line_re }
    }

    fn parse(&self, line: &str) -> IngredientsList {
        let caps = self.line_re.captures(line).unwrap();
        let ingredients: Vec<String> = caps[1].split(' ').map(String::from).collect();
        let allergens: Vec<String> = caps[2].split(", ").map(String::from).collect();

        IngredientsList {
            ingredients,
            allergens,
        }
    }
}

fn load_input() -> Vec<IngredientsList> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read input file");
    let parser = IngredientsListParser::new();

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parser.parse(line))
        .collect()
}

fn count_non_allergen_ingredients(
    foods: &[IngredientsList],
    allergen_possibles: &HashMap<&str, HashSet<&str>>,
) -> usize {
    let possibly_allergic = possible_allergic_ingredients(allergen_possibles);
    count_non_allergic(foods, &possibly_allergic)
}

fn find_allergen_possibles(foods: &[IngredientsList]) -> HashMap<&str, HashSet<&str>> {
    let mut allergens: HashMap<&str, HashSet<&str>> = HashMap::new();

    for food in foods {
        for allergen in food.allergens.iter() {
            let ingredients: HashSet<&str> = food
                .ingredients
                .iter()
                .map(|ingredient| ingredient.as_str())
                .collect();
            let new_possibles: HashSet<&str> = match allergens.get_mut(allergen.as_str()) {
                Some(possibles) => possibles.intersection(&ingredients).cloned().collect(),
                None => ingredients,
            };
            allergens.insert(&allergen, new_possibles);
        }
    }

    allergens
}

fn possible_allergic_ingredients<'a>(
    allergen_possibles: &HashMap<&str, HashSet<&'a str>>,
) -> HashSet<&'a str> {
    allergen_possibles.values().fold(
        HashSet::new(),
        |curr_ingredients: HashSet<&str>, new_ingredients: &HashSet<&str>| -> HashSet<&str> {
            curr_ingredients.union(new_ingredients).cloned().collect()
        },
    )
}

fn count_non_allergic(foods: &[IngredientsList], possible_allergic: &HashSet<&str>) -> usize {
    foods
        .iter()
        .map(|food| {
            food.ingredients
                .iter()
                .filter(|ingredient| !possible_allergic.contains(ingredient.as_str()))
                .count()
        })
        .sum()
}

fn find_dangerous_ingredients<'a>(
    allergen_possibles: &mut HashMap<&'a str, HashSet<&'a str>>,
) -> String {
    let dangerous_ingredients = deduce_dangerous_ingredients(allergen_possibles);
    canonical_list(dangerous_ingredients)
}

struct AllergenPair<'a> {
    allergen: &'a str,
    ingredient: &'a str,
}

fn deduce_dangerous_ingredients<'a>(
    allergen_possibles: &mut HashMap<&'a str, HashSet<&'a str>>,
) -> Vec<AllergenPair<'a>> {
    let allergens: Vec<&str> = allergen_possibles.keys().cloned().collect();

    for k in allergens.iter() {
        if let Some(ingredient) = get_lone_ingredient(allergen_possibles, k) {
            remove_possibility_recur(allergen_possibles, &allergens, k, ingredient);
        }
    }

    if !allergen_possibles.values().all(|set| set.len() == 1) {
        panic!("could not deduce a unique solution");
    }

    allergen_possibles
        .iter()
        .map(|(k, v)| AllergenPair {
            allergen: k,
            ingredient: v.iter().next().unwrap(),
        })
        .collect()
}

fn get_lone_ingredient<'a>(
    allergen_possibles: &HashMap<&str, HashSet<&'a str>>,
    allergen: &str,
) -> Option<&'a str> {
    let v = allergen_possibles.get(allergen).unwrap();
    if v.len() == 1 {
        Some(v.iter().next().unwrap())
    } else {
        None
    }
}

fn remove_possibility_recur(
    allergen_possibles: &mut HashMap<&str, HashSet<&str>>,
    all_allergens: &[&str],
    allergen: &str,
    ingredient: &str,
) {
    for &k in all_allergens {
        if k == allergen {
            continue;
        }

        if let Some(other_ingredient) = remove_ingredient(allergen_possibles, k, ingredient) {
            remove_possibility_recur(allergen_possibles, all_allergens, k, other_ingredient);
        }
    }
}

fn remove_ingredient<'a>(
    allergen_possibles: &mut HashMap<&str, HashSet<&'a str>>,
    allergen: &str,
    ingredient: &str,
) -> Option<&'a str> {
    let v = allergen_possibles.get_mut(allergen).unwrap();
    let removed = v.remove(ingredient);

    if removed && v.len() == 1 {
        Some(v.iter().next().unwrap())
    } else {
        None
    }
}

fn canonical_list(mut dangerous_ingredients: Vec<AllergenPair>) -> String {
    dangerous_ingredients.sort_by_key(|pair| pair.allergen);
    dangerous_ingredients
        .into_iter()
        .map(|pair| pair.ingredient)
        .collect::<Vec<&str>>()
        .join(",")
}
