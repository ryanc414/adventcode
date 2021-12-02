use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let filename = parse_args();
    let mut input = load_input(&filename);

    let num_matches_1 = find_num_matches(&input.0, &input.1);
    println!("found {} matches using original rules", num_matches_1);

    update_rules(&mut input.0);
    let num_matches_2 = find_num_matches(&input.0, &input.1);
    println!("found {} matches using updated rules", num_matches_2);
}

fn parse_args() -> String {
    let mut args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    args.remove(1)
}

enum Rule {
    Literal(char),
    Combination(Vec<usize>),
    Option(Vec<usize>, Vec<usize>),
}

struct RuleParser {
    line_re: Regex,
    option_re: Regex,
    combination_re: Regex,
    literal_re: Regex,
}

impl RuleParser {
    fn new() -> Self {
        Self {
            line_re: Regex::new(r"^(\d+): (.+)$").unwrap(),
            option_re: Regex::new(r"^([0-9 ]+) \| ([0-9 ]+)$").unwrap(),
            combination_re: Regex::new(r"^[0-9 ]+$").unwrap(),
            literal_re: Regex::new("^\"([a-z])\"$").unwrap(),
        }
    }

    fn parse(&self, line: &str) -> (usize, Rule) {
        let line_caps = self.line_re.captures(line).unwrap();
        let rule_num: usize = line_caps[1].parse().unwrap();
        let rule_contents = &line_caps[2];

        if let Some(rule_caps) = self.option_re.captures(rule_contents) {
            let left: Vec<usize> = rule_caps[1]
                .split(' ')
                .map(|rule_num| rule_num.parse().unwrap())
                .collect();
            let right: Vec<usize> = rule_caps[2]
                .split(' ')
                .map(|rule_num| rule_num.parse().unwrap())
                .collect();
            return (rule_num, Rule::Option(left, right));
        }

        if self.combination_re.is_match(rule_contents) {
            let combo: Vec<usize> = rule_contents
                .split(' ')
                .map(|rule_num| rule_num.parse().unwrap())
                .collect();
            return (rule_num, Rule::Combination(combo));
        }

        let rule_caps = self.literal_re.captures(rule_contents).unwrap();
        let literal_char = rule_caps[1].chars().next().unwrap();

        (rule_num, Rule::Literal(literal_char))
    }
}

fn load_input(filename: &str) -> (HashMap<usize, Rule>, Vec<String>) {
    let contents = fs::read_to_string(filename).expect("could not read input file");
    let sections: Vec<&str> = contents
        .split("\n\n")
        .filter(|section| !section.is_empty())
        .collect();
    if sections.len() != 2 {
        panic!("unexpected number of sections in input");
    }

    let parser = RuleParser::new();

    let rules: HashMap<usize, Rule> = sections[0]
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| parser.parse(line))
        .collect();

    let messages: Vec<String> = sections[1]
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| line.to_string())
        .collect();

    (rules, messages)
}

fn find_num_matches(rules: &HashMap<usize, Rule>, messages: &[String]) -> usize {
    let rule = rules.get(&0).unwrap();
    messages
        .iter()
        .filter(|message| validate_rule(rules, rule, message))
        .count()
}

fn validate_rule(all_rules: &HashMap<usize, Rule>, rule: &Rule, message: &str) -> bool {
    let counts = validate_rule_recur(all_rules, rule, message);
    counts.iter().any(|&c| c == message.len())
}

fn validate_rule_recur(all_rules: &HashMap<usize, Rule>, rule: &Rule, message: &str) -> Vec<usize> {
    if message.is_empty() {
        return Vec::new();
    }

    match rule {
        &Rule::Literal(c) => {
            if message.chars().next().unwrap() == c {
                vec![1]
            } else {
                Vec::new()
            }
        }
        Rule::Combination(rules) => validate_rule_combo(rules, all_rules, message),
        Rule::Option(left, right) => {
            let mut left_counts = validate_rule_combo(left, all_rules, message);
            let mut right_counts = validate_rule_combo(right, all_rules, message);
            left_counts.append(&mut right_counts);
            left_counts
        }
    }
}

fn validate_rule_combo(
    rules: &[usize],
    all_rules: &HashMap<usize, Rule>,
    message: &str,
) -> Vec<usize> {
    let next_rule = &rules[0];
    let rest_rules = &rules[1..];

    let rule_counts = validate_rule_recur(all_rules, &all_rules[next_rule], message);
    if rest_rules.is_empty() {
        return rule_counts;
    }

    let mut all_counts: Vec<usize> = Vec::new();

    for count in rule_counts {
        let child_counts = validate_rule_combo(rest_rules, all_rules, &message[count..]);
        for c in child_counts.iter().map(|child_count| count + child_count) {
            all_counts.push(c);
        }
    }

    all_counts
}

fn update_rules(rules: &mut HashMap<usize, Rule>) {
    rules.insert(8, Rule::Option(vec![42], vec![42, 8]));
    rules.insert(11, Rule::Option(vec![42, 31], vec![42, 11, 31]));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_input() {
        let mut input = load_input("basic_input.txt");

        let num_matches = find_num_matches(&input.0, &input.1);
        assert_eq!(num_matches, 2);

        update_rules(&mut input.0);
        let num_matches = find_num_matches(&input.0, &input.1);
        assert_eq!(num_matches, 2);
    }

    #[test]
    fn test_full_input() {
        let mut input = load_input("full_input.txt");

        let num_matches = find_num_matches(&input.0, &input.1);
        assert_eq!(num_matches, 222);

        update_rules(&mut input.0);
        let num_matches = find_num_matches(&input.0, &input.1);
        assert_eq!(num_matches, 339);
    }
}
