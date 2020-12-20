use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let num_matches = find_num_matches(&input.0, &input.1);
    println!("found {} matches", num_matches);
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

fn load_input() -> (HashMap<usize, Rule>, Vec<String>) {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read input file");
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
    match validate_rule_recur(all_rules, rule, message) {
        Some(count) => count == message.len(),
        None => false,
    }
}

fn validate_rule_recur(
    all_rules: &HashMap<usize, Rule>,
    rule: &Rule,
    message: &str,
) -> Option<usize> {
    match rule {
        &Rule::Literal(c) => {
            if message.chars().next().unwrap() == c {
                Some(1)
            } else {
                None
            }
        }
        Rule::Combination(rules) => validate_rule_combo(rules, all_rules, message),
        Rule::Option(left, right) => {
            if let Some(count) = validate_rule_combo(left, all_rules, message) {
                return Some(count);
            }

            validate_rule_combo(right, all_rules, message)
        }
    }
}

fn validate_rule_combo(
    rules: &[usize],
    all_rules: &HashMap<usize, Rule>,
    message: &str,
) -> Option<usize> {
    let mut ix = 0;

    for rule_ix in rules {
        match validate_rule_recur(all_rules, &all_rules[rule_ix], &message[ix..]) {
            Some(count) => {
                ix += count;
            }
            None => return None,
        }
    }

    Some(ix)
}
