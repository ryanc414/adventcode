use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;

fn main() {
    let input = load_input();

    let invalid_vals = input.find_all_invalid_values();
    let err_rate = input.find_err_rate(&invalid_vals);
    println!("ticket scanning error rate is {}", err_rate);

    let result = input.multiply_departures(&invalid_vals);
    println!("multiple of all departure fields is {}", result);
}

type Ticket = Vec<u64>;

struct TicketInfo {
    rules: Vec<TicketRule>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

impl TicketInfo {
    fn parse(input: &str) -> Self {
        let sections: Vec<&str> = input
            .split("\n\n")
            .filter(|section| !section.is_empty())
            .collect();
        if sections.len() != 3 {
            panic!("unexpected number of sections in input: {}", sections.len());
        }

        let rules = Self::parse_rules(&sections[0]);
        let my_ticket = Self::parse_my_ticket(&sections[1]);
        let nearby_tickets = Self::parse_nearby_tickets(&sections[2]);

        Self {
            rules,
            my_ticket,
            nearby_tickets,
        }
    }

    fn parse_rules(rules_section: &str) -> Vec<TicketRule> {
        let rules_re = Regex::new(r"^([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)\s*$").unwrap();
        rules_section
            .split('\n')
            .map(|line| {
                let caps = rules_re.captures(line).unwrap();
                TicketRule {
                    field_name: caps[1].to_string(),
                    ranges: [
                        (caps[2].parse().unwrap(), caps[3].parse().unwrap()),
                        (caps[4].parse().unwrap(), caps[5].parse().unwrap()),
                    ],
                }
            })
            .collect()
    }

    fn parse_my_ticket(input: &str) -> Ticket {
        let lines: Vec<&str> = input.split('\n').collect();
        if lines.len() != 2 {
            panic!("unexpected number of lines for ticket input");
        }
        if lines[0] != "your ticket:" {
            panic!("unexpected first line of my ticket input: {}", lines[0]);
        }

        Self::parse_ticket(&lines[1])
    }

    fn parse_nearby_tickets(section: &str) -> Vec<Ticket> {
        let lines: Vec<&str> = section.split('\n').collect();
        if lines[0] != "nearby tickets:" {
            panic!(
                "unexpected first line of nearby tickets input: {}",
                lines[0]
            );
        }

        lines[1..]
            .iter()
            .filter(|line| !line.is_empty())
            .map(|&line| Self::parse_ticket(line))
            .collect()
    }

    fn parse_ticket(line: &str) -> Ticket {
        line.split(',').map(|num| num.parse().unwrap()).collect()
    }

    fn find_all_invalid_values(&self) -> Vec<Vec<u64>> {
        let ranges = self.combine_ranges();
        self.nearby_tickets
            .iter()
            .map(|ticket| Self::find_invalid_values(ticket, &ranges))
            .collect()
    }

    fn find_err_rate(&self, invalid_vals: &[Vec<u64>]) -> u64 {
        invalid_vals
            .iter()
            .map(|vals: &Vec<u64>| -> u64 { vals.iter().sum() })
            .sum()
    }

    fn combine_ranges(&self) -> Vec<(u64, u64)> {
        // First, get both ranges from every rule and combine them in a single
        // vec. Sort by the lower range value for each.
        let mut all_ranges: Vec<(u64, u64)> = Vec::new();

        for rule in self.rules.iter() {
            all_ranges.push(rule.ranges[0]);
            all_ranges.push(rule.ranges[1]);
        }

        all_ranges.sort_unstable_by_key(|range| range.0);

        // Next, combine overlapping ranges. For example, 1-5 and 3-8 can be
        // combined to 1-8. This combination is extra work now but allows us
        // to make fewer checks on every ticket.
        let mut combined_ranges: Vec<(u64, u64)> = Vec::new();
        let mut i = 0;

        while i < all_ranges.len() {
            let lower = all_ranges[i].0;
            let mut upper = all_ranges[i].1;
            let mut j = i + 1;

            while j < all_ranges.len() {
                if all_ranges[j].0 > all_ranges[i].1 {
                    // no overlap
                    break;
                }

                if all_ranges[j].1 > upper {
                    upper = all_ranges[j].1;
                }
                j += 1;
            }

            combined_ranges.push((lower, upper));
            i = j;
        }

        combined_ranges
    }

    fn find_invalid_values(ticket: &[u64], ranges: &[(u64, u64)]) -> Vec<u64> {
        ticket
            .iter()
            .cloned()
            .filter(|&val| !ranges.iter().any(|range| Self::in_range(range, val)))
            .collect()
    }

    fn multiply_departures(&self, invalid_vals: &[Vec<u64>]) -> u64 {
        let ticket_field_order = self
            .infer_ticket_fields(invalid_vals)
            .expect("could not infer ticket fields");
        let my_ticket = self.build_ticket_map(&ticket_field_order);
        Self::multiply_ticket_departures(&my_ticket)
    }

    fn infer_ticket_fields(&self, invalid_vals: &[Vec<u64>]) -> Option<Vec<usize>> {
        let valid_tickets = self.valid_tickets(invalid_vals);

        let mut all_possible_fields: Vec<HashSet<usize>> =
            vec![(0..self.rules.len()).collect(); self.my_ticket.len()];

        for ticket in valid_tickets {
            for (i, &val) in ticket.iter().enumerate() {
                let possible_fields = self.possible_fields(val);
                all_possible_fields[i] = all_possible_fields[i]
                    .intersection(&possible_fields)
                    .cloned()
                    .collect();
            }
        }

        Self::infer_fields_from_possibles(&mut all_possible_fields)
    }

    fn infer_fields_from_possibles(
        all_possible_fields: &mut [HashSet<usize>],
    ) -> Option<Vec<usize>> {
        for i in 0..all_possible_fields.len() {
            // sanity check
            if all_possible_fields[i].is_empty() {
                return None;
            }

            if all_possible_fields[i].len() == 1 {
                // Found a val for this field! We can remove it as a possibility
                // from all the others.
                let &val = all_possible_fields[i].iter().next().unwrap();
                Self::remove_possibility(all_possible_fields, val, i);
            }
        }

        if !all_possible_fields
            .iter()
            .all(|possibles| possibles.len() == 1)
        {
            return None;
        }

        Some(
            all_possible_fields
                .iter()
                .map(|possibles| possibles.iter().next().unwrap())
                .cloned()
                .collect(),
        )
    }

    fn remove_possibility(all_possible_fields: &mut [HashSet<usize>], val: usize, i: usize) {
        for j in 0..all_possible_fields.len() {
            if i == j {
                continue;
            }
            let removed = all_possible_fields[j].remove(&val);
            if removed && all_possible_fields[j].len() == 1 {
                let &other_val = all_possible_fields[j].iter().next().unwrap();
                Self::remove_possibility(all_possible_fields, other_val, j);
            }
        }
    }

    fn possible_fields(&self, val: u64) -> HashSet<usize> {
        let mut fields: HashSet<usize> = HashSet::new();

        for (i, rule) in self.rules.iter().enumerate() {
            if Self::in_range(&rule.ranges[0], val) || Self::in_range(&rule.ranges[1], val) {
                fields.insert(i);
            }
        }

        fields
    }

    fn in_range(range: &(u64, u64), val: u64) -> bool {
        val >= range.0 && val <= range.1
    }

    fn valid_tickets(&self, invalid_vals: &[Vec<u64>]) -> Vec<&Ticket> {
        invalid_vals
            .iter()
            .enumerate()
            .filter(|(_, vals)| vals.is_empty())
            .map(|(i, _)| &self.nearby_tickets[i])
            .collect()
    }

    fn build_ticket_map(&self, field_indices: &[usize]) -> HashMap<String, u64> {
        field_indices
            .iter()
            .map(|&i| self.rules[i].field_name.to_string())
            .zip(self.my_ticket.iter().cloned())
            .collect()
    }

    fn multiply_ticket_departures(ticket: &HashMap<String, u64>) -> u64 {
        ticket.iter().fold(1, |product, (k, v)| {
            if k.starts_with("departure") {
                product * v
            } else {
                product
            }
        })
    }
}

struct TicketRule {
    field_name: String,
    ranges: [(u64, u64); 2],
}

fn load_input() -> TicketInfo {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename")
    }

    let contents = fs::read_to_string(&args[1]).expect("error reading input file");

    TicketInfo::parse(&contents)
}
