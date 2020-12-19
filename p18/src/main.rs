use std::env;
use std::fs;

fn main() {
    let input = load_input();
    let sum = sum_expressions(&input);
    println!("sum of expressions is {}", sum);
}

#[derive(Debug)]
enum Expression {
    Value(i64),
    SubExpr(Box<Expression>, Operator, Box<Expression>),
}

impl Expression {
    fn parse(line: &str) -> Self {
        if let Some(parts) = Self::split_at_operator(line) {
            let left = Self::parse(parts.0);
            let op = Operator::parse(parts.1);
            let right = Self::parse(parts.2);

            return Self::SubExpr(Box::new(left), op, Box::new(right));
        }

        // If the expression is within parentheses, remove them and parse the
        // contents.
        if line.starts_with('(') && line.ends_with(')') {
            return Self::parse(&line[1..(line.len() - 1)]);
        }

        Self::Value(line.trim().parse().unwrap())
    }

    fn split_at_operator(line: &str) -> Option<(&str, &str, &str)> {
        let mut depth = 0;

        for (i, c) in line.char_indices().rev() {
            match c {
                '(' => {
                    depth -= 1;
                }
                ')' => {
                    depth += 1;
                }
                '+' | '-' | '*' | '/' => {
                    if depth == 0 {
                        if i == 0 {
                            panic!("cannot parse {}", i);
                        }
                        return Some((&line[0..(i - 1)], &line[i..(i + 1)], &line[(i + 2)..]));
                    }
                }
                _ => {}
            };
        }

        None
    }

    fn evaluate(&self) -> i64 {
        match self {
            &Self::Value(val) => val,
            Self::SubExpr(left, op, right) => match op {
                Operator::Add => left.evaluate() + right.evaluate(),
                Operator::Subtract => left.evaluate() - right.evaluate(),
                Operator::Multiply => left.evaluate() * right.evaluate(),
                Operator::Divide => left.evaluate() / right.evaluate(),
            },
        }
    }
}

#[derive(Debug)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator {
    fn parse(raw_op: &str) -> Self {
        let op = raw_op.trim();
        if op.len() != 1 {
            panic!("cannot parse {} as an operator", op);
        }

        match op.chars().next().unwrap() {
            '+' => Self::Add,
            '-' => Self::Subtract,
            '*' => Self::Multiply,
            '/' => Self::Divide,
            _ => panic!("cannot parse {} as an operator", op),
        }
    }
}

fn load_input() -> Vec<Expression> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("please specify input filename");
    }

    let contents = fs::read_to_string(&args[1]).expect("could not read input file");

    contents
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| Expression::parse(line))
        .collect()
}

fn sum_expressions(expressions: &[Expression]) -> i64 {
    expressions.iter().map(|expr| expr.evaluate()).sum()
}
