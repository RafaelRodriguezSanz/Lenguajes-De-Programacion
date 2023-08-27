pub mod parser{

    use crate::{operator::operator::Operator, attribute::Attribute};

    pub fn to_double(input: &str)-> Option<f64> {
        match input.parse::<f64>() {
            Ok(number) => Some(number),
            Err(_err) => None
        }
    }

    pub fn to_operator(input: &str) -> Option<Operator> {
        match input {
            s if s == Operator::ADD.to_string() => Some(Operator::ADD),
            s if s == Operator::SUB.to_string() => Some(Operator::SUB),
            s if s == Operator::MULT.to_string() => Some(Operator::MULT),
            _ => None
        }
    }

    pub fn parse_input(input: &str) -> Option<Attribute> {
        if let Some(number) = to_double(input) {
            Some(Attribute::Number(number))
        } else if let Some(op) = to_operator(input) {
            Some(Attribute::Operator(op))
        } else {
            None
        }
    }
    

}

pub use self::parser::{parse_input, to_double, to_operator};