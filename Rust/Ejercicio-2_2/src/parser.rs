pub mod parser{

    
    use crate::{operator::{Operator, UnaryOperator, BinaryOperator}, attribute::Attribute};

    pub fn to_double(input: &str)-> Option<f64> {
        match input.parse::<f64>() {
            Ok(number) => Some(number),
            Err(_err) => None
        }
    }

    pub fn to_operator(input: &str) -> Option<Operator> {
        match input {
            s if s == UnaryOperator::NOT.to_string() => Some(Operator::Unary(UnaryOperator::NOT)),
            s if s == BinaryOperator::ADD.to_string() => Some(Operator::Binary(BinaryOperator::ADD)),
            s if s == BinaryOperator::SUB.to_string() => Some(Operator::Binary(BinaryOperator::SUB)),
            s if s == BinaryOperator::MULT.to_string() => Some(Operator::Binary(BinaryOperator::MULT)),
            s if s == BinaryOperator::EQ.to_string() => Some(Operator::Binary(BinaryOperator::EQ)),
            s if s == BinaryOperator::DIFF.to_string() => Some(Operator::Binary(BinaryOperator::DIFF)),
            s if s == BinaryOperator::LT.to_string() => Some(Operator::Binary(BinaryOperator::LT)),
            s if s == BinaryOperator::LTE.to_string() => Some(Operator::Binary(BinaryOperator::LTE)),
            s if s == BinaryOperator::GT.to_string() => Some(Operator::Binary(BinaryOperator::GT)),
            s if s == BinaryOperator::GTE.to_string() => Some(Operator::Binary(BinaryOperator::GTE)),
            s if s == BinaryOperator::AND.to_string() => Some(Operator::Binary(BinaryOperator::AND)),
            s if s == BinaryOperator::OR.to_string() => Some(Operator::Binary(BinaryOperator::OR)),
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