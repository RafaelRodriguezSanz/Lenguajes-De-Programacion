use std::fmt;

pub mod operator {

    pub enum UnaryOperator {
        NOT
    }

    impl std::fmt::Display for UnaryOperator {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                UnaryOperator::NOT => write!(f, "NOT"),
            }
        }
    }

    impl std::fmt::Display for BinaryOperator {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                BinaryOperator::ADD => write!(f, "ADD"),
                BinaryOperator::SUB => write!(f, "SUB"),
                BinaryOperator::MULT => write!(f, "MULT"),
                BinaryOperator::EQ => write!(f, "EQ"),
                BinaryOperator::DIFF => write!(f, "DIFF"),
                BinaryOperator::LT => write!(f, "LT"),
                BinaryOperator::LTE => write!(f, "LTE"),
                BinaryOperator::GT => write!(f, "GT"),
                BinaryOperator::GTE => write!(f, "GTE"),
                BinaryOperator::AND => write!(f, "AND"),
                BinaryOperator::OR => write!(f, "OR")
            }
        }
    }

    pub enum BinaryOperator {
        ADD,
        SUB,
        MULT,
        EQ,
        DIFF,
        LT,
        LTE,
        GT,
        GTE,
        AND,
        OR
    }

    pub trait Operable {
        fn operate(&self, x: f64, y: Option<f64>) -> f64;
    }

    impl Operable for UnaryOperator {
        fn operate(&self, value: f64, _second_value: Option<f64>) -> f64 {
            match self {
                UnaryOperator::NOT => if value == 0.0 { 1.0 } else { 0.0 }
            }
        }
    }

    impl Operable for BinaryOperator {
        fn operate(&self, x: f64, y: Option<f64>) -> f64 {
            match self {
                BinaryOperator::ADD => x + y.unwrap_or(0.0),
                BinaryOperator::SUB => x - y.unwrap_or(0.0),
                BinaryOperator::MULT => x * y.unwrap_or(1.0),
                BinaryOperator::EQ => if x == y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::DIFF => if x != y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::LT => if x < y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::LTE => if x <= y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::GT => if x > y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::GTE => if x >= y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::AND => if x != 0.0 && y.unwrap_or(0.0) != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::OR => if x != 0.0 || y.unwrap_or(0.0) != 0.0 { 1.0 } else { 0.0 }
            }
        }
    }

    pub enum Operator {
        Unary(UnaryOperator),
        Binary(BinaryOperator),
    }

    impl Operator {
        pub fn operate(&self, x: f64, y: Option<f64>) -> f64 {
            match self {
                Operator::Unary(op) => op.operate(x, y),
                Operator::Binary(op) => op.operate(x, y),
            }
        }

        pub fn to_string(&self) -> String {
            match self {
                Operator::Unary(op) => op.to_string(),
                Operator::Binary(op) => op.to_string(),
            }
        }        

        pub fn values() -> Vec<String> {
            let unary_values: Vec<String> = vec![UnaryOperator::NOT.to_string()];
            let binary_values: Vec<String> = vec![
                BinaryOperator::ADD.to_string(),
                BinaryOperator::SUB.to_string(),
                BinaryOperator::MULT.to_string(),
                BinaryOperator::EQ.to_string(),
                BinaryOperator::DIFF.to_string(),
                BinaryOperator::LT.to_string(),
                BinaryOperator::LTE.to_string(),
                BinaryOperator::GT.to_string(),
                BinaryOperator::GTE.to_string(),
                BinaryOperator::AND.to_string(),
                BinaryOperator::OR.to_string(),
            ];
        
            unary_values.into_iter().chain(binary_values).collect()
        }
    }

}

pub use operator::{Operator, UnaryOperator, BinaryOperator};
