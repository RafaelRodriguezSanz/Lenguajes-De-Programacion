use std::fmt;
use std::f64::consts::PI;
use std::f64;

pub mod operator {
    pub enum ZeroaryOperator {
        PI
    }

    pub enum UnaryOperator {
        NOT,
        COS
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

    impl std::fmt::Display for UnaryOperator {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                UnaryOperator::NOT => write!(f, "NOT"),
                UnaryOperator::COS => write!(f, "COS")
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

    impl std::fmt::Display for ZeroaryOperator {
       fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
           match self {
            ZeroaryOperator::PI => write!(f, "PI")
           }
       }
    }

    pub trait Operable {
        fn operate(&self, x: Option<f64>, y: Option<f64>) -> f64;
    }

    impl Operable for ZeroaryOperator {
        fn operate(&self, _first_value: Option<f64>, _second_value: Option<f64>) -> f64 {
            match self {
                ZeroaryOperator::PI => std::f64::consts::PI
            }
        }
    }

    impl Operable for UnaryOperator {
        fn operate(&self, value: Option<f64>, _second_value: Option<f64>) -> f64 {
            match self {
                UnaryOperator::NOT => if value.unwrap() == 0.0 { 1.0 } else { 0.0 },
                UnaryOperator::COS => f64::cos(value.unwrap())
            }
        }
    }

    impl Operable for BinaryOperator {
        fn operate(&self, x: Option<f64>, y: Option<f64>) -> f64 {
            match self {
                BinaryOperator::ADD => x.unwrap_or(0.0) + y.unwrap_or(0.0),
                BinaryOperator::SUB => x.unwrap_or(0.0) - y.unwrap_or(0.0),
                BinaryOperator::MULT => x.unwrap_or(0.0) * y.unwrap_or(1.0),
                BinaryOperator::EQ => if x.unwrap_or(0.0) == y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::DIFF => if x.unwrap_or(0.0) != y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::LT => if x.unwrap_or(0.0) < y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::LTE => if x.unwrap_or(0.0) <= y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::GT => if x.unwrap_or(0.0) > y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::GTE => if x.unwrap_or(0.0) >= y.unwrap_or(0.0) { 1.0 } else { 0.0 },
                BinaryOperator::AND => if x.unwrap_or(0.0) != 0.0 && y.unwrap_or(0.0) != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::OR => if x.unwrap_or(0.0) != 0.0 || y.unwrap_or(0.0) != 0.0 { 1.0 } else { 0.0 }
            }
        }
    }

    pub enum Operator {
        Zeroary(ZeroaryOperator),
        Unary(UnaryOperator),
        Binary(BinaryOperator),
    }

    impl Operator {
        pub fn operate(&self, x: Option<f64>, y: Option<f64>) -> f64 {
            match self {
                Operator::Zeroary(op) => op.operate(x, y),
                Operator::Unary(op) => op.operate(x, y),
                Operator::Binary(op) => op.operate(x, y),
            }
        }

        pub fn to_string(&self) -> String {
            match self {
                Operator::Zeroary(op) => op.to_string(),
                Operator::Unary(op) => op.to_string(),
                Operator::Binary(op) => op.to_string(),
            }
        }        

        pub fn values() -> Vec<String> {
            return vec![
                UnaryOperator::COS.to_string(),
                UnaryOperator::NOT.to_string(),
                ZeroaryOperator::PI.to_string(),
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
        }
    }

}

pub use operator::{Operator, UnaryOperator, BinaryOperator};
