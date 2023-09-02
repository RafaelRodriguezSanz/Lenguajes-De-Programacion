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
                BinaryOperator::LTE => write!(f, "LTE"),
                BinaryOperator::LT => write!(f, "LT"),
                BinaryOperator::GTE => write!(f, "GTE"),
                BinaryOperator::GT => write!(f, "GT"),
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
                BinaryOperator::LTE.to_string(),
                BinaryOperator::LT.to_string(),
                BinaryOperator::GTE.to_string(),
                BinaryOperator::GT.to_string(),
                BinaryOperator::AND.to_string(),
                BinaryOperator::OR.to_string(),
            ];
        }
    }

}

pub use operator::{Operator, UnaryOperator, BinaryOperator};

#[cfg(test)]
mod operator_tests {
    use super::*;
    use operator::*;

    #[test]
    fn test_zeroary_operator() {
        let pi_operator = Operator::Zeroary(ZeroaryOperator::PI);
        assert_eq!(pi_operator.operate(None, None), std::f64::consts::PI);
    }

    #[test]
    fn test_unary_operator_cos() {
        let cos_operator = Operator::Unary(UnaryOperator::COS);
        assert_eq!(cos_operator.operate(Some(0.0), None), 1.0);
        assert_eq!(cos_operator.operate(Some(std::f64::consts::PI), None), -1.0);
    }

    #[test]
    fn test_unary_operator_not() {
        let not_operator = Operator::Unary(UnaryOperator::NOT);
        assert_eq!(not_operator.operate(Some(0.0), None), 1.0);
        assert_eq!(not_operator.operate(Some(1.0), None), 0.0);
    }

    #[test]
    fn test_binary_operator_add() {
        let add_operator = Operator::Binary(BinaryOperator::ADD);
        assert_eq!(add_operator.operate(Some(2.0), Some(3.0)), 5.0);
        assert_eq!(add_operator.operate(None, Some(5.0)), 5.0);
    }

    #[test]
    fn test_binary_operator_sub() {
        let sub_operator = Operator::Binary(BinaryOperator::SUB);
        assert_eq!(sub_operator.operate(Some(7.0), Some(3.0)), 4.0);
        assert_eq!(sub_operator.operate(None, Some(5.0)), -5.0);
    }

    #[test]
    fn test_binary_operator_mult() {
        let mult_operator = Operator::Binary(BinaryOperator::MULT);
        assert_eq!(mult_operator.operate(Some(2.0), Some(3.0)), 6.0);
        assert_eq!(mult_operator.operate(None, Some(5.0)), 0.0);
    }

    #[test]
    fn test_binary_operator_eq() {
        let eq_operator = Operator::Binary(BinaryOperator::EQ);
        assert_eq!(eq_operator.operate(Some(2.0), Some(2.0)), 1.0);
        assert_eq!(eq_operator.operate(Some(2.0), Some(3.0)), 0.0);
    }

    #[test]
    fn test_binary_operator_diff() {
        let diff_operator = Operator::Binary(BinaryOperator::DIFF);
        assert_eq!(diff_operator.operate(Some(2.0), Some(2.0)), 0.0);
        assert_eq!(diff_operator.operate(Some(2.0), Some(3.0)), 1.0);
    }

    #[test]
    fn test_binary_operator_lt() {
        let lt_operator = Operator::Binary(BinaryOperator::LT);
        assert_eq!(lt_operator.operate(Some(2.0), Some(3.0)), 1.0);
        assert_eq!(lt_operator.operate(Some(3.0), Some(2.0)), 0.0);
    }

    #[test]
    fn test_binary_operator_lte() {
        let lte_operator = Operator::Binary(BinaryOperator::LTE);
        assert_eq!(lte_operator.operate(Some(2.0), Some(3.0)), 1.0);
        assert_eq!(lte_operator.operate(Some(3.0), Some(2.0)), 0.0);
        assert_eq!(lte_operator.operate(Some(2.0), Some(2.0)), 1.0);
    }

    #[test]
    fn test_binary_operator_gt() {
        let gt_operator = Operator::Binary(BinaryOperator::GT);
        assert_eq!(gt_operator.operate(Some(3.0), Some(2.0)), 1.0);
        assert_eq!(gt_operator.operate(Some(2.0), Some(3.0)), 0.0);
    }

    #[test]
    fn test_binary_operator_gte() {
        let gte_operator = Operator::Binary(BinaryOperator::GTE);
        assert_eq!(gte_operator.operate(Some(3.0), Some(2.0)), 1.0);
        assert_eq!(gte_operator.operate(Some(2.0), Some(3.0)), 0.0);
        assert_eq!(gte_operator.operate(Some(2.0), Some(2.0)), 1.0);
    }

    #[test]
    fn test_binary_operator_and() {
        let and_operator = Operator::Binary(BinaryOperator::AND);
        assert_eq!(and_operator.operate(Some(1.0), Some(1.0)), 1.0);
        assert_eq!(and_operator.operate(Some(1.0), Some(0.0)), 0.0);
        assert_eq!(and_operator.operate(Some(0.0), Some(1.0)), 0.0);
        assert_eq!(and_operator.operate(Some(0.0), Some(0.0)), 0.0);
    }

    #[test]
    fn test_binary_operator_or() {
        let or_operator = Operator::Binary(BinaryOperator::OR);
        assert_eq!(or_operator.operate(Some(1.0), Some(1.0)), 1.0);
        assert_eq!(or_operator.operate(Some(1.0), Some(0.0)), 1.0);
        assert_eq!(or_operator.operate(Some(0.0), Some(1.0)), 1.0);
        assert_eq!(or_operator.operate(Some(0.0), Some(0.0)), 0.0);
    }

    #[test]
    fn test_operator_to_string() {
        let pi_operator = Operator::Zeroary(ZeroaryOperator::PI);
        let not_operator = Operator::Unary(UnaryOperator::NOT);
        let add_operator = Operator::Binary(BinaryOperator::ADD);

        assert_eq!(pi_operator.to_string(), "PI".to_string());
        assert_eq!(not_operator.to_string(), "NOT".to_string());
        assert_eq!(add_operator.to_string(), "ADD".to_string());
    }

    #[test]
    fn test_operator_values() {
        let values = Operator::values();
        assert_eq!(
            values,
            vec![
                "COS".to_string(),
                "NOT".to_string(),
                "PI".to_string(),
                "ADD".to_string(),
                "SUB".to_string(),
                "MULT".to_string(),
                "EQ".to_string(),
                "DIFF".to_string(),
                "LTE".to_string(),
                "LT".to_string(),
                "GTE".to_string(),
                "GT".to_string(),
                "AND".to_string(),
                "OR".to_string()
            ]
        );
    }

}
