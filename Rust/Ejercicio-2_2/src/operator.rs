pub mod operator { 

    #[derive(PartialEq)]
    pub enum Operator {
        ADD,
        SUB,
        MULT
    }

    impl Operator {
        pub fn to_string(&self) -> &'static str {
            match self {
                Operator::ADD => "ADD",
                Operator::SUB => "SUB",
                Operator::MULT => "MULT",
            }
        }

        pub fn values() -> Vec<Operator> {
            vec![Operator::ADD, Operator::SUB, Operator::MULT]
        }
        
        pub fn operate(&self, x: f64, y: f64) -> f64 {
            match self {
                Operator::ADD => x + y,
                Operator::SUB => x - y,
                Operator::MULT => x * y,
            }
        }
    }
}

pub use self::operator::Operator::*;