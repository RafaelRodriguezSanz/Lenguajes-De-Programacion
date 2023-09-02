use std::fmt;

pub mod method {
    use crate::variables::Variables;


    pub enum ZeroaryMethod {
        DUP,
        POP
    }

    impl std::fmt::Display for ZeroaryMethod {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                ZeroaryMethod::DUP => write!(f, "DUP"),
                ZeroaryMethod::POP => write!(f, "POP"),
            }
        }
    }
    
    pub trait Methodable {
        fn method(&self, pile: &mut Vec<f64>, memory:  &mut Vec<f64>) -> f64;
    }

    impl Methodable for ZeroaryMethod {
        fn method(&self, pile: &mut Vec<f64>, mut _memory:  &mut Vec<f64>) -> f64 {
            match self {
                ZeroaryMethod::DUP => {
                    return pile.last().unwrap().clone();
                }
                ZeroaryMethod::POP => {
                    return pile.pop().unwrap();
                }
            }
        }
    }

    pub enum Method {
        Zeroary(ZeroaryMethod)
    }

    impl Method {
        pub fn method(&self, pile: &mut Vec<f64>, mut memory:  &mut Vec<f64>) -> f64 {
            match self {
                Method::Zeroary(mth) => mth.method(pile, memory)
            }
        }

        pub fn to_string(&self) -> String {
            match self {
                Method::Zeroary(mth) => mth.to_string()
            }
        }

        pub fn values() -> Vec<String> {
            return vec![
                ZeroaryMethod::DUP.to_string(),
                ZeroaryMethod::POP.to_string(),
            ];
        }
    }

}

pub use method::{Method, ZeroaryMethod};
