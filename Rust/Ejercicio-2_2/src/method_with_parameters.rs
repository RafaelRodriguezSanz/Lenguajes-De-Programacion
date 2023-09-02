use std::fmt;

pub mod method_with_parameters {

    pub enum UnaryMethodWithParameters {
        SET
    }

    pub enum ZeroaryMethodWithParameters {
        GET
    }
    
    impl std::fmt::Display for UnaryMethodWithParameters {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                UnaryMethodWithParameters::SET => write!(f, "SET"),
            }
        }
    }

    impl std::fmt::Display for ZeroaryMethodWithParameters {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                ZeroaryMethodWithParameters::GET => write!(f, "GET")
            }
        }
    }

    pub trait Methodable {
        fn methodWithParameters(&self, pile: &mut Vec<f64>, memory: &mut Vec<f64>, index:f64) -> f64;
    }

    impl Methodable for UnaryMethodWithParameters {
        fn methodWithParameters(&self, pile: &mut Vec<f64>, memory: &mut Vec<f64>, index:f64) -> f64 {
            match self {
                UnaryMethodWithParameters::SET => {
                    if let Some(value) = pile.pop() {
                        memory.insert(index as usize, value);
                        memory.remove((index + 1.0) as usize);
                        return value;
                    } else {
                        panic!("No value to add")
                    }
                }
            }
        }
    }

    impl Methodable for ZeroaryMethodWithParameters {
        fn methodWithParameters(&self, pile: &mut Vec<f64>, memory: &mut Vec<f64>, index: f64) -> f64 {
            match self {
                ZeroaryMethodWithParameters::GET => {
                    let value = *memory.get(index as usize).unwrap();
                    pile.push(value);
                    return value;
                }
            }
        }
    }

    pub enum MethodWithParameters {
        Unary(UnaryMethodWithParameters, f64),
        Zeroary(ZeroaryMethodWithParameters, f64)
    }

    impl MethodWithParameters {
        pub fn methodWithParameters(&self, pile: &mut Vec<f64>, memory: &mut Vec<f64>, parameter:f64) -> f64 {
            match self {
                MethodWithParameters::Unary(mth, _) => mth.methodWithParameters(pile, memory, parameter),
                MethodWithParameters::Zeroary(mth , _) => mth.methodWithParameters(pile, memory, parameter)
            }
        }

        pub fn to_string(&self) -> String {
            match self {
                MethodWithParameters::Unary(mth, _) => mth.to_string(),
                MethodWithParameters::Zeroary(mth, _) => mth.to_string()
            }
        }

        pub fn values() -> Vec<String> {
            return vec![
                ZeroaryMethodWithParameters::GET.to_string(),
                UnaryMethodWithParameters::SET.to_string()
            ];
        }
    }

}

pub use method_with_parameters::{MethodWithParameters, UnaryMethodWithParameters};
