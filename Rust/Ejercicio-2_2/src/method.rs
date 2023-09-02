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

#[cfg(test)]
mod tests {
    use super::*;
    use method::*;

    #[test]
    fn test_dup_method() {
        let mut pile: Vec<f64> = vec![1.0, 2.0, 3.0];
        let mut memory: Vec<f64> = vec![];
        let dup_method = Method::Zeroary(ZeroaryMethod::DUP);

        let result = dup_method.method(&mut pile, &mut memory);

        assert_eq!(result, 3.0);
        assert_eq!(memory, vec![]);
    }

    #[test]
    fn test_pop_method() {
        let mut pile: Vec<f64> = vec![1.0, 2.0, 3.0];
        let mut memory: Vec<f64> = vec![];
        let pop_method = Method::Zeroary(ZeroaryMethod::POP);

        let result = pop_method.method(&mut pile, &mut memory);

        assert_eq!(result, 3.0);
        assert_eq!(memory, vec![]);
    }

    #[test]
    fn test_method_to_string() {
        let dup_method = Method::Zeroary(ZeroaryMethod::DUP);
        let pop_method = Method::Zeroary(ZeroaryMethod::POP);

        assert_eq!(dup_method.to_string(), "DUP".to_string());
        assert_eq!(pop_method.to_string(), "POP".to_string());
    }

    #[test]
    fn test_method_values() {
        let values = Method::values();
        assert_eq!(values, vec!["DUP".to_string(), "POP".to_string()]);
    }

    #[test]
    fn test_pop_method_with_memory() {
        let mut pile: Vec<f64> = vec![1.0, 2.0, 3.0];
        let mut memory: Vec<f64> = vec![0.0, 10.0, 20.0];
        let pop_method = Method::Zeroary(ZeroaryMethod::POP);

        let result = pop_method.method(&mut pile, &mut memory);

        assert_eq!(result, 3.0);
        assert_eq!(pile, vec![1.0, 2.0]);
        assert_eq!(memory, vec![0.0, 10.0, 20.0]);
    }

    #[test]
    fn test_pop_method_with_memory_and_duplicated_value() {
        let mut pile: Vec<f64> = vec![3.0, 3.0, 3.0];
        let mut memory: Vec<f64> = vec![0.0, 10.0, 20.0];
        let pop_method = Method::Zeroary(ZeroaryMethod::POP);

        let result = pop_method.method(&mut pile, &mut memory);

        assert_eq!(result, 3.0); 
        assert_eq!(pile, vec![3.0, 3.0]);
        assert_eq!(memory, vec![0.0, 10.0, 20.0]);
    }
}
