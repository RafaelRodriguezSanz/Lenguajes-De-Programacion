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

#[cfg(test)]
mod tests {
    use super::*;
    use method_with_parameters::*;

    #[test]
    fn test_set_method_with_parameters() {
        let mut pile: Vec<f64> = vec![1.0, 2.0, 3.0];
        let mut memory: Vec<f64> = vec![0.0, 0.0, 0.0];
        let index = 1.0;
        let set_method = MethodWithParameters::Unary(UnaryMethodWithParameters::SET, index);

        let result = set_method.methodWithParameters(&mut pile, &mut memory, index);

        assert_eq!(result, 3.0);
        assert_eq!(pile, vec![1.0, 2.0]);
        assert_eq!(memory, vec![0.0, 3.0, 0.0]);
    }

    #[test]
    fn test_get_method_with_parameters() {
        let mut pile: Vec<f64> = vec![1.0, 2.0, 3.0];
        let mut memory: Vec<f64> = vec![0.0, 10.0, 20.0];
        let index = 1.0;
        let get_method = MethodWithParameters::Zeroary(ZeroaryMethodWithParameters::GET, index);

        let result = get_method.methodWithParameters(&mut pile, &mut memory, index);

        assert_eq!(result, 10.0); // El valor obtenido debe ser 10.0
        assert_eq!(pile, vec![1.0, 2.0, 3.0, 10.0]);
        assert_eq!(memory, vec![0.0, 10.0, 20.0]);
    }

    #[test]
    fn test_method_with_parameters_to_string() {
        let set_method = MethodWithParameters::Unary(UnaryMethodWithParameters::SET, 1.0);
        let get_method = MethodWithParameters::Zeroary(ZeroaryMethodWithParameters::GET, 2.0);

        assert_eq!(set_method.to_string(), "SET".to_string());
        assert_eq!(get_method.to_string(), "GET".to_string());
    }

    #[test]
    fn test_method_with_parameters_values() {
        let values = MethodWithParameters::values();
        assert_eq!(values, vec!["GET".to_string(), "SET".to_string()]);
    }

    #[test]
    fn test_set_method_with_parameters_value_inserted() {
        let mut pile: Vec<f64> = vec![7.5];
        let mut memory: Vec<f64> = vec![0.0, 0.0, 0.0];
        let index = 2.0;
        let set_method = MethodWithParameters::Unary(UnaryMethodWithParameters::SET, index);

        let result = set_method.methodWithParameters(&mut pile, &mut memory, index);

        assert_eq!(result, 7.5);
        assert_eq!(pile, vec![]); 
        assert_eq!(memory, vec![0.0, 0.0, 7.5]); 
    }

    #[test]
    fn test_get_method_with_parameters_value_pushed_to_pile() {
        let mut pile: Vec<f64> = vec![];
        let mut memory: Vec<f64> = vec![5.0, 8.0, 12.0];
        let index = 0.0;
        let get_method = MethodWithParameters::Zeroary(ZeroaryMethodWithParameters::GET, index);

        let result = get_method.methodWithParameters(&mut pile, &mut memory, index);

        assert_eq!(result, 5.0);
        assert_eq!(pile, vec![5.0]);
        assert_eq!(memory, vec![5.0, 8.0, 12.0]);
    }
}
