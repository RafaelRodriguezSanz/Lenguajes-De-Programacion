pub mod pile {

    use crate::{
        method::{Method, ZeroaryMethod},
        method_with_parameters::{MethodWithParameters, UnaryMethodWithParameters, method_with_parameters::ZeroaryMethodWithParameters},
        operator::operator::{Operable, Operator},
    };

    pub struct Pile {
        pub data: Vec<f64>,
    }

    impl Pile {
        pub fn new() -> Pile {
            Pile {
                data: Vec::<f64>::new(),
            }
        }

        pub fn push(&mut self, valor: f64) {
            self.data.push(valor);
        }

        pub fn pop(&mut self) -> Option<f64> {
            return self.data.pop();
        }

        pub fn to_string(&self) -> String {
            format!("{:?}", self.data)
        }

        pub fn operate(&mut self, operator: Operator) {
            match operator {
                Operator::Zeroary(zeroary_op) => {
                    let result = zeroary_op.operate(None, None);
                    self.push(result);
                }
                Operator::Unary(unary_op) => {
                    let value = self.pop().unwrap();
                    let result = unary_op.operate(Some(value), None);
                    self.push(result);
                }
                Operator::Binary(binary_op) => {
                    let last_operand = self.pop().unwrap();
                    let first_operand = self.pop().unwrap();
                    let result = binary_op.operate(Some(first_operand), Some(last_operand));
                    self.push(result);
                }
            }
        }

        pub fn method(&mut self, method: Method, variables:  &mut Vec<f64>) {
            match method {
                Method::Zeroary(zeroary_mth) => match zeroary_mth {
                    ZeroaryMethod::DUP => {
                        let result = crate::method::method::Methodable::method(
                            &zeroary_mth,
                            &mut self.data,
                            variables,
                        );
                        self.push(result);
                    }
                    ZeroaryMethod::POP => {
                        crate::method::method::Methodable::method(
                            &zeroary_mth,
                            &mut self.data,
                            variables,
                        );
                    }
                }
            }
        }

        pub fn method_with_parameters(&mut self, method: MethodWithParameters, variables:  &mut Vec<f64>, parameter:f64) {
            match method {
                MethodWithParameters::Unary(unary_mth, parameter) => match unary_mth {
                    UnaryMethodWithParameters::SET => {
                        crate::method_with_parameters::method_with_parameters::Methodable::methodWithParameters(
                            &unary_mth,
                            &mut self.data,
                            variables,
                            parameter
                        );
                    }
                },
                MethodWithParameters::Zeroary(unary_mth, parameter) => match unary_mth {
                    ZeroaryMethodWithParameters::GET => {
                        crate::method_with_parameters::method_with_parameters::Methodable::methodWithParameters(
                            &unary_mth,
                            &mut self.data,
                            variables,
                            parameter
                        );
                    }
                },
            }
        }
    }
}

pub use self::pile::Pile;

#[cfg(test)]
mod pile_tests {
    
    use super::*;
    use crate::operator::*;
    use crate::method_with_parameters::method_with_parameters::*;
    use crate::method::*;
    use crate::operator::operator::*;

    #[test]
    fn test_push_and_pop() {
        let mut pile = Pile::new();
        pile.push(1.0);
        pile.push(2.0);

        assert_eq!(pile.pop(), Some(2.0));
        assert_eq!(pile.pop(), Some(1.0));
        assert_eq!(pile.pop(), None);
    }

    #[test]
    fn test_operate_with_zeroary_operator() {
        let mut pile = Pile::new();
        pile.operate(Operator::Zeroary(ZeroaryOperator::PI));

        assert_eq!(pile.pop(), Some(std::f64::consts::PI));
    }

    #[test]
    fn test_operate_with_unary_operator() {
        let mut pile = Pile::new();
        pile.push(1.0);
        pile.operate(Operator::Unary(UnaryOperator::COS));

        assert_eq!(pile.pop(), Some(f64::cos(1.0)));
    }

    #[test]
    fn test_operate_with_binary_operator() {
        let mut pile = Pile::new();
        pile.push(2.0);
        pile.push(3.0);
        pile.operate(Operator::Binary(BinaryOperator::ADD));

        assert_eq!(pile.pop(), Some(5.0));
    }

    #[test]
    fn test_method_zeroary() {
        let mut pile = Pile::new();
        pile.push(1.3);
        let mut variables = Vec::new();
        pile.method(Method::Zeroary(ZeroaryMethod::DUP), &mut variables);

        assert_eq!(pile.pop(), Some(1.3));
    }

}
