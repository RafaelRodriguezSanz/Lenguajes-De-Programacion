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
