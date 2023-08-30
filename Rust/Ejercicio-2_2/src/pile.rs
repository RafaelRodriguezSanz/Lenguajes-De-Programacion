
pub mod pile {
    use crate::operator::operator::{Operator, Operable};

 

    pub struct Pile {
        data: Vec<f64>,
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

        pub fn pop(&mut self)  -> Option<f64> {
            return self.data.pop();
        }
        
        pub fn to_string(&self) -> String {
            format!("{:?}", self.data)
        }

        pub fn operate(&mut self, operator: Operator) {
            match operator {
                Operator::Unary(unary_op) => {
                    let value = self.pop().unwrap();
                    let result = unary_op.operate(value, None);
                    self.push(result);
                }
                Operator::Binary(binary_op) => {
                    let last_operand = self.pop().unwrap();
                    let first_operand = self.pop().unwrap();
                    let result = binary_op.operate(first_operand, Some(last_operand));
                    self.push(result);
                }
            }
        }  
    }
}

pub use self::pile::Pile;
