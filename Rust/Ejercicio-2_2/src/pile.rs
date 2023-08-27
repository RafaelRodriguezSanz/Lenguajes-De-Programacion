
pub mod pile {
    use crate::{parser::parse_input, attribute::Attribute, operator::operator::Operator};
 

    pub struct Pile {
        data: Vec<f64>,
    }

    impl Pile {
        pub fn new() -> Pile {
            Pile {
                data: Vec::<f64>::new(),
            }
        }

        pub fn get_data(&mut self) -> Vec::<f64> {
            self.data.clone()
        }
    
        pub fn push(&mut self, valor: f64) {
            self.data.push(valor);
        }

        pub fn pop(&mut self)  -> Option<f64> {
            return self.data.pop();
        }

        pub fn is_empty(&mut self)  -> bool {
            return self.data.is_empty();
        }
        
        pub fn to_string(&self) -> String {
            format!("{:?}", self.data)
        }

        pub fn operate(&mut self, operator:Operator) {
            let last_operand = self.pop().unwrap();
            let first_operand = self.pop().unwrap();
            self.push(operator.operate(first_operand, last_operand));
        }    
    }
}

pub use self::pile::Pile;
