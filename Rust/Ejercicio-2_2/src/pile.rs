
pub mod pile {
    use crate::operator::operator::Operator;
 

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

        pub fn operate(&mut self, operator:Operator) {
            let last_operand = self.pop().unwrap();
            let first_operand = self.pop().unwrap();
            self.push(operator.operate(first_operand, last_operand));
        }    
    }
}

pub use self::pile::Pile;
