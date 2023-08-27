pub mod attribute {
    use crate::operator::operator::Operator;
 

    pub enum Attribute {
        Number(f64),
        Operator(Operator),
    }

    impl Attribute {
        
    }

}

pub use self::attribute::Attribute;