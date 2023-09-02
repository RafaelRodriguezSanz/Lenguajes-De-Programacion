pub mod attribute {
    use crate::{operator::operator::Operator, method::Method, method_with_parameters::MethodWithParameters};
 

    pub enum Attribute {
        Number(f64),
        Operator(Operator),
        Method(Method),
        MethodWithParameters(MethodWithParameters, f64),
    }

    impl Attribute {
        
    }

}

pub use self::attribute::Attribute;