pub mod attribute {
    use crate::{operator::operator::Operator, method::Method, method_with_parameters::MethodWithParameters, method_with_control_flow::MethodWithControlFlow};
 
    pub enum Attribute {
        Number(f64),
        Operator(Operator),
        Method(Method),
        MethodWithParameters(MethodWithParameters, f64),
        MethodWithControlFlow(MethodWithControlFlow, f64),
    }

}

pub use self::attribute::Attribute;
