pub mod parser{

    
    use crate::{operator::{Operator, UnaryOperator, BinaryOperator, operator::ZeroaryOperator}, attribute::Attribute, {method::{ZeroaryMethod}}, method::Method, method_with_parameters::{UnaryMethodWithParameters, MethodWithParameters, method_with_parameters::ZeroaryMethodWithParameters}, method_with_control_flow::{MethodWithControlFlow, ZeroaryMethodWithControlFlow, UnaryMethodWithControlFlow}};

    pub fn to_double(input: &str)-> Option<f64> {
        if input.contains("+") {
            match input.replace("+", "").parse::<f64>() {
                Ok(number) => Some(number),
                Err(_err) => None
            }
        } else {
            if input.contains("-"){
                match input.replace("-", "").parse::<f64>() {
                    Ok(number) => Some(number * -1.0),
                    Err(_err) => None
                }
            } else {
                match input.parse::<f64>() {
                    Ok(number) => Some(number),
                    Err(_err) => None
        
                }
            }
        }    
    }

    pub fn to_operator(input: &str) -> Option<Operator> {
        match input {
            s if s == ZeroaryOperator::PI.to_string() => Some(Operator::Zeroary(ZeroaryOperator::PI)),
            s if s == UnaryOperator::NOT.to_string() => Some(Operator::Unary(UnaryOperator::NOT)),
            s if s == UnaryOperator::COS.to_string() => Some(Operator::Unary(UnaryOperator::COS)),
            s if s == BinaryOperator::ADD.to_string() => Some(Operator::Binary(BinaryOperator::ADD)),
            s if s == BinaryOperator::SUB.to_string() => Some(Operator::Binary(BinaryOperator::SUB)),
            s if s == BinaryOperator::MULT.to_string() => Some(Operator::Binary(BinaryOperator::MULT)),
            s if s == BinaryOperator::EQ.to_string() => Some(Operator::Binary(BinaryOperator::EQ)),
            s if s == BinaryOperator::DIFF.to_string() => Some(Operator::Binary(BinaryOperator::DIFF)),
            s if s == BinaryOperator::LT.to_string() => Some(Operator::Binary(BinaryOperator::LT)),
            s if s == BinaryOperator::LTE.to_string() => Some(Operator::Binary(BinaryOperator::LTE)),
            s if s == BinaryOperator::GT.to_string() => Some(Operator::Binary(BinaryOperator::GT)),
            s if s == BinaryOperator::GTE.to_string() => Some(Operator::Binary(BinaryOperator::GTE)),
            s if s == BinaryOperator::AND.to_string() => Some(Operator::Binary(BinaryOperator::AND)),
            s if s == BinaryOperator::OR.to_string() => Some(Operator::Binary(BinaryOperator::OR)),
            _ => None
        }
    }

    pub fn to_method(input: &str) -> Option<Method> {
        match input {
            s if s == ZeroaryMethod::DUP.to_string() => Some(Method::Zeroary(ZeroaryMethod::DUP)),
            s if s == ZeroaryMethod::POP.to_string() => Some(Method::Zeroary(ZeroaryMethod::POP)),
            _ => None
        }
    }

    pub fn to_method_with_parameters(input: &str, value: f64) -> Option<MethodWithParameters> {
        match input {
            s if s == ZeroaryMethodWithParameters::GET.to_string() => {
                Some(MethodWithParameters::Zeroary(ZeroaryMethodWithParameters::GET, value))
            }
            s if s == UnaryMethodWithParameters::SET.to_string() => {
                Some(MethodWithParameters::Unary(UnaryMethodWithParameters::SET, value))
            }
            _ => None,
        }
    }    

    pub fn to_method_with_control_flow(input: &str, value: f64) -> Option<MethodWithControlFlow> {
        match input {
            s if s == ZeroaryMethodWithControlFlow::UJP.to_string() => {
                Some(MethodWithControlFlow::Zeroary(ZeroaryMethodWithControlFlow::UJP, value))
            }
            s if s == UnaryMethodWithControlFlow::CJP.to_string() => {
                Some(MethodWithControlFlow::Unary(UnaryMethodWithControlFlow::CJP, value))
            }
            _ => None,
        }
    }    

    pub fn parse_input(input: &str) -> Option<Attribute> {
        if let Some(number) = to_double(input) {
            Some(Attribute::Number(number))
        } else if Operator::values().contains(&input.to_string()) {
            if let Some(mth) = to_operator(input) {
                Some(Attribute::Operator(mth))
            } else {
                None
            }
        } else if Method::values().contains(&input.to_string()) {
            if let Some(mth) = to_method(input) {
                Some(Attribute::Method(mth))
            } else {
                None
            }
        } else if MethodWithParameters::values().contains(&input.split(":").next().unwrap().to_string()) {
            if let Some(mth) = to_method_with_parameters(input.split(":").collect::<Vec<_>>()[0], to_double(input.split(":").collect::<Vec<_>>()[1]).unwrap()) {
                Some(Attribute::MethodWithParameters(mth, to_double(input.split(":").collect::<Vec<_>>()[1]).unwrap()))
            }
            else {
                None
            }
        } else if MethodWithControlFlow::values().contains(&input.split(":").next().unwrap().to_string()) {
                if let Some(mth) = to_method_with_control_flow(input.split(":").collect::<Vec<_>>()[0], to_double(input.split(":").collect::<Vec<_>>()[1]).unwrap()) {
                    Some(Attribute::MethodWithControlFlow(mth, to_double(input.split(":").collect::<Vec<_>>()[1]).unwrap()))
                }
                else {
                    None
                }
        } else {
            None
        }
    }
    

}

pub use self::parser::{parse_input, to_double, to_operator};

#[cfg(test)]
mod parser_tests {
    
    use super::*;

    #[test]
    fn test_to_double() {
        assert_eq!(to_double("123.45"), Some(123.45));
        assert_eq!(to_double("abc"), None);
    }

}
