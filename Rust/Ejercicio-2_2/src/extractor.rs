pub mod extractor {
    use regex::Regex;
    use crate::{operator::operator::Operator, method::Method, method_with_parameters::MethodWithParameters, method_with_control_flow::{UnaryMethodWithControlFlow, MethodWithControlFlow}};

    pub fn extract(line: &str)-> Vec<String> {
        if line.len() < usize::MAX {
            let operator_strings: Vec<String> = Operator::values()
            .iter()
            .map(|op| op.to_string().to_string())
            .collect();

            let method_strings: Vec<String> = Method::values()
            .iter()
            .map(|op| op.to_string().to_string())
            .collect();

            let method_strings_with_parameters: Vec<String> = MethodWithParameters::values()
            .iter()
            .map(|op| op.to_string().to_string())
            .collect();

            let method_strings_with_control_flow: Vec<String> = MethodWithControlFlow::values()
            .iter()
            .map(|op| op.to_string().to_string())
            .collect();

            let regex_pattern = format!(r"{}|{}|{}|{}\:[-+]?\d+|\d+(\.\d+)?|\d+",  // Agregar [-+]?
            operator_strings.join(r"|"), method_strings.join(r"|"),
            method_strings_with_parameters.join(r"\:\d+|"), 
            method_strings_with_control_flow.join(r"\:[-+]?\d+|"));
            
            let regex = Regex::new(&regex_pattern).unwrap();

            regex.find_iter(line)
            .map(|capture| capture.as_str().to_string())
            .collect()
        } else {
            panic!("Too much big input")
        }
    }

}

pub use self::extractor::extract;

#[cfg(test)]
mod unit_tests {
    use crate::{operator::Operator, method::Method, method_with_parameters::MethodWithParameters};
    use super::*;
    use super::*;

    #[test]
    fn operators() {
        let expected:Vec<String> = Operator::values()
        .iter()
        .map(|op| op.to_string().to_string())
        .collect();

        assert_eq!(expected, extract(&expected.join(" ").to_string()));
    }

    #[test]
    fn methods() {
        let expected: Vec<String> = Method::values()
            .iter()
            .map(|method| method.to_string())
            .collect();

        assert_eq!(expected, extract(&expected.join(" ").to_string()));
    }

    #[test]
    fn methods_with_parameters() {
        let expected: Vec<String> = MethodWithParameters::values()
        .iter()
        .map(|method| format!("{}:1", method.to_string()))
        .collect();

        assert_eq!(expected, extract(&expected.join(" ").to_string()));
    }

    #[test]
    fn mixed_operations() {
        let input = "1 ADD 2 SUB SET:3 GET:3 LT 4 LTE AND OR 5.0";
        let expected: Vec<String> = vec![
            "1".to_string(),
            "ADD".to_string(),
            "2".to_string(),
            "SUB".to_string(),
            "SET:3".to_string(),
            "GET:3".to_string(),
            "LT".to_string(),
            "4".to_string(),
            "LTE".to_string(),
            "AND".to_string(),
            "OR".to_string(),
            "5.0".to_string(),
        ];

        assert_eq!(expected, extract(input));
    }

    #[test]
    fn empty_input() {
        assert_eq!(Vec::<String>::new(), extract(""));
    }

    #[test]
    fn single_number() {
        assert_eq!(vec!["42".to_string()], extract("42"));
    }

    #[test]
    fn invalid_input() {
        assert_eq!(Vec::<String>::new(), extract("INVALID_INPUT"));
    }

    #[test]
    fn decimal_numbers() {
        let input = "3.14 ADD 2.5 SUB";
        assert_eq!(vec!["3.14".to_string(), "ADD".to_string(), "2.5".to_string(), "SUB".to_string()], extract(input));
    }

    #[test]
    fn complex_expression() {
        let input = "1 ADD 2 MULT SET:3 GET:3 LT 4 LTE AND OR NOT";
        assert_eq!(vec!["1".to_string(), "ADD".to_string(), "2".to_string(), "MULT".to_string(), "SET:3".to_string(), "GET:3".to_string(), "LT".to_string(), "4".to_string(), "LTE".to_string(), "AND".to_string(), "OR".to_string(), "NOT".to_string()], extract(input));
    }

    #[test]
    fn repeated_operators() {
        let input = "1 ADD ADD 2 SUB MULT";
        assert_eq!(vec!["1".to_string(), "ADD".to_string(), "ADD".to_string(), "2".to_string(), "SUB".to_string(), "MULT".to_string()], extract(input));
    }

    #[test]
    fn long_decimal_number() {
        let input = "9".repeat(1000) + ".1234";
        assert_eq!(vec![input.clone()], extract(&input));
    }

    #[test]
    fn single_character_input() {
        let input = "A";
        let result = extract(input);
        assert_eq!(vec![] as Vec<String>, result);
    }

    #[test]
    fn long_input() {
        let input = "A".repeat(10000);
        let result = extract(&input);
        assert_eq!(vec![] as Vec<String>, result);
    }

    #[test]
    fn special_characters_input() {
        let input = "COS(x) + π = 3.14";
        let result = extract(input);
        assert_eq!(vec!["COS".to_string(), "3.14".to_string()], result);
    }

    #[test]
    fn mixed_input() {
        let input = "1 + 2.5 * SET:3 GET:3 LT 4 LTE";
        let result = extract(input);
        assert_eq!(vec!["1".to_string(),"2.5".to_string(), "SET:3".to_string(), "GET:3".to_string(), "LT".to_string(), "4".to_string(), "LTE".to_string()], result);
    }

}




