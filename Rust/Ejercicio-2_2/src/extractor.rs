pub mod extractor {
    use regex::Regex;
    use crate::{operator::operator::Operator, method::Method, method_with_parameters::MethodWithParameters};

    pub fn extract(line: &str)-> Vec<String> {
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

        let regex_pattern = format!(r"{}|{}|{}:\d+|\d+\.\d+|\d+", operator_strings.join(r"|"), method_strings.join(r"|"), method_strings_with_parameters.join(r"\:\d+|"));
        let regex = Regex::new(&regex_pattern).unwrap();

        regex.captures_iter(line)
            .map(|capture| capture[0].to_string())
            .collect()
    }

}

pub use self::extractor::extract;






