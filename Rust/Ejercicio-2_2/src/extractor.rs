pub mod extractor {
    use regex::Regex;
    use crate::operator::operator::Operator;

    pub fn extract(line: &str)-> Vec<String> {
        let operator_strings: Vec<String> = Operator::values()
        .iter()
        .map(|op| op.to_string().to_string())
        .collect();

        let regex_pattern = format!(r"\d+\.\d+|\d+|{}", operator_strings.join("|"));
        let regex = Regex::new(&regex_pattern).unwrap();

        regex.captures_iter(line)
            .map(|capture| capture[0].to_string())
            .collect()
    }

}

pub use self::extractor::extract;






