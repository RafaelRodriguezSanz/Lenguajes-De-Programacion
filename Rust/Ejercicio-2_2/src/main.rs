mod operator;
mod parser;
mod pile;
mod extractor;
mod attribute;
mod method;
mod method_with_parameters;
mod method_with_control_flow;
mod variables;
use std::io::{self, Write, BufRead};
use crate::{pile::Pile, attribute::Attribute, variables::Variables};


fn main() {
    let mut pile: Pile = Pile::new();
    let mut variables: Variables = Variables::new(10);
    let stdin = io::stdin();
    println!("\n⚠ Type \"exit\" to close...");
    println!("Enter Data:\n");
    print!("▶ ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        //pile.data.clear();
        if let Ok(line) = line {
            if !line.is_empty() {
                let extraction = extractor::extract(&line);
                let mut i = 0;
                while i != extraction.len(){
                    let value = extraction.get(i).unwrap();
                    let parsed_value = parser::parse_input(&value).unwrap();
                    match parsed_value {
                        Attribute::MethodWithControlFlow(method, offset) => {
                            Pile::method_with_control_flow(&mut pile, method, offset-1.0,  &mut i as *mut usize);
                        }
                        Attribute::MethodWithParameters(method, parameter) => {
                            Pile::method_with_parameters(&mut pile, method,  &mut variables.data, parameter);
                        }
                        Attribute::Method(method) => {
                            Pile::method(&mut pile, method, &mut variables.data);
                        }
                        Attribute::Operator(operator) => {
                            pile.operate(operator);
                        }
                        Attribute::Number(number) => {
                            pile.push(number);
                        }
                    }
                    i+=1;
                }
                println!("{}", pile.to_string())
            } else {
                println!("Closing software...");
                break;
            }
        }
        
        print!("▶ ");
        io::stdout().flush().unwrap();
    }
    println!("Hasta la vista, baby! 😎");
}