mod operator;
mod parser;
mod pile;
mod extractor;
mod attribute;
mod Operator;
use std::io::{self, Write, BufRead};
use crate::{pile::Pile, attribute::Attribute};

fn main() {
    let mut pile: Pile = Pile::new();
    let stdin = io::stdin();
    println!("\n⚠ Type \"exit\" to close...");
    println!("Enter Data:\n");
    print!("▶ ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        if let Ok(line) = line {
            if !line.is_empty() {
                let extraction = extractor::extract(&line);
                for value in extraction {
                    let parsed_value = parser::parse_input(&value).unwrap();
                    match parsed_value {
                        Attribute::Operator(operator) => {
                            pile.operate(operator);
                        }
                        Attribute::Number(number) => {
                            pile.push(number);
                        }
                    }
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