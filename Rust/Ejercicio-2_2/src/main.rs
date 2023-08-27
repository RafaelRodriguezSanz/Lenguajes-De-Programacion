mod operator;
mod parser;
mod pile;
mod extractor;
mod attribute;
use std::io::{self, Write, BufRead};
use crate::pile::Pile;

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
                    if let Some(parsed_operator) = parser::to_operator(&value) {
                        if operator::operator::Operator::values().contains(&parsed_operator) {
                            pile.operate(parsed_operator);
                        }
                    } else {
                        if let Some(parsed_double) = parser::to_double(&value) {
                            pile.push(parsed_double);
                        }
                    }
                }
                println!("{:?}", pile.get_data())
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