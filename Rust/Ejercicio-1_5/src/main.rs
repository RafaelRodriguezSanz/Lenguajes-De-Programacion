use std::io::{self, Write, BufRead};
use regex::Regex;

fn main() {
    let mut pile: Vec<f64> = Vec::new();
    let stdin = io::stdin();
    let regex = Regex::new(r"\d+\.\d+|\d+").unwrap();
    println!("\n⚠ Type \"exit\" to close...");
    println!("Enter Data:\n");
    print!("▶ ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        if let Ok(line) = line {
            if !line.is_empty() {
                if line == "exit" {
                    println!("Closing software...");
                    break;
                }
                for value in regex.captures_iter(&line) {
                    match value[0].parse::<f64>() {
                        Ok(double) => {
                            pile.push(double);
                            println!("⬇ {}", double);
                            io::stdout().flush().unwrap();
                        }
                        Err(_) => {
                            println!("Error: Can not convert {} to f64", value[0].to_string());
                            continue;
                        }
                    }
                }
            } else {
                if let Some(double) = pile.pop() {
                    println!("⬆ {}", double);
                } else {
                    println!("There was no values left ✖");
                }
            }
        }
        
        print!("▶ ");
        io::stdout().flush().unwrap();
    }

    println!("\nFinal values in pile: {:?}", pile);
}
