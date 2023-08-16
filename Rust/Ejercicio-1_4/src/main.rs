use std::io::{self, Write, BufRead};
use rand::Rng;

fn main() {
    let mut palabras: Vec<String> = Vec::new();
    let stdin = io::stdin();
    let mut rng = rand::thread_rng();
    println!("Ingrese su lista de palabras:");
    let mut i = 1;
    print!("{}-", i);
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        if let Ok(line) = line {
            if !line.is_empty() {
                palabras.push(line.clone());
                i += 1;
                print!("{}-", i);
                io::stdout().flush().unwrap();
            } else {
                break;
            }
        }
    }
    println!("\nRANDOM: {}", palabras[rng.gen_range(0..palabras.len())]);
}