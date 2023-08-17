use std::io;

fn prompt(message: &str, line: &mut String) {
    println!("{message}");
    io::stdin().read_line(line).expect("Could not read from stdin!");
}

fn main() {
    let mut name = String::new();
    prompt("What is your name?", &mut name);
    println!("Hello, {}!", name.trim());
}
