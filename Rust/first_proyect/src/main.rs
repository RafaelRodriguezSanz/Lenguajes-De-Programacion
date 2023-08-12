mod operations;

fn main() {
    println!("Hello, world!");
    let result = operations::my_module::suma(3, 4);
    println!("Suma: {}", result);
}