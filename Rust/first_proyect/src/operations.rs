pub mod my_module {
    pub fn suma(a: i32, b: i32) -> i32 {
        a + b
    }
}

pub use self::my_module::suma;

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn internal() {
        assert_eq!(4, suma(2, 2));
    }
}