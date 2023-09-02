pub mod variables {

    pub struct Variables {
        pub data: Vec<f64>,
    }
    
    impl Variables {
        pub fn new(size: usize) -> Self {
            Self {
                data: vec![0.0; size],
            }
        }
    
        pub fn get(&self, index: usize) -> Option<f64> {
            self.data.get(index).copied()
        }
    
        pub fn set(&mut self, index: usize, value: f64) -> Option<()> {
            if let Some(element) = self.data.get_mut(index) {
                *element = value;
                Some(())
            } else {
                None
            }
        }
    }
}

pub use self::variables::Variables;