use std::fmt;

pub mod method_with_control_flow {


    pub enum UnaryMethodWithControlFlow{
        CJP
    }

    pub enum ZeroaryMethodWithControlFlow {
        UJP
    }
    
    impl std::fmt::Display for UnaryMethodWithControlFlow {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                UnaryMethodWithControlFlow::CJP => write!(f, "CJP"),
            }
        }
    }

    impl std::fmt::Display for ZeroaryMethodWithControlFlow {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                ZeroaryMethodWithControlFlow::UJP => write!(f, "UJP")
            }
        }
    }

    pub trait Methodable {
        fn method_with_control_flow(&self, offset:f64, control_flow: *mut usize, parameter:f64) -> ();
    }

    impl Methodable for UnaryMethodWithControlFlow {
        fn method_with_control_flow(&self, offset:f64, control_flow: *mut usize, parameter:f64) -> () {
            match self {
                UnaryMethodWithControlFlow::CJP => {
                    if(parameter != 0.0) {
                        unsafe {
                            *control_flow = (*control_flow).wrapping_add(offset.floor() as usize);
                        }
                    }
                }
            }
        }
    }

    impl Methodable for ZeroaryMethodWithControlFlow {
        fn method_with_control_flow(&self, offset: f64, control_flow: *mut usize, _parameter: f64) -> () {
            match self {
                ZeroaryMethodWithControlFlow::UJP => unsafe {
                    *control_flow = (*control_flow).wrapping_add(offset.floor() as usize);
                }
            }
        }
    }
    

    pub enum MethodWithControlFlow {
        Unary(UnaryMethodWithControlFlow, f64),
        Zeroary(ZeroaryMethodWithControlFlow, f64)
    }
    

    impl MethodWithControlFlow {
        pub fn MethodWithControlFlow(&self, offset: f64, control_flow: *mut usize, parameter: f64 ) -> () {
            match self {
                MethodWithControlFlow::Unary(mth, _parameter) => {
                    mth.method_with_control_flow(offset, control_flow, parameter);
                }
                MethodWithControlFlow::Zeroary(mth, _parameter) => {
                    mth.method_with_control_flow(offset, control_flow, parameter);
                }
            }
        }

        pub fn to_string(&self) -> String {
            match self {
                MethodWithControlFlow::Unary(mth,  _) => mth.to_string(),
                MethodWithControlFlow::Zeroary(mth,  _) => mth.to_string()
            }
        }

        pub fn values() -> Vec<String> {
            return vec![
                ZeroaryMethodWithControlFlow::UJP.to_string(),
                UnaryMethodWithControlFlow::CJP.to_string()
            ];
        }

    }

}

pub use method_with_control_flow::{MethodWithControlFlow, UnaryMethodWithControlFlow, ZeroaryMethodWithControlFlow};
