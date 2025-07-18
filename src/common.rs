use std::{
    error::Error,
    fmt::{self, Debug, Display},
};

pub struct AnaError {
    msg: String,
}

impl From<&str> for AnaError {
    fn from(value: &str) -> Self {
        AnaError {
            msg: value.to_string(),
        }
    }
}

impl From<String> for AnaError {
    fn from(value: String) -> Self {
        AnaError { msg: value }
    }
}

impl From<fmt::Error> for AnaError {
    fn from(value: fmt::Error) -> Self {
        AnaError::from(format!("{}", value))
    }
}

impl Debug for AnaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Display for AnaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for AnaError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

pub fn try_index<T>(vec: &Vec<T>, index: usize) -> Option<&T> {
    if index < vec.len() {
        Some(&vec[index])
    } else {
        None
    }
}

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
