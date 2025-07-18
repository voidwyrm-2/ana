use std::{any::Any, fmt::Display};

use crate::grammar::lexer::TokenType;

#[derive(Clone, Debug)]
pub enum AnaType {
    Int,
    Float,
}

type AnaValueBox = Box<dyn AnaValue>;

pub trait AnaValue {
    fn as_any(&self) -> &dyn Any;
    fn as_string(&self) -> String;
    fn kind(&self) -> AnaType;
    fn cast(&self, into: AnaType) -> AnaValueBox;
    fn op(&self, operation: TokenType, other: AnaValueBox) -> AnaValueBox;
}

macro_rules! format_downcast_expect {
    ($($left:expr, $right:expr, $name:expr),+) => {
        format!(
            "(possibly misused?) invalid downcast from {:?} to {:?} in {}",
        $($left)+.kind(),
        $($right)+.kind(),
        $($name)+,
        )
        .as_str()
    };
}

#[derive(Clone, Debug)]
pub struct AnaInt {
    value: i32,
}

impl AnaInt {
    pub fn new(value: i32) -> AnaInt {
        AnaInt { value: value }
    }
}

impl Display for AnaInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl AnaValue for AnaInt {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_string(&self) -> String {
        format!("{}", self)
    }

    fn kind(&self) -> AnaType {
        AnaType::Int
    }

    fn cast(&self, into: AnaType) -> AnaValueBox {
        match into {
            AnaType::Int => Box::new(self.clone()),
            AnaType::Float => Box::new(AnaFloat::new(self.value as f32)),
        }
    }

    fn op(&self, operation: TokenType, other: AnaValueBox) -> AnaValueBox {
        let other_cast = other.cast(self.kind());
        let other_v = other_cast
            .as_any()
            .downcast_ref::<AnaInt>()
            .expect(format_downcast_expect!(self, other, "AnaInt::op"));

        let result = match operation {
            TokenType::Add => self.value + other_v.value,
            TokenType::Subtract => self.value - other_v.value,
            TokenType::Multiply => self.value * other_v.value,
            TokenType::Divide => self.value / other_v.value,
            TokenType::Modulus => self.value % other_v.value,
            _ => unreachable!("unreachable branch {:?} in AnaInt::op", operation),
        };

        Box::new(AnaInt::new(result))
    }
}

#[derive(Clone, Debug)]
pub struct AnaFloat {
    value: f32,
}

impl AnaFloat {
    pub fn new(value: f32) -> AnaFloat {
        AnaFloat { value: value }
    }
}

impl Display for AnaFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl AnaValue for AnaFloat {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_string(&self) -> String {
        format!("{}", self)
    }

    fn kind(&self) -> AnaType {
        AnaType::Float
    }

    fn cast(&self, into: AnaType) -> AnaValueBox {
        match into {
            AnaType::Int => Box::new(AnaInt::new(self.value as i32)),
            AnaType::Float => Box::new(self.clone()),
        }
    }

    fn op(&self, operation: TokenType, other: AnaValueBox) -> AnaValueBox {
        let other_cast = other.cast(self.kind());
        let other_v = other_cast
            .as_any()
            .downcast_ref::<AnaFloat>()
            .expect(format_downcast_expect!(self, other, "AnaFloat::op"));

        let result = match operation {
            TokenType::Add => self.value + other_v.value,
            TokenType::Subtract => self.value - other_v.value,
            TokenType::Multiply => self.value * other_v.value,
            TokenType::Divide => self.value / other_v.value,
            TokenType::Modulus => self.value % other_v.value,
            _ => unreachable!("unreachable branch {:?} in AnaInt::op", operation),
        };

        Box::new(AnaFloat::new(result))
    }
}
