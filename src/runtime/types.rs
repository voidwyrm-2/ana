use std::{any::Any, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    common::AnaError,
    grammar::{
        lexer::{Token, TokenType},
        parser::Node,
    },
};

#[derive(Clone, Debug)]
pub enum AnaType {
    Any,
    Unit,
    Bool,
    Int,
    Float,
    Seq,
    String,
    Table,
    Function,
    Data,
}

impl AnaType {
    fn as_i32(&self) -> i32 {
        match self {
            AnaType::Any => 0,
            AnaType::Unit => 1,
            AnaType::Bool => 2,
            AnaType::Int => 3,
            AnaType::Float => 4,
            AnaType::Seq => 5,
            AnaType::String => 6,
            AnaType::Table => 7,
            AnaType::Function => 8,
            AnaType::Data => 9,
        }
    }
}

impl Display for AnaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AnaType::Any => "Any",
                AnaType::Unit => "Unit",
                AnaType::Bool => "Bool",
                AnaType::Int => "Int",
                AnaType::Float => "Float",
                AnaType::Seq => "Array",
                AnaType::String => "String",
                AnaType::Table => "Table",
                AnaType::Function => "Function",
                AnaType::Data => "Data",
            }
        )
    }
}

impl PartialEq for AnaType {
    fn eq(&self, other: &Self) -> bool {
        let a = self.as_i32();
        let b = other.as_i32();

        a == b || a == 0 || b == 0
    }

    fn ne(&self, other: &Self) -> bool {
        !(self == other)
    }
}

impl Eq for AnaType {}

macro_rules! err_uncastable {
    ($self:expr, $into:expr) => {
        Err(AnaError::from(format!(
            "cannot cast {} (type of {}) into type {}",
            $self,
            $self.kind(),
            $into
        )))
    };
}

macro_rules! err_invalid_operation {
    ($left:expr, $right:expr, $operation:expr) => {
        Err(AnaError::from(format!(
            "cannot use operator '{}' on {} and {} (type of {})",
            $operation,
            $left,
            $right,
            $left.kind(),
        )))
    };
}

#[macro_export]
macro_rules! ana_int {
    ($value:expr) => {
        AnaValue::Int($value)
    };
}

#[macro_export]
macro_rules! ana_float {
    ($value:expr) => {
        AnaValue::Float($value)
    };
}

#[macro_export]
macro_rules! ana_bool {
    ($value:expr) => {
        AnaValue::Bool($value)
    };
}

fn is_nocast(operation: &TokenType, other: &AnaValue) -> bool {
    let op = match operation {
        TokenType::Equals | TokenType::NotEquals | TokenType::LessThan | TokenType::GreaterThan => {
            true
        }
        _ => false,
    };

    let alt = match other {
        AnaValue::Float(_) | AnaValue::Bool(_) => true,
        _ => false,
    };

    op && alt
}

fn is_altcast(operation: &TokenType, other: &AnaValue) -> bool {
    let op = match operation {
        TokenType::Equals | TokenType::NotEquals | TokenType::LessThan | TokenType::GreaterThan => {
            true
        }
        _ => false,
    };

    let alt = match other {
        AnaValue::Float(_) | AnaValue::Bool(_) => true,
        _ => false,
    };

    op && alt
}

pub type TableMap = HashMap<String, AnaValue>;

#[derive(Clone, Debug)]
pub enum AnaValue {
    Unit,
    Bool(bool),
    Int(i32),
    Float(f32),
    Seq(Vec<AnaValue>),
    String(String),
    Table(TableMap),
    Function(AnaFunction),
    Data(Rc<dyn Any>),
}

impl AnaValue {
    pub fn kind(&self) -> AnaType {
        match self {
            AnaValue::Unit => AnaType::Unit,
            AnaValue::Bool(_) => AnaType::Bool,
            AnaValue::Int(_) => AnaType::Int,
            AnaValue::Float(_) => AnaType::Float,
            AnaValue::Seq(_) => AnaType::Seq,
            AnaValue::String(_) => AnaType::String,
            AnaValue::Table(_) => AnaType::Table,
            AnaValue::Function(_) => AnaType::Function,
            AnaValue::Data(_) => AnaType::Data,
        }
    }

    pub fn copy(&self) -> AnaValue {
        self.cast(self.kind())
            .expect(format!("copy failure of {}", self.kind()).as_str())
    }

    pub fn get_inner(&self, index: AnaValue) -> Result<AnaValue, AnaError> {
        match self {
            AnaValue::Seq(seq) => {
                if let AnaValue::Int(ind) = index {
                    Ok(seq[ind as usize].clone())
                } else {
                    Err(AnaError::from(format!(
                        "cannot index into {} with type {}",
                        self.kind(),
                        index.kind(),
                    )))
                }
            }
            AnaValue::Table(tab) => {
                if let AnaValue::String(ref ind) = index {
                    if let Some(value) = tab.get(ind) {
                        Ok(value.clone())
                    } else {
                        Err(AnaError::from(format!(
                            "'{}' does not exist in {}",
                            ind,
                            self.kind(),
                        )))
                    }
                } else {
                    Err(AnaError::from(format!(
                        "cannot index into {} with type {}",
                        self.kind(),
                        index.kind(),
                    )))
                }
            }
            _ => Err(AnaError::from(format!(
                "cannot index into type {}",
                self.kind()
            ))),
        }
    }

    pub fn set_inner(&mut self, index: AnaValue, value: AnaValue) -> Result<(), AnaError> {
        match self {
            AnaValue::Seq(seq) => {
                if let AnaValue::Int(ind) = index {
                    seq[ind as usize] = value;
                    Ok(())
                } else {
                    Err(AnaError::from(format!(
                        "cannot index into {} with type {}",
                        self.kind(),
                        index.kind(),
                    )))
                }
            }
            AnaValue::Table(tab) => {
                if let AnaValue::String(ind) = index {
                    tab.insert(ind, value);
                    Ok(())
                } else {
                    Err(AnaError::from(format!(
                        "cannot index into {} with type {}",
                        self.kind(),
                        index.kind(),
                    )))
                }
            }
            _ => Err(AnaError::from(format!(
                "cannot index into type {}",
                self.kind()
            ))),
        }
    }

    pub fn cast(&self, into: AnaType) -> Result<AnaValue, AnaError> {
        match self {
            AnaValue::Unit => match into {
                AnaType::Bool => Ok(ana_bool!(false)),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Bool(from) => match into {
                AnaType::Bool => Ok(ana_bool!(*from)),
                AnaType::Int => Ok(if *from { ana_int!(1) } else { ana_int!(0) }),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Int(from) => match into {
                AnaType::Bool => Ok(ana_bool!(*from != 0)),
                AnaType::Int => Ok(ana_int!(*from)),
                AnaType::Float => Ok(AnaValue::Float(*from as f32)),
                AnaType::Seq => Ok(AnaValue::Seq(vec![AnaValue::Int(*from)])),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Float(from) => match into {
                AnaType::Bool => Ok(ana_bool!(*from != 0.0)),
                AnaType::Int => Ok(ana_int!(*from as i32)),
                AnaType::Float => Ok(AnaValue::Float(*from)),
                AnaType::Seq => Ok(AnaValue::Seq(vec![AnaValue::Float(*from)])),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Seq(from) => match into {
                AnaType::Bool => Ok(ana_bool!(from.len() != 0)),
                AnaType::Seq => {
                    let mut new_arr: Vec<AnaValue> = Vec::new();

                    for v in from {
                        new_arr.push(v.clone());
                    }

                    Ok(AnaValue::Seq(new_arr))
                }
                AnaType::String => {
                    let mut new_str = String::new();

                    for v in from {
                        if let AnaValue::Int(n) = v {
                            let un = match (*n).try_into() {
                                Ok(n) => Ok(n),
                                Err(e) => Err(AnaError::from(format!("{}", e))),
                            }?;

                            if let Some(c) = char::from_u32(un) {
                                new_str.push(c);
                            } else {
                                return Err(AnaError::from(format!(
                                    "cannot convert {} into character",
                                    un
                                )));
                            }
                        } else {
                            return Err(AnaError::from(format!(
                                "cannot convert {} (type of {}) into character",
                                v,
                                v.kind(),
                            )));
                        }
                    }

                    Ok(AnaValue::String(new_str))
                }
                _ => err_uncastable!(self, into),
            },
            AnaValue::String(from) => match into {
                AnaType::Bool => Ok(ana_bool!(from.len() != 0)),
                AnaType::Seq => {
                    let mut new_arr: Vec<AnaValue> = Vec::new();

                    for v in from.clone().into_bytes() {
                        new_arr.push(AnaValue::Int(v as i32));
                    }

                    Ok(AnaValue::Seq(new_arr))
                }
                AnaType::String => Ok(AnaValue::String(from.clone())),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Table(from) => match into {
                AnaType::Bool => Ok(ana_bool!(from.len() != 0)),
                AnaType::Table => todo!("implement Table -> Table casting"),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Function(_) => match into {
                AnaType::Bool => Ok(ana_bool!(true)),
                AnaType::Function => todo!("implement Function -> Function casting"),
                _ => err_uncastable!(self, into),
            },
            AnaValue::Data(_) => err_uncastable!(self, into),
        }
    }

    pub fn op(&self, operation: &TokenType, other: &AnaValue) -> Result<AnaValue, AnaError> {
        // String concatenation: only allow string ⊂ string
        if let AnaValue::String(str) = self {
            if let AnaValue::String(other_str) = other {
                match *operation {
                    TokenType::Concat => return Ok(AnaValue::String(str.clone() + other_str)),
                    TokenType::Equals => return Ok(ana_bool!(*str == *other_str)),
                    TokenType::NotEquals => return Ok(ana_bool!(*str != *other_str)),
                    _ => {
                        return Err(AnaError::from(format!(
                            "unsupported operation '{}' for String + String",
                            operation
                        )));
                    }
                }
            }
        }
        if let AnaValue::Seq(arr) = self {
            if let AnaValue::Seq(other_arr) = other {
                if *operation == TokenType::Concat {
                    let mut new_arr: Vec<AnaValue> = Vec::new();

                    for item in arr {
                        new_arr.push(item.clone());
                    }

                    for item in other_arr {
                        new_arr.push(item.clone());
                    }

                    Ok(AnaValue::Seq(new_arr))
                } else {
                    if arr.len() != other_arr.len() {
                        return Err(AnaError::from(
                            "cannot perform an operation on sequences of differing sizes",
                        ));
                    }

                    let mut new_arr: Vec<AnaValue> = Vec::new();

                    for i in 0..arr.len() {
                        new_arr.push(arr[i].op(operation, &other_arr[i])?);
                    }

                    Ok(AnaValue::Seq(new_arr))
                }
            } else if let AnaValue::String(_) = other {
                let other_seq = other.cast(AnaType::Seq)?;

                self.op(operation, &other_seq)
            } else {
                let mut new_arr: Vec<AnaValue> = Vec::new();

                for i in 0..arr.len() {
                    new_arr.push(arr[i].op(operation, other)?);
                }

                Ok(AnaValue::Seq(new_arr))
            }
        } else {
            let own_casted = if is_altcast(operation, other) && self.kind() != other.kind() {
                self.cast(other.kind())?
            } else {
                self.clone()
            };

            let casted = if is_nocast(operation, other) {
                other.copy()
            } else {
                other.cast(self.kind())?
            };

            match own_casted {
                AnaValue::Int(left) => {
                    if let AnaValue::Int(right) = casted {
                        let v = match operation {
                            TokenType::Add => ana_int!(left + right),
                            TokenType::Subtract => ana_int!(left - right),
                            TokenType::Multiply => ana_int!(left * right),
                            TokenType::Divide => ana_int!(left / right),
                            TokenType::Modulus => ana_int!(left % right),
                            TokenType::LessThan => ana_bool!(left < right),
                            TokenType::GreaterThan => ana_bool!(left > right),
                            TokenType::Equals => ana_bool!(left == right),
                            TokenType::NotEquals => ana_bool!(left != right),
                            _ => return err_invalid_operation!(self, casted, operation),
                        };

                        Ok(v)
                    } else {
                        unreachable!()
                    }
                }
                AnaValue::Float(left) => {
                    if let AnaValue::Float(right) = casted {
                        let v = match operation {
                            TokenType::Add => ana_float!(left + right),
                            TokenType::Subtract => ana_float!(left - right),
                            TokenType::Multiply => ana_float!(left * right),
                            TokenType::Divide => ana_float!(left / right),
                            TokenType::Modulus => ana_float!(left % right),
                            TokenType::LessThan => ana_bool!(left < right),
                            TokenType::GreaterThan => ana_bool!(left > right),
                            TokenType::Equals => ana_bool!(left == right),
                            TokenType::NotEquals => ana_bool!(left != right),
                            _ => return err_invalid_operation!(self, casted, operation),
                        };

                        Ok(v)
                    } else {
                        unreachable!()
                    }
                }
                AnaValue::Bool(left) => {
                    if let AnaValue::Bool(right) = casted {
                        let v = match operation {
                            TokenType::Equals => ana_bool!(left == right),
                            TokenType::NotEquals => ana_bool!(left != right),
                            _ => return err_invalid_operation!(self, casted, operation),
                        };

                        Ok(v)
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!("missing branch for {}", self.kind()),
            }
        }
    }
}

impl Display for AnaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnaValue::Unit => write!(f, "Unit"),
            AnaValue::Bool(v) => write!(f, "{}", v),
            AnaValue::Int(v) => write!(f, "{}", v),
            AnaValue::Float(v) => write!(f, "{}", v),
            AnaValue::Seq(v) => {
                let strs: Vec<String> = v.iter().map(|i| format!("{}", i)).collect();
                write!(f, "[{}]", strs.join(", "))
            }
            AnaValue::String(v) => write!(f, "{}", v),
            AnaValue::Table(v) => write!(f, "{:?}", v),
            AnaValue::Function(v) => match v {
                AnaFunction::Composite(_) => write!(f, "<function>"),
                AnaFunction::Builtin(_) => write!(f, "<builtin function>"),
            },
            AnaValue::Data(v) => write!(f, "{:?}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AnaFunction {
    Composite((Vec<Token>, Vec<Node>)),
    Builtin(fn(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError>),
}
