use std::collections::HashMap;

use crate::{
    common::AnaError,
    insert_f, insert_v,
    runtime::{
        stdlib::expect_args,
        types::{AnaFunction, AnaType, AnaValue, TableMap},
    },
};

pub fn get_builtins() -> TableMap {
    let mut m: TableMap = HashMap::new();

    insert_v!(m, "Unit", AnaValue::Unit);

    insert_v!(m, "true", AnaValue::Bool(true));

    insert_v!(m, "false", AnaValue::Bool(false));

    insert_f!(m, "Typeof", builtin_typeof);
    insert_f!(m, "Len", builtin_len);
    insert_f!(m, "Get", builtin_get);
    insert_f!(m, "Set", builtin_set);

    m
}

fn builtin_typeof(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    Ok(Some(AnaValue::String(args[0].kind().to_string())))
}

fn builtin_len(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    // TODO: make this more in line with Get and Set
    match &args[0] {
        AnaValue::String(s) => Ok(Some(AnaValue::Int(s.chars().count() as i32))),
        AnaValue::Seq(seq) => Ok(Some(AnaValue::Int(seq.len() as i32))),
        _ => Err(AnaError::from("Len expects a String or Seq")),
    }
}

fn builtin_get(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [AnaType::Any, AnaType::Any])?;

    let obj = args[0].clone();
    let index = args[1].clone();

    let result = obj.get_inner(index)?;

    Ok(Some(result))
}

fn builtin_set(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [AnaType::Any, AnaType::Any, AnaType::Any])?;

    let mut obj = args[0].clone();
    let index = args[1].clone();
    let value = args[2].clone();

    obj.set_inner(index, value)?;

    Ok(Some(obj))
}
