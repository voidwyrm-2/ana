use std::collections::HashMap;

use crate::{
    common::AnaError,
    insert_f, insert_v,
    runtime::types::{AnaFunction, AnaValue, TableMap},
};

pub fn get_builtins() -> TableMap {
    let mut m: TableMap = HashMap::new();

    insert_v!(m, "Unit", AnaValue::Unit);

    insert_v!(m, "true", AnaValue::Bool(true));

    insert_v!(m, "false", AnaValue::Bool(false));

    insert_f!(m, "Typeof", builtin_typeof);
    insert_f!(m, "Len", builtin_len);
    insert_f!(m, "Index", builtin_index);

    m
}

fn builtin_typeof(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    Ok(Some(AnaValue::String(args[0].kind().to_string())))
}

fn builtin_len(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    if args.len() != 1 {
        return Err(AnaError::from("Len expects 1 argument"));
    }
    match &args[0] {
        AnaValue::String(s) => Ok(Some(AnaValue::Int(s.chars().count() as i32))),
        AnaValue::Seq(seq) => Ok(Some(AnaValue::Int(seq.len() as i32))),
        _ => Err(AnaError::from("Len expects a String or Seq")),
    }
}

fn builtin_index(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    if args.len() != 2 {
        return Err(AnaError::from(
            "Index expects 2 arguments (string/seq, int)",
        ));
    }
    match (&args[0], &args[1]) {
        (AnaValue::String(s), AnaValue::Int(i)) => {
            let idx = *i as usize;
            if let Some(ch) = s.chars().nth(idx) {
                Ok(Some(AnaValue::String(ch.to_string())))
            } else {
                Err(AnaError::from("Index out of bounds for String"))
            }
        }
        (AnaValue::Seq(seq), AnaValue::Int(i)) => {
            let idx = *i as usize;
            if idx < seq.len() {
                Ok(Some(seq[idx].clone()))
            } else {
                Err(AnaError::from("Index out of bounds for Seq"))
            }
        }
        _ => Err(AnaError::from("Index expects (String, Int) or (Seq, Int)")),
    }
}
