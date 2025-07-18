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

    m
}

fn builtin_typeof(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    Ok(Some(AnaValue::String(args[0].kind().to_string())))
}
