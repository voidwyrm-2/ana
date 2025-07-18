use std::collections::HashMap;

use crate::{
    common::AnaError,
    runtime::types::{AnaFunction, AnaType, AnaValue, TableMap},
};

#[macro_export]
macro_rules! insert_v {
    ($m:expr, $name:expr, $value:expr) => {
        $m.insert($name.to_string(), $value)
    };
}

#[macro_export]
macro_rules! insert_f {
    ($m:expr, $name:expr, $func:expr) => {
        insert_v!($m, $name, AnaValue::Function(AnaFunction::Builtin($func)))
    };
}

#[macro_export]
macro_rules! expect_args {
    ($args:expr, $amount:expr) => {};
}

pub fn get_stdlib_module(name: &String) -> Result<AnaValue, AnaError> {
    let m = match name.as_str() {
        "io" => Ok(get_mod_io()),
        "debug" => Ok(get_mod_debug()),
        _ => Err(AnaError::from(format!(
            "no module '{}' exists in std",
            name
        ))),
    }?;

    Ok(AnaValue::Table(m))
}

fn get_mod_io() -> TableMap {
    let mut m: TableMap = HashMap::new();

    insert_f!(m, "Outln", io_outln);

    m
}

fn io_outln(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();

    println!("{}", items.join(" "));

    Ok(None)
}

fn get_mod_debug() -> TableMap {
    let mut m: TableMap = HashMap::new();

    insert_f!(m, "Assert", mdebug_assert);

    m
}

fn mdebug_assert(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    let check = args[0].cast(AnaType::Bool)?;

    if let AnaValue::Bool(cond) = check {
        if !cond {
            return Err(AnaError::from(args[1].to_string()));
        }
    }

    Ok(None)
}
