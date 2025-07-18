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
macro_rules! create_module {
    ($(addf $fname:expr, $fun:expr)*) => {
        let mut m: TableMap = HashMap::new();

        $(insert_f!(m, $fname, $fun);)*

        return m
    };
}

pub fn expect_args<const N: usize>(
    args: &Vec<AnaValue>,
    expected: [AnaType; N],
) -> Result<(), AnaError> {
    if args.len() != expected.len() {
        return Err(AnaError::from(format!(
            "expected {} {}, but found {} instead",
            expected.len(),
            if expected.len() == 1 {
                "argument"
            } else {
                "arguments"
            },
            args.len()
        )));
    }

    for i in 0..args.len() {
        if args[i].kind() != expected[i] {
            return Err(AnaError::from(format!(
                "expected type {}, but found {} (type of {}) instead",
                expected[i],
                args[i],
                args[i].kind()
            )));
        }
    }

    Ok(())
}

pub fn get_stdlib_module(name: &String) -> Result<AnaValue, AnaError> {
    let m = match name.as_str() {
        "io" => Ok(get_mod_io()),
        "tables" => Ok(get_mod_tables()),
        "debug" => Ok(get_mod_debug()),
        _ => Err(AnaError::from(format!(
            "no module '{}' exists in std",
            name
        ))),
    }?;

    Ok(AnaValue::Table(m))
}

fn get_mod_io() -> TableMap {
    create_module! {
        addf "Outln", io_outln
    }
}

fn io_outln(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    let items: Vec<String> = args.iter().map(|a| a.to_string()).collect();

    println!("{}", items.join(" "));

    Ok(None)
}

fn get_mod_tables() -> TableMap {
    create_module! {
        addf "Create", tables_create
        addf "Get", tables_get
        addf "Set", tables_set
    }
}

fn tables_create(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [])?;

    Ok(Some(AnaValue::Table(HashMap::new())))
}

fn tables_get(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [AnaType::Table, AnaType::String])?;

    let t = &args[0];
    let index = args[1].clone();

    let inner = t.get_inner(index)?;

    Ok(Some(inner))
}

fn tables_set(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [AnaType::Table, AnaType::String, AnaType::Any])?;

    // TODO: maybe make this change the original?

    let mut t = args[0].clone();
    let index = args[1].clone();
    let value = args[2].clone();

    t.set_inner(index, value)?;

    Ok(Some(t))
}

fn get_mod_debug() -> TableMap {
    create_module! {
        addf "Assert", mdebug_assert
    }
}

fn mdebug_assert(args: Vec<AnaValue>) -> Result<Option<AnaValue>, AnaError> {
    expect_args(&args, [AnaType::Bool, AnaType::String])?;

    let check = args[0].cast(AnaType::Bool)?;

    if let AnaValue::Bool(cond) = check {
        if !cond {
            return Err(AnaError::from(args[1].to_string()));
        }
    }

    Ok(None)
}
