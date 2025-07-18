use std::collections::HashMap;

use crate::{
    common::AnaError,
    grammar::{lexer::TokenType, parser::Node},
    runtime::{
        builtins::get_builtins,
        stdlib::get_stdlib_module,
        types::{AnaFunction, AnaType, AnaValue, TableMap},
    },
    token_anaerr, token_err,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ScopeStatus {
    Global,
    Block,
    Func,
}

impl ScopeStatus {
    pub fn as_i32(&self) -> i32 {
        match self {
            ScopeStatus::Global => 0,
            ScopeStatus::Block => 1,
            ScopeStatus::Func => 2,
        }
    }

    pub fn promote(&self, status: ScopeStatus) -> ScopeStatus {
        if self.as_i32() < status.as_i32() {
            status
        } else {
            self.clone()
        }
    }

    pub fn inside_of(&self, status: ScopeStatus) -> bool {
        self.as_i32() >= status.as_i32()
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    bindings: TableMap,
}

impl Scope {
    pub fn new(parent: Option<Box<Scope>>, default_bindings: Option<TableMap>) -> Scope {
        Scope {
            parent: parent,
            bindings: default_bindings.unwrap_or(HashMap::new()),
        }
    }

    pub fn get(&self, name: &String) -> Option<&AnaValue> {
        if let Some(value) = self.bindings.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: String, value: AnaValue) {
        self.bindings.insert(name, value);
    }

    pub fn has(&self, name: &String) -> bool {
        self.bindings.contains_key(name)
    }

    pub fn has_any(&self, name: &String) -> bool {
        if self.has(name) {
            true
        } else if let Some(parent) = &self.parent {
            parent.has_any(name)
        } else {
            false
        }
    }

    fn keys_inner(&self, out: &mut Vec<String>) {
        for k in self.bindings.keys() {
            out.push(k.clone());
        }

        if let Some(parent) = &self.parent {
            parent.keys_inner(out);
        }
    }

    pub fn keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = Vec::new();

        self.keys_inner(&mut keys);

        keys
    }
}

macro_rules! expect_func {
    ($status:expr, $anchor:expr, $msg:expr) => {
        if !$status.inside_of(ScopeStatus::Func) {
            return Err(token_anaerr!(
                $anchor,
                "cannot use {} outside of a function",
                $msg,
            ));
        }
    };
}

macro_rules! expect_block {
    ($status:expr, $anchor:expr, $msg:expr) => {
        if $status.inside_of(ScopeStatus::Global) {
            return Err(token_anaerr!(
                $anchor,
                "cannot use {} outside of a block",
                $msg,
            ));
        }
    };
}

macro_rules! expect_global {
    ($status:expr, $anchor:expr, $msg:expr) => {
        if !$status.inside_of(ScopeStatus::Global) {
            return Err(token_anaerr!(
                $anchor,
                "cannot use {} in a scope lower than of global",
                $msg,
            ));
        }
    };
}

pub struct Interpreter {
    scope: Scope,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            scope: Scope::new(None, Some(get_builtins())),
        }
    }

    pub fn enter_scope(&mut self) {
        //println!("before enter: {:?}", self.scope);
        let parent = Box::new(self.scope.clone());
        self.scope = Scope::new(Some(parent), None);
        //println!("after enter: {:?}", self.scope);
    }

    pub fn exit_scope(&mut self) {
        //println!("before exit: {:?}", self.scope);
        if let Some(parent) = &self.scope.parent {
            self.scope = *parent.clone();
        }
        //println!("after exit: {:?}", self.scope);
    }

    pub fn do_expr(&mut self, expr: Node) -> Result<AnaValue, AnaError> {
        match expr {
            Node::BinaryExpr { left, op, right } => {
                if *op.token().get_typ() == TokenType::MethodOf {
                    self.handle_methodof(*left, *right)
                } else {
                    let lv = self.do_expr(*left)?;

                    let rv_e = self.do_expr(*right);

                    let rv = rv_e?;

                    let operation = op.token().get_typ();

                    let result = match lv.op(operation, &rv) {
                        Ok(r) => Ok(r),
                        Err(e) => Err(token_anaerr!(op.token(), "{}", e)),
                    }?;

                    Ok(result)
                }
            }
            Node::Ident(path) => {
                let mut now_value: Option<AnaValue> = None;

                for part in path {
                    if let TokenType::Ident(name) = part.get_typ() {
                        if now_value.is_none() {
                            if let Some(value) = self.scope.get(name) {
                                now_value = Some(value.clone());
                            } else {
                                return Err(token_anaerr!(part, "'{}' doesn't exist", name));
                            }
                        } else {
                            let unwrapped = now_value.unwrap();

                            let inner_value =
                                match unwrapped.get_inner(AnaValue::String(name.clone())) {
                                    Ok(r) => Ok(r),
                                    Err(e) => Err(token_anaerr!(part, "{}", e)),
                                }?;

                            now_value = Some(inner_value);
                        }
                    } else {
                        unreachable!()
                    }
                }

                Ok(now_value.unwrap())
            }
            Node::Int(v) => {
                let val = match v.get_typ() {
                    TokenType::Int(v) => v,
                    _ => unreachable!(),
                };

                Ok(AnaValue::Int(*val))
            }
            Node::Float(v) => {
                let val = match v.get_typ() {
                    TokenType::Float(v) => v,
                    _ => unreachable!(),
                };

                Ok(AnaValue::Float(*val))
            }
            Node::String(v) => {
                let val = match v.get_typ() {
                    TokenType::String(v) => v,
                    _ => unreachable!(),
                };

                Ok(AnaValue::String(val.clone()))
            }
            Node::Seq { start: _, contents } => {
                let mut values: Vec<AnaValue> = Vec::new();

                for expr in contents {
                    values.push(self.do_expr(expr)?);
                }

                Ok(AnaValue::Seq(values))
            }
            Node::FunctionCall { name, args } => {
                let fun = self.do_expr(*name.clone())?;

                let args_seq = if let AnaValue::Seq(seq) = self.do_expr(*args.clone())? {
                    seq
                } else {
                    unreachable!(
                        "args was expected to be AnaValue::Seq, but was actually {:?}",
                        args
                    )
                };

                if let AnaValue::Function(func) = fun {
                    let result = match func {
                        AnaFunction::Composite((args, content)) => {
                            if args_seq.len() < args.len() {
                                Err(token_anaerr!(
                                    args[args_seq.len()],
                                    "expected {} arguments, but {} were given",
                                    args.len(),
                                    args_seq.len()
                                ))
                            } else if args_seq.len() > args.len() {
                                Err(token_anaerr!(
                                    args.last().unwrap(),
                                    "expected {} arguments, but {} were given",
                                    args.len(),
                                    args_seq.len()
                                ))
                            } else {
                                self.enter_scope();

                                for i in 0..args.len() {
                                    if let TokenType::Ident(name) = args[i].get_typ() {
                                        self.scope.set(name.clone(), args_seq[i].clone());
                                    } else {
                                        unreachable!()
                                    }
                                }

                                let result = self.execute_nodes(content, ScopeStatus::Func)?;

                                self.exit_scope();

                                Ok(result)
                            }
                        }
                        AnaFunction::Builtin(f) => f(args_seq),
                    }?;

                    Ok(result.unwrap_or(AnaValue::Unit))
                } else {
                    Err(token_anaerr!(
                        name.token(),
                        "cannot call type of {}",
                        fun.kind()
                    ))
                }
            }
            Node::Function { args, block, .. } => {
                if let Node::Block(_, content) = *block {
                    Ok(AnaValue::Function(AnaFunction::Composite((args, content))))
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!("missing branch for {:?} in Interpreter::do_expr", expr),
        }
    }

    pub fn do_if(&mut self, node: Node, status: ScopeStatus) -> Result<Option<AnaValue>, AnaError> {
        if let Node::If {
            expr,
            block_true,
            block_false,
            ..
        } = node
        {
            let result = self.do_expr(*expr.clone())?;

            let cond = match result.cast(AnaType::Bool) {
                Ok(v) => Ok(v),
                Err(e) => Err(token_anaerr!(expr.token(), "{}", e)),
            }?;

            if let AnaValue::Bool(b) = cond {
                if b {
                    if let Node::Block(_, nodes) = *block_true {
                        let result =
                            self.execute_nodes(nodes, status.promote(ScopeStatus::Block))?;

                        if let Some(value) = result {
                            return Ok(Some(value));
                        }
                    } else {
                        unreachable!()
                    }
                } else if let Some(block) = block_false {
                    if let Node::Block(_, nodes) = *block {
                        let result =
                            self.execute_nodes(nodes, status.promote(ScopeStatus::Block))?;

                        if let Some(value) = result {
                            return Ok(Some(value));
                        }
                    } else if let Node::If { .. } = *block {
                        return self.do_if(*block, status);
                    } else {
                        unreachable!()
                    }
                }
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }

        Ok(None)
    }

    fn handle_methodof(&mut self, left: Node, right: Node) -> Result<AnaValue, AnaError> {
        if let Node::FunctionCall { name, args } = right {
            if let Node::Seq { start, contents } = *args {
                let mut new_args: Vec<Node> = Vec::with_capacity(contents.len() + 1);

                new_args.push(left);

                contents.iter().for_each(|a| new_args.push(a.clone()));

                self.do_expr(Node::FunctionCall {
                    name: name,
                    args: Box::new(Node::Seq {
                        start: start.clone(),
                        contents: new_args,
                    }),
                })
            } else {
                unreachable!()
            }
        } else {
            Err(token_anaerr!(
                right.token(),
                "only functions can be used as methods",
            ))
        }
    }

    pub fn execute_nodes(
        &mut self,
        nodes: Vec<Node>,
        status: ScopeStatus,
    ) -> Result<Option<AnaValue>, AnaError> {
        for node in nodes {
            match node {
                Node::Binding { name, expr } => {
                    let value = self.do_expr(*expr)?;

                    if let TokenType::Ident(idname) = name.token().get_typ() {
                        if self.scope.has(idname) {
                            return Err(token_anaerr!(name.token(), "'{}' already exists", idname));
                        }

                        self.scope.set(idname.clone(), value);
                    } else {
                        unreachable!();
                    }
                }
                Node::FunctionCall { .. } => _ = self.do_expr(node)?,
                Node::Return((_, expr)) => {
                    expect_func!(status, expr.token(), "return");

                    let result = self.do_expr(*expr)?;

                    return Ok(Some(result));
                }
                Node::Import((_, path)) => {
                    expect_global!(status, path, "imports");

                    if let TokenType::String(str) = path.get_typ() {
                        if str.starts_with("std/") {
                            let modname = &str[4..].to_string();

                            let value = match get_stdlib_module(modname) {
                                Ok(v) => Ok(v),
                                Err(e) => Err(token_anaerr!(path, "{}", e)),
                            }?;

                            if self.scope.has(modname) {
                                return Err(token_anaerr!(path, "'{}' already exists", modname));
                            }

                            self.scope.set(modname.clone(), value);
                        } else if str.starts_with("lib/") {
                            todo!("implement library module importing")
                        } else {
                            todo!("implement local module importing")
                        }
                    } else {
                        unreachable!()
                    }
                }
                Node::Block(_, content) => {
                    self.enter_scope();
                    _ = self.execute_nodes(content, status.promote(ScopeStatus::Block))?;
                    self.exit_scope();
                }
                Node::If { .. } => {
                    if let Some(value) = self.do_if(node, status.clone())? {
                        return Ok(Some(value));
                    }
                }
                Node::BinaryExpr { left, op, right } => {
                    if *op.token().get_typ() != TokenType::MethodOf {
                        unreachable!();
                    } else {
                        _ = self.handle_methodof(*left, *right)?;
                    }
                }
                _ => unreachable!("missing branch for {:?} in Interpreter::execute", node),
            }
        }

        //for k in self.scope.keys() {
        //    println!("{}: {}", k, self.scope.get(&k).unwrap());
        //}

        Ok(None)
    }

    pub fn execute(&mut self, nodes: Vec<Node>) -> Result<(), AnaError> {
        self.execute_nodes(nodes, ScopeStatus::Global)?;

        Ok(())
    }
}
