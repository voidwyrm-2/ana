use std::fmt::Display;

use crate::{
    common::{variant_eq, AnaError},
    grammar::lexer::{Token, TokenType},
    token_anaerr, token_err,
};

#[derive(Clone, Debug)]
pub enum Node {
    Int(Token),
    Float(Token),
    String(Token),
    Op(Token),
    BinaryExpr {
        left: Box<Node>,
        op: Box<Node>,
        right: Box<Node>,
    },
    Seq {
        start: Token,
        contents: Vec<Node>,
    },
    Ident(Vec<Token>),
    Binding {
        name: Box<Node>,
        expr: Box<Node>,
    },
    Require((Token, Token)),
    FunctionCall {
        name: Box<Node>,
        args: Box<Node>,
    },
    Block(Token, Vec<Node>),
    Function {
        start: Token,
        args: Vec<Token>,
        block: Box<Node>,
    },
    Return((Token, Box<Node>)),
    If {
        start: Token,
        expr: Box<Node>,
        block_true: Box<Node>,
        block_false: Option<Box<Node>>,
    },
}

impl Node {
    pub fn token(&self) -> &Token {
        match self {
            Self::Int(t) => t,
            Self::Float(t) => t,
            Self::String(t) => t,
            Self::Op(t) => t,
            Self::BinaryExpr { left: n, .. } => n.token(),
            Self::Seq { start: t, .. } => t,
            Self::Ident(v) => &v[0],
            Self::Binding { name: n, .. } => n.token(),
            Self::Require((t, _)) => t,
            Self::FunctionCall { name, .. } => name.token(),
            Self::Block(t, _) => t,
            Self::Function { start: t, .. } => t,
            Self::Return((t, _)) => t,
            Self::If { start: t, .. } => t,
        }
    }

    pub fn formt(&self, indent: usize) -> String {
        let fmt = match self {
            Self::Int(_) | Self::Float(_) | Self::String(_) | Self::Op(_) => {
                format!("{:?}", self)
            }

            Self::BinaryExpr { left, op, right } => format!(
                "BinaryExpr {{\n left: {},\n op: {},\n right: {},\n}}",
                left.formt(indent + 1),
                op.formt(indent + 1),
                right.formt(indent + 1),
            ),

            Self::Seq { contents, .. } => {
                let seqf = contents
                    .iter()
                    .map(|node| node.formt(indent + 1))
                    .collect::<Vec<String>>()
                    .join(",\n ");

                format!("Seq [\n {},\n]", seqf)
            }

            Self::Ident(path) => {
                let pathf = path
                    .iter()
                    .map(|tok| format!("{}", tok))
                    .collect::<Vec<String>>()
                    .join(",\n ");

                format!("Ident [\n {},\n]", pathf)
            }

            Self::Binding { name, expr } => format!(
                "Binding {{\n name: {},\n expr: {},\n}}",
                name.formt(indent + 1),
                expr.formt(indent + 1)
            ),

            Self::Require((_, str)) => format!("Require({})", str),

            Self::FunctionCall { name, args } => format!(
                "FunctionCall {{\n name: {},\n args: {},\n}}",
                name.formt(indent + 1),
                args.formt(indent + 1)
            ),

            Self::Block(_, nodes) => {
                let nodesf = nodes
                    .iter()
                    .map(|node| node.formt(indent + 1))
                    .collect::<Vec<String>>()
                    .join(",\n ");

                format!("Block [\n {},\n]", nodesf)
            }

            Self::Function { args, block, .. } => {
                let argsf = args
                    .iter()
                    .map(|tok| format!("{:?}", tok))
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "Function {{\n args: ({}),\n block: {},\n\n}}",
                    argsf,
                    block.formt(indent + 1)
                )
            }

            Self::Return((_, expr)) => format!("Return {{\n expr: {},\n}}", expr.formt(indent + 1)),

            Self::If {
                expr,
                block_true,
                block_false,
                ..
            } => format!(
                "If {{\n expr: {},\n block_true: {},\n block_false: {},\n}}",
                expr.formt(indent + 1),
                block_true.formt(indent + 1),
                if let Some(block) = block_false {
                    block.formt(indent + 1)
                } else {
                    String::new()
                },
            ),
        };

        let mut pad = String::new();

        for _ in 0..indent {
            pad.push(' ');
        }

        fmt.lines()
            .map(|line| pad.clone() + line)
            .collect::<Vec<String>>()
            .join("\n")
            .trim()
            .to_owned()
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.formt(0))
    }
}

type ParserResult = Result<(), AnaError>;

pub struct Parser {
    idx: usize,
    tokens: Vec<Token>,
    refer_stack: Vec<Node>,
    nodes: Vec<Node>,
    block_nodes: Vec<Node>,
    block_nesting: Vec<u8>,
    last_node: Vec<(Node, bool)>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            idx: 0,
            tokens: tokens,
            refer_stack: Vec::new(),
            nodes: Vec::new(),
            block_nodes: Vec::new(),
            block_nesting: Vec::new(),
            last_node: Vec::new(),
        }
    }

    fn cur(&self) -> &Token {
        return &self.tokens[self.idx];
    }

    fn next(&mut self) -> &Token {
        let t = &self.tokens[self.idx];
        self.idx += 1;
        return t;
    }

    fn eat(&mut self) {
        _ = self.next()
    }

    fn peek(&self) -> Option<&Token> {
        if self.idx + 1 < self.tokens.len() {
            Some(&self.tokens[self.idx + 1])
        } else {
            None
        }
    }

    fn prev(&self) -> Option<&Token> {
        if self.idx != 0 {
            Some(&self.tokens[self.idx - 1])
        } else {
            None
        }
    }

    fn add_node(&mut self, node: Node) {
        if self.block_nesting.len() > 0 {
            self.block_nodes.push(node.clone());
        } else {
            self.nodes.push(node.clone());
        }

        self.last_node.push((node, false));
    }

    fn pop_node(&mut self) -> Option<Node> {
        _ = self.last_node.pop();

        if self.block_nesting.len() > 0 {
            self.block_nodes.pop()
        } else {
            self.nodes.pop()
        }
    }

    fn add_refered(&mut self, node: Node) {
        self.refer_stack.push(node.clone());
        self.last_node.push((node, true));
    }

    fn pop_refered(&mut self) -> Option<Node> {
        _ = self.last_node.pop();
        self.refer_stack.pop()
    }

    fn expect_t(&self, t: &Token, tt: TokenType) -> ParserResult {
        if variant_eq(t.get_typ(), &tt) {
            Ok(())
        } else {
            Err(token_anaerr!(
                t,
                "expected token '{}', but found '{}' instead",
                tt,
                t.get_typ(),
            ))
        }
    }

    fn expect(&self, tt: TokenType) -> ParserResult {
        let t = &self.tokens[self.idx];
        self.expect_t(t, tt)
    }

    fn import(&mut self, start: &Token) -> ParserResult {
        self.expect(TokenType::String(String::new()))?;

        let name = self.next().clone();

        self.add_node(Node::Require((start.clone(), name)));

        Ok(())
    }

    fn r#return(&mut self, start: &Token) -> ParserResult {
        let expr = self.expr_inner(0)?;

        self.add_node(Node::Return((start.clone(), Box::new(expr))));

        Ok(())
    }

    fn ident_path(&mut self, start: &Token) -> ParserResult {
        if *self.cur().get_typ() != TokenType::IdentPathSep {
            self.add_refered(Node::Ident(vec![start.clone()]));
            return Ok(());
        }

        let mut sep = true;
        let mut path_nodes: Vec<Token> = vec![start.clone()];

        loop {
            if sep {
                self.expect(TokenType::IdentPathSep)?;
                sep = false;
            } else {
                sep = true;
            }

            let cur = self.next();

            if variant_eq(cur.get_typ(), &TokenType::Ident(String::new())) {
                path_nodes.push(cur.clone());

                if let Some(peeked) = self.peek() {
                    if *peeked.get_typ() != TokenType::IdentPathSep {
                        break;
                    }
                }
            }
        }

        self.add_refered(Node::Ident(path_nodes));

        Ok(())
    }

    fn block(&mut self, start: &Token) -> Result<Node, AnaError> {
        self.block_nesting.push(0);
        self.inner_parse()?;
        _ = self.block_nesting.pop();

        let node = Node::Block(start.clone(), self.block_nodes.clone());

        self.block_nodes = Vec::new();

        Ok(node)
    }

    fn function_args(&mut self) -> Result<Vec<Token>, AnaError> {
        let mut comma = false;
        let mut arg_tokens: Vec<Token> = vec![];

        // special condition for ()
        if let TokenType::ParenRight = self.cur().get_typ() {
            self.eat();
        } else {
            loop {
                if comma {
                    self.expect(TokenType::Comma)?;
                    self.eat();
                } else {
                    self.expect(TokenType::Ident(String::new()))?;
                    comma = true;
                }

                let ident = self.next().clone();

                arg_tokens.push(ident.clone());

                let cur = self.cur();

                if *cur.get_typ() == TokenType::ParenRight {
                    self.eat();
                    break;
                }
            }
        }

        Ok(arg_tokens)
    }

    fn function(&mut self, start: &Token) -> ParserResult {
        self.expect(TokenType::ParenLeft)?;
        self.eat();

        let args = self.function_args()?;

        let block_start = self.next().clone();
        let block = self.block(&block_start)?;
        self.eat();

        self.add_refered(Node::Function {
            start: start.clone(),
            args: args,
            block: Box::new(block),
        });

        self.idx -= 1;

        Ok(())
    }

    fn r#if(&mut self, start: &Token) -> ParserResult {
        self.expect(TokenType::ParenLeft)?;
        self.eat();

        let expr = self.expr_inner(0)?;

        let bt_start = self.next().clone();

        let block_true = self.block(&bt_start)?;

        let mut block_false: Option<Box<Node>> = None;

        if let TokenType::Else = self.cur().get_typ() {
            self.eat();

            let mut do_else = true;

            if let TokenType::If = self.cur().get_typ() {
                let start = self.cur().clone();

                do_else = false;
                self.eat();

                self.r#if(&start)?;

                block_false = Some(Box::new(self.pop_node().unwrap()));
            }

            if do_else {
                self.expect(TokenType::BraceLeft)?;
                self.eat();

                let block = self.block(&self.cur().clone())?;

                block_false = Some(Box::new(block));
            }
        }

        self.add_node(Node::If {
            start: start.clone(),
            expr: Box::new(expr),
            block_true: Box::new(block_true),
            block_false: block_false,
        });

        Ok(())
    }

    fn expr_inner(&mut self, min_bp: u16) -> Result<Node, AnaError> {
        let lct = self.next().clone();

        let mut lhs = match lct.get_typ() {
            TokenType::Int(_) => Node::Int(lct),
            TokenType::Float(_) => Node::Float(lct),
            TokenType::String(_) => Node::String(lct),
            TokenType::Ident(_) => {
                self.ident_path(&lct)?;

                let cur = self.cur().clone();
                if let TokenType::BracketLeft = cur.get_typ() {
                    self.eat();
                    self.list_stmt(&cur)?;

                    self.pop_node().unwrap()
                } else {
                    self.pop_refered().unwrap()
                }
            }
            TokenType::ParenLeft => {
                let lhs = self.expr_inner(0);

                if *self.cur().get_typ() != TokenType::ParenRight {
                    return Err(token_anaerr!(
                        lct,
                        "unexpected token '{}' in expression",
                        lct.get_typ(),
                    ));
                }

                lhs?
            }
            TokenType::BracketLeft => {
                self.list(&lct)?;

                self.pop_refered().unwrap()
            }
            TokenType::Function => {
                self.function(&lct)?;

                self.pop_refered().unwrap()
            }
            _ => {
                return Err(token_anaerr!(
                    lct,
                    "unexpected token '{}' in expression",
                    lct.get_typ(),
                ));
            }
        };

        loop {
            let opt = self.next().clone();

            let op = match opt.get_typ() {
                TokenType::Eof
                | TokenType::StatementEnding
                | TokenType::Comma
                | TokenType::ParenRight
                | TokenType::BracketRight => break,

                TokenType::MethodOf
                | TokenType::Equals
                | TokenType::NotEquals
                | TokenType::LessThan
                | TokenType::GreaterThan
                | TokenType::Add
                | TokenType::Concat
                | TokenType::Subtract
                | TokenType::Multiply
                | TokenType::Divide
                | TokenType::Modulus => Node::Op(opt),

                _ => {
                    return Err(token_anaerr!(
                        opt,
                        "unexpected operator token '{}' in expression",
                        opt.get_typ(),
                    ))
                }
            };

            if let Some((l_bp, r_bp)) = infix_binding_power(op.token().get_typ()) {
                if l_bp < min_bp {
                    break;
                }

                let rhs = self.expr_inner(r_bp)?;

                lhs = Node::BinaryExpr {
                    left: Box::new(lhs),
                    op: Box::new(op),
                    right: Box::new(rhs),
                };

                if let Some(t) = self.prev() {
                    match t.get_typ() {
                        TokenType::Eof
                        | TokenType::StatementEnding
                        | TokenType::Comma
                        | TokenType::ParenRight
                        | TokenType::BracketRight => {
                            break;
                        }
                        _ => (),
                    }
                }
            }
        }

        Ok(lhs)
    }

    fn list(&mut self, start: &Token) -> ParserResult {
        let mut comma = false;
        let mut list_nodes: Vec<Node> = vec![];

        loop {
            if comma {
                self.expect(TokenType::Comma)?;
                self.eat();
            } else {
                comma = true;
            }

            list_nodes.push(self.expr_inner(0)?);
            self.idx -= 1;

            let cur = self.cur();

            if *cur.get_typ() == TokenType::BracketRight {
                self.eat();
                break;
            } else if *cur.get_typ() != TokenType::Comma {
                unreachable!("expected Comma, but found '{}' instead", cur.get_typ())
            }
        }

        self.add_refered(Node::Seq {
            start: start.clone(),
            contents: list_nodes,
        });

        Ok(())
    }

    // not actually a statement, but I couldn't think of a better name
    fn list_stmt(&mut self, start: &Token) -> ParserResult {
        let mut is_call = false;

        if let Some((last, is_deferred)) = self.last_node.last() {
            if let Node::Ident(_) = last {
                if !*is_deferred {
                    unreachable!();
                }

                is_call = true;
            }
        }

        // special condition for []
        if let TokenType::BracketRight = self.cur().get_typ() {
            self.add_refered(Node::Seq {
                start: start.clone(),
                contents: Vec::new(),
            });

            self.eat();
        } else {
            self.list(start)?;
        }

        if is_call {
            let args = self.pop_refered().unwrap();
            let name = self.pop_refered().unwrap();

            let fncall = Node::FunctionCall {
                name: Box::new(name),
                args: Box::new(args),
            };

            if let Some((n, is_deferred)) = self.last_node.last() {
                if let Node::Op(t) = n {
                    if !*is_deferred {
                        unreachable!();
                    }

                    if *t.get_typ() == TokenType::MethodOf {
                        let op = self.pop_refered().unwrap();
                        let left = self.pop_refered().unwrap();

                        self.add_node(Node::BinaryExpr {
                            left: Box::new(left),
                            op: Box::new(op),
                            right: Box::new(fncall),
                        });

                        return Ok(());
                    }
                }
            }

            self.add_node(fncall);
        }

        Ok(())
    }

    fn inner_parse(&mut self) -> ParserResult {
        loop {
            let cur = self.next().clone();

            if let TokenType::BraceRight = cur.get_typ() {
                if self.block_nesting.len() > 0 {
                    break;
                }
            }

            match cur.get_typ() {
                TokenType::Eof => break,
                TokenType::StatementEnding => self.idx -= 1,
                TokenType::Ident(ident) => {
                    match ident.as_str() {
                        _ => {
                            self.ident_path(&cur)?;
                            continue;
                        }
                    };
                }
                TokenType::Bind => {
                    let expr = self.expr_inner(0)?;
                    self.idx -= 1;
                    let name = self.pop_refered().unwrap();

                    self.add_node(Node::Binding {
                        name: Box::new(name),
                        expr: Box::new(expr),
                    });
                }
                TokenType::MethodOf => {
                    self.add_refered(Node::Op(cur));
                    continue;
                }
                TokenType::Require => self.import(&cur)?,
                TokenType::Return => {
                    self.r#return(&cur)?;
                    self.idx -= 1;
                }
                TokenType::If => {
                    self.r#if(&cur)?;
                    continue;
                }
                TokenType::BracketLeft => self.list_stmt(&cur)?,
                TokenType::BraceLeft => {
                    let block = self.block(&cur)?;

                    self.add_node(block);

                    continue;
                }
                _ => return Err(token_anaerr!(cur, "unexpected token '{}'", cur.get_typ(),)),
            }

            let t = self.next().clone();
            self.expect_t(&t, TokenType::StatementEnding)?;
        }

        Ok(())
    }

    pub fn parse(&mut self) -> Result<Vec<Node>, AnaError> {
        self.block_nesting.clear();
        self.inner_parse()?;

        if let Some(rt) = self.pop_refered() {
            Err(token_anaerr!(
                rt.token(),
                "unexpected token '{}'",
                rt.token().get_typ(),
            ))
        } else {
            Ok(self.nodes.clone())
        }
    }
}

fn infix_binding_power(op: &TokenType) -> Option<(u16, u16)> {
    match op {
        TokenType::Add | TokenType::Subtract => Some((1, 2)),
        TokenType::Multiply | TokenType::Divide | TokenType::Modulus => Some((3, 4)),
        TokenType::Concat => Some((5, 6)),
        TokenType::LessThan | TokenType::GreaterThan => Some((7, 8)),
        TokenType::Equals | TokenType::NotEquals => Some((9, 10)),
        TokenType::MethodOf => Some((20, 21)),
        _ => None,
    }
}
