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
    Ident(Token),
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
            Self::Ident(t) => t,
            Self::Op(t) => t,
            Self::BinaryExpr { left: n, .. } => n.token(),
            Self::Seq { start: t, .. } => t,
            Self::Binding { name: n, .. } => n.token(),
            Self::Require((t, _)) => t,
            Self::FunctionCall { name, .. } => name.token(),
            Self::Block(t, _) => t,
            Self::Function { start: t, .. } => t,
            Self::Return((t, _)) => t,
            Self::If { start: t, .. } => t,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Int(_) | Self::Float(_) | Self::String(_) | Self::Ident(_) | Self::Op(_) => true,
            _ => false,
        }
    }

    pub fn is_composite(&self) -> bool {
        !self.is_primitive()
    }

    pub fn formt(&self, indent: usize) -> String {
        let fmt = match self {
            Self::Int(_) | Self::Float(_) | Self::String(_) | Self::Ident(_) | Self::Op(_) => {
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
    block_nodes: Vec<Vec<Node>>,
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
        self.prev_by(1)
    }

    fn prev_by(&self, offset: usize) -> Option<&Token> {
        if offset < self.idx {
            Some(&self.tokens[self.idx - offset])
        } else {
            None
        }
    }

    fn add_node(&mut self, node: Node) {
        if self.block_nodes.len() > 0 {
            let len = self.block_nodes.len();
            self.block_nodes[len - 1].push(node.clone());
        } else {
            self.nodes.push(node.clone());
        }

        self.last_node.push((node, false));
    }

    fn pop_node(&mut self) -> Option<Node> {
        _ = self.last_node.pop();

        if self.block_nodes.len() > 0 {
            let len = self.block_nodes.len();
            self.block_nodes[len - 1].pop()
        } else {
            self.nodes.pop()
        }
    }

    fn nodes_len(&self) -> usize {
        if self.block_nodes.len() > 0 {
            let len = self.block_nodes.len();
            self.block_nodes[len - 1].len()
        } else {
            self.nodes.len()
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
        self.expect_t(self.cur(), tt)
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

        if let Some(tok) = self.prev() {
            if *tok.get_typ() == TokenType::StatementEnding {
                self.idx -= 1;
            }
        }

        Ok(())
    }

    fn block(&mut self, start: &Token) -> Result<Node, AnaError> {
        self.block_nodes.push(Vec::new());
        self.parse_inner()?;

        self.idx -= 1;
        self.expect(TokenType::BraceRight)?;
        self.eat();

        let inner = self.block_nodes.pop().unwrap();

        Ok(Node::Block(start.clone(), inner))
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

    fn function(&mut self, start: &Token) -> Result<Node, AnaError> {
        self.expect(TokenType::ParenLeft)?;
        self.eat();

        let args = self.function_args()?;

        let block_start = self.next().clone();
        let block = self.block(&block_start)?;

        Ok(Node::Function {
            start: start.clone(),
            args: args,
            block: Box::new(block),
        })
    }

    fn r#if(&mut self, start: &Token) -> Result<Node, AnaError> {
        self.expect(TokenType::ParenLeft)?;
        self.eat();

        let expr = self.expr_inner(0)?;

        if let Some(tok) = self.prev() {
            if *tok.get_typ() == TokenType::ParenRight {
                self.idx -= 1;
            }
        }

        self.expect(TokenType::ParenRight)?;
        self.eat();

        self.expect(TokenType::BraceLeft)?;

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

                let subif = self.r#if(&start)?;

                block_false = Some(Box::new(subif));
            }

            if do_else {
                self.expect(TokenType::BraceLeft)?;
                self.eat();

                let block = self.block(&self.cur().clone())?;

                block_false = Some(Box::new(block));
            }
        }

        Ok(Node::If {
            start: start.clone(),
            expr: Box::new(expr),
            block_true: Box::new(block_true),
            block_false: block_false,
        })
    }

    fn expr_inner(&mut self, min_bp: u16) -> Result<Node, AnaError> {
        let lct = self.next().clone();

        let mut lhs = match lct.get_typ() {
            TokenType::Int(_) => Node::Int(lct),
            TokenType::Float(_) => Node::Float(lct),
            TokenType::String(_) => Node::String(lct),
            TokenType::Ident(_) => Node::Ident(lct),
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
            TokenType::BracketLeft => self.list(&lct)?,
            TokenType::Function => self.function(&lct)?,
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
                | TokenType::Bind
                | TokenType::Comma
                | TokenType::ParenRight
                | TokenType::BracketRight => break,

                TokenType::MethodOf
                | TokenType::FieldOf
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

                TokenType::BracketLeft => {
                    return self.list_stmt(&opt, Some(lhs.clone()));
                }

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

    fn list(&mut self, start: &Token) -> Result<Node, AnaError> {
        let mut comma = false;
        let mut list_nodes: Vec<Node> = vec![];

        if *self.cur().get_typ() == TokenType::BracketRight {
            return Ok(Node::Seq {
                start: start.clone(),
                contents: list_nodes,
            });
        }

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

        Ok(Node::Seq {
            start: start.clone(),
            contents: list_nodes,
        })
    }

    // not actually a statement, but I couldn't think of a better name
    fn list_stmt(&mut self, start: &Token, call_expr: Option<Node>) -> Result<Node, AnaError> {
        let seq = self.list(start)?;

        if let Some(name) = call_expr {
            Ok(Node::FunctionCall {
                name: Box::new(name),
                args: Box::new(seq),
            })
        } else {
            Ok(seq)
        }
    }

    fn bind(&mut self, name: Node) -> ParserResult {
        let expr = self.expr_inner(0)?;

        if let Some(tok) = self.prev() {
            if *tok.get_typ() == TokenType::StatementEnding {
                self.idx -= 1;
            }
        }

        self.add_node(Node::Binding {
            name: Box::new(name),
            expr: Box::new(expr),
        });

        Ok(())
    }

    fn parse_inner(&mut self) -> ParserResult {
        loop {
            let cur = self.next().clone();

            if *cur.get_typ() == TokenType::BraceRight {
                if self.block_nodes.len() > 0 {
                    break;
                }
            }

            match cur.get_typ() {
                TokenType::Eof => break,

                TokenType::StatementEnding => self.idx -= 1,

                TokenType::Ident(_) => {
                    let tok = self.cur();

                    match tok.get_typ() {
                        TokenType::FieldOf => {
                            self.idx -= 1;
                            let expr = self.expr_inner(0)?;

                            if *self.cur().get_typ() == TokenType::Bind {
                                return Err(token_anaerr!(
                                    self.cur(),
                                    "{}",
                                    "cannot assign to composite identifier"
                                ));
                            } else {
                                self.add_node(expr);
                            }
                        }
                        TokenType::Bind => {
                            self.eat();
                            self.bind(Node::Ident(cur))?
                        }
                        _ => self.add_node(Node::Ident(cur)),
                    }
                }

                TokenType::Int(_)
                | TokenType::Float(_)
                | TokenType::String(_)
                | TokenType::ParenLeft
                | TokenType::BracketLeft
                | TokenType::Function => {
                    self.idx -= 1;
                    let expr = self.expr_inner(0)?;

                    self.add_node(expr);
                }

                TokenType::MethodOf => {
                    self.add_refered(Node::Op(cur));
                    continue;
                }

                TokenType::Require => self.import(&cur)?,

                TokenType::Return => self.r#return(&cur)?,

                TokenType::If => {
                    let ifstmt = self.r#if(&cur)?;

                    self.add_node(ifstmt);

                    continue;
                }

                TokenType::BraceLeft => {
                    self.eat();

                    let block = self.block(&cur)?;

                    self.add_node(block);

                    continue;
                }

                _ => return Err(token_anaerr!(cur, "unexpected token '{}'", cur.get_typ(),)),
            }

            self.expect(TokenType::StatementEnding)?;
            self.eat();
        }

        Ok(())
    }

    pub fn parse(&mut self) -> Result<Vec<Node>, AnaError> {
        self.block_nodes.clear();
        self.parse_inner()?;

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
        TokenType::FieldOf => Some((18, 19)),
        TokenType::MethodOf => Some((20, 21)),
        _ => None,
    }
}
