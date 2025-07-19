use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use unicode_segmentation::UnicodeSegmentation;

use crate::common::{try_index, AnaError};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Eof,
    Int(i32),
    Float(f32),
    String(String),
    Ident(String),
    StatementEnding,
    Add,
    Concat,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    Bind,
    IdentPathSep,
    MethodOf,
    Require,
    Function,
    Return,
    If,
    Else,
    Comma,
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    BraceLeft,
    BraceRight,
}

impl Into<String> for TokenType {
    fn into(self) -> String {
        let str = match self {
            Self::Int(v) => v.to_string(),
            Self::Float(v) => v.to_string(),
            Self::String(v) => format!("\"{}\"", v),
            Self::Ident(v) => v,
            _ => match self {
                Self::Int(_) => unreachable!(),
                Self::Float(_) => unreachable!(),
                Self::String(_) => unreachable!(),
                Self::Ident(_) => unreachable!(),
                Self::Eof => "Eof",
                Self::StatementEnding => ";",
                Self::Add => "+",
                Self::Concat => "⊂",
                Self::Subtract => "-",
                Self::Multiply => "*",
                Self::Divide => "/",
                Self::Modulus => "%",
                Self::Equals => "=",
                Self::NotEquals => "≠",
                Self::LessThan => "<",
                Self::GreaterThan => ">",
                Self::Bind => "←",
                Self::IdentPathSep => "→",
                Self::MethodOf => "⇒",
                Self::Require => "require",
                Self::Function => "λ",
                Self::Return => "return",
                Self::If => "if",
                Self::Else => "else",
                Self::Comma => ",",
                Self::ParenLeft => "(",
                Self::ParenRight => ")",
                Self::BracketLeft => "[",
                Self::BracketRight => "]",
                Self::BraceLeft => "(",
                Self::BraceRight => ")",
            }
            .to_string(),
        };

        str.to_string()
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym: String = self.clone().into();
        write!(f, "{}", sym)
    }
}

#[derive(Clone)]
pub struct Token {
    typ: TokenType,
    col: usize,
    ln: usize,
}

impl Token {
    pub fn new(typ: TokenType, col: usize, ln: usize) -> Token {
        Token {
            typ: typ,
            col: col,
            ln: ln,
        }
    }

    pub fn get_typ(&self) -> &TokenType {
        &self.typ
    }

    pub fn err(&self) -> String {
        format!("Error on line {}, col {}:", self.ln, self.col)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}, {}, {}>", self.typ, self.col, self.ln)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.typ == *other
    }

    fn ne(&self, other: &TokenType) -> bool {
        !(self == other)
    }
}

#[macro_export]
macro_rules! token_err {
    ($tok:expr, $fmt:expr, $($msg:tt)*) => {
        format!("{} {}", $tok.err(), format!($fmt, $($msg)*))
    };
}

#[macro_export]
macro_rules! token_anaerr {
    ($tok:expr, $fmt:expr, $($msg:tt)*) => {
        AnaError::from(token_err!($tok, $fmt, $($msg)*))
    };
}

fn create_chartoks<'a>() -> HashMap<&'a str, TokenType> {
    HashMap::from([
        ("+", TokenType::Add),
        ("⊂", TokenType::Concat),
        ("-", TokenType::Subtract),
        ("*", TokenType::Multiply),
        ("/", TokenType::Divide),
        ("%", TokenType::Modulus),
        ("=", TokenType::Equals),
        ("≠", TokenType::NotEquals),
        ("<", TokenType::LessThan),
        (">", TokenType::GreaterThan),
        ("←", TokenType::Bind),
        ("→", TokenType::IdentPathSep),
        ("⇒", TokenType::MethodOf),
        ("λ", TokenType::Function),
        (",", TokenType::Comma),
        ("(", TokenType::ParenLeft),
        (")", TokenType::ParenRight),
        ("[", TokenType::BracketLeft),
        ("]", TokenType::BracketRight),
        ("{", TokenType::BraceLeft),
        ("}", TokenType::BraceRight),
    ])
}

pub struct Lexer<'a> {
    text: Vec<&'a str>,
    idx: usize,
    col: usize,
    ln: usize,
    chartoks: HashMap<&'a str, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a String) -> Lexer<'a> {
        let graphs: Vec<&'a str> = text.graphemes(true).collect();

        Lexer {
            text: graphs,
            idx: 0,
            col: 1,
            ln: 1,
            chartoks: create_chartoks::<'a>(),
        }
    }

    fn adv(&mut self) {
        self.idx += 1;
        self.col += 1;

        if try_index(&self.text, self.idx).is_some_and(|ch| *ch == "\n") {
            self.ln += 1;
            self.col = 1;
        }
    }

    fn is_num(&self, idx: usize) -> bool {
        return try_index(&self.text, idx).is_some_and(|ch| {
            ch.len() == 1 && (ch.chars().all(|c| c.is_ascii_digit()) || *ch == "_")
        });
    }

    fn is_ident(&self) -> bool {
        return try_index(&self.text, self.idx).is_some_and(|ch| {
            ch.len() == 1 && (ch.chars().all(|c| c.is_ascii_alphanumeric()) || *ch == "_")
        });
    }

    fn collect_num(&mut self, neg: bool) -> Token {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = if neg {
            String::from("-")
        } else {
            String::new()
        };

        let mut dot = false;

        while self.is_num(self.idx) {
            if self.text[self.idx] == "." {
                if dot || !self.is_num(self.idx + 1) {
                    break;
                }

                dot = true;
            }

            str.push_str(self.text[self.idx]);
            self.adv();
        }

        let e_msg = format!("somehow got the invalid string '{}' in collect_num", str);

        let typ = if dot {
            TokenType::Float(str.parse().expect(e_msg.as_str()))
        } else {
            TokenType::Int(str.parse().expect(e_msg.as_str()))
        };

        Token::new(typ, startcol, startln)
    }

    fn collect_ident(&mut self) -> Token {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = String::new();

        while self.is_ident() {
            str.push_str(self.text[self.idx]);
            self.adv();
        }

        let tt = match str.as_str() {
            "require" => TokenType::Require,
            "return" => TokenType::Return,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            _ => TokenType::Ident(str),
        };

        Token::new(tt, startcol, startln)
    }

    fn collect_string(&mut self) -> Result<Token, AnaError> {
        let startcol = self.col;
        let startln = self.ln;

        let mut str = String::new();
        let mut escaped = false;

        self.adv();

        while let Some(ch) = try_index(&self.text, self.idx) {
            if escaped {
                str.push_str(match *ch {
                    "\\" | "\"" | "'" => *ch,
                    "n" => "\n",
                    "t" => "\t",
                    "a" => "\u{7}",
                    "0" => "\u{0}",
                    _ => {
                        return Err(token_anaerr!(
                            Token::new(TokenType::Eof, self.col, self.ln),
                            "invalid escape character '{}'",
                            ch
                        ));
                    }
                });

                escaped = false;
            } else if *ch == "\\" {
                escaped = true;
            } else if *ch == "\"" {
                break;
            } else {
                str.push_str(ch);
            }

            self.adv();
        }

        let err = token_anaerr!(
            Token::new(TokenType::Eof, self.col, self.ln),
            "unterminated string literal",
        );

        if let Some(ch) = try_index(&self.text, self.idx) {
            if *ch != "\"" {
                return Err(err);
            }
        } else {
            return Err(err);
        }

        self.adv();

        Ok(Token::new(TokenType::String(str), startcol, startln))
    }

    fn skip_line_comment(&mut self) {
        while try_index(&self.text, self.idx).is_some_and(|ch| *ch != "\n") {
            self.adv();
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, AnaError> {
        let mut tokens: Vec<Token> = Vec::new();

        let text_clone = self.text.clone();

        while let Some(ch) = try_index(&text_clone, self.idx) {
            if ch.trim().is_empty() {
                self.adv();
            } else if *ch == ";" {
                self.skip_line_comment();
            } else if self.is_num(self.idx) && *ch != "." && *ch != "_" {
                tokens.push(self.collect_num(false));
            } else if self.is_ident() {
                tokens.push(self.collect_ident());
            } else if *ch == "\"" {
                tokens.push(self.collect_string()?);
            } else if *ch == "." {
                tokens.push(Token::new(TokenType::StatementEnding, self.col, self.ln));

                self.adv();
            } else if let Some(tt) = self.chartoks.get(ch) {
                tokens.push(Token::new(tt.clone(), self.col, self.ln));

                self.adv();
            } else {
                return Err(token_anaerr!(
                    Token::new(TokenType::Eof, self.col, self.ln),
                    "invalid character '{}'",
                    ch
                ));
            }
        }

        tokens.push(Token::new(TokenType::Eof, self.col, self.ln - 1));

        Ok(tokens)
    }
}
