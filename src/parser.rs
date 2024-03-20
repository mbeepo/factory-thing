use std::collections::HashMap;

use chumsky::{error::{self, Simple}, text::ident, Parser};
use logos::Lexer;

use crate::{lexer::{LexerError, Token}, Item, Recipe, Stream};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<logos::Span> for Span {
    fn from(value: logos::Span) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken{ expected: Token, found: Token, span: Span },
    UnknownToken(Span),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InfixOp {
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    Mul,
    Div,
    Add,
    Sub,
}

impl From<String> for InfixOp {
    fn from(value: String) -> Self {
        match value.as_ref() {
            "==" => Self::Eq,
            ">"  => Self::Gt,
            ">=" => Self::Gte,
            "<"  => Self::Lt,
            "<=" => Self::Lte,
            "*"  => Self::Mul,
            "/"  => Self::Div,
            "+"  => Self::Add,
            "-"  => Self::Sub,
            _ => unreachable!("Invalid infix op"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// <ident>
    Ident { name: String },
    /// <literal>
    Literal(Literal),
    /// <lhs> <op> <rhs>
    InfixOp { lhs: Box<Expr>, op: InfixOp, rhs: Box<Expr> },
    /// <name> = <rhs>
    Assign { name: String, rhs: Box<Expr> },
    /// item <name>
    Item { name: String },
    /// <lhs>[<portion>]
    Partial { lhs: Box<Expr>, portion: f64 },
    /// recipe <name>(<inputs>) -> <outputs> / <period>
    Recipe { name: String, inputs: Vec<Expr>, outputs: Vec<Expr>, period: Box<Expr> },
}

pub fn parser() -> impl Parser<Token, Vec<Expr>, Error = Simple<Token>> {
    use chumsky::prelude::*;

    let ident = select! { Token::Ident(name) => { dbg!(&name); name } }.labelled("ident");
    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(e) => Expr::Literal(Literal::Int(e)),
            Token::Float(e) => Expr::Literal(Literal::Float(e.parse::<f64>().unwrap())),
            Token::String(e) => Expr::Literal(Literal::String(e)),
            Token::True => Expr::Literal(Literal::Bool(true)),
            Token::False => Expr::Literal(Literal::Bool(false)),
        }
        .labelled("value");
        let atom = choice((val, ident.map(|name| Expr::Ident { name })));
        let infix = atom.then(choice((
            just(Token::InfixOp("+".to_owned()))
                .labelled("add")
                .to(InfixOp::Add),
            just(Token::InfixOp("-".to_owned()))
                .labelled("subtract")
                .to(InfixOp::Sub),
            just(Token::InfixOp("*".to_owned()))
                .labelled("multiply")
                .to(InfixOp::Mul)
        ))).then(atom).map(|t| {
            Expr::InfixOp { lhs: Box::new(t.0.0), op: t.0.1, rhs: Box::new(t.1) }
        });
        choice((infix, atom))
    });

    let item = just(Token::Keyword("item".to_owned()))
        .ignore_then(ident)
        .map(|name| Expr::Item { name: name.to_owned() })
        .then_ignore(just(Token::Ctrl(';')))
        .boxed();
    
    let items = expr.clone().map(|t| { dbg!(&t); t }).separated_by(just(Token::Ctrl(','))).map(|t| { dbg!(&t); t });
    let recipe = just(Token::Keyword("recipe".to_owned()))
        .ignore_then(ident)
        .then(items.clone().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .then_ignore(just(Token::Output))
        .then(items.clone())
        .then_ignore(just(Token::InfixOp("/".to_owned())))
        .then(expr.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .boxed()
        .map(|t| {
            dbg!(&t);
            Expr::Recipe { name: t.0.0.0, inputs: t.0.0.1, outputs: t.0.1, period: Box::new(t.1) }
        });

    item.or(recipe).repeated().at_least(1)
}