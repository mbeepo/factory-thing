use std::fmt::Display;

use chumsky::{error::Simple, Parser};

use super::lexer::Token;

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
    Assign,
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Self::Eq => "=",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Assign => "=",
        };

        write!(f, "{}", content)
    }
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
            "="  => Self::Assign,
            _ => unreachable!("Invalid infix op"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// <ident>
    Ident(String),
    /// <literal>
    Literal(Literal),
    /// <lhs> <op> <rhs>
    InfixOp { lhs: Box<Expr>, op: InfixOp, rhs: Box<Expr> },
    /// <name> = <rhs>
    Assign { name: String, rhs: Box<Expr> },
    /// product <name>
    Product { name: String },
    /// <lhs>[<portion>]
    Partial { lhs: Box<Expr>, portion: f64 },
    /// recipe <name>(<inputs>) -> <outputs> / <period>
    Recipe { name: String, inputs: Vec<Expr>, outputs: Vec<Expr>, period: Box<Expr> },
    // <name>(<args>)
    Call { lhs: Box<Expr>, args: Vec<Expr> },   
    /// <lhs>.<rhs>
    Access { lhs: Box<Expr>, rhs: String },
    /// knowledge <name>(<outputs>)
    Knowledge { name: String, outputs: Vec<String> },
    /// [<contents>]
    List { contents: Vec<Expr> },
}

pub fn parser() -> impl Parser<Token, Vec<Expr>, Error = Simple<Token>> {
    use chumsky::prelude::*;

    let ident = select! { Token::Ident(name) => name }.labelled("ident");
    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(e) => Expr::Literal(Literal::Int(e)),
            Token::Float(e) => Expr::Literal(Literal::Float(e.parse::<f64>().unwrap())),
            Token::String(e) => Expr::Literal(Literal::String(e)),
            Token::True => Expr::Literal(Literal::Bool(true)),
            Token::False => Expr::Literal(Literal::Bool(false)),
        }.labelled("value");

        let atom = choice((val, ident.map(Expr::Ident), expr.clone().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))));
        let products = expr.clone().separated_by(just(Token::Ctrl(',')));
        let access = atom.clone().then_ignore(just(Token::InfixOp(".".to_owned()))).then(ident).map(|(lhs, rhs)| { Expr::Access { lhs: Box::new(lhs), rhs }});
        let call = choice((access.clone(), atom.clone())).then(products.clone().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))).map(|(lhs, args)| { Expr::Call { lhs: Box::new(lhs), args } });
        let list = products.clone().delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']'))).map(|contents| Expr::List { contents });
        let op_arg = choice((call.clone(), access.clone(), atom.clone(), list.clone()));

        let infix = op_arg.clone().then(choice((
            just(Token::InfixOp("*".to_owned()))
                .labelled("multiply")
                .to(InfixOp::Mul),
            just(Token::InfixOp("+".to_owned()))
                .labelled("add")
                .to(InfixOp::Add),
            just(Token::InfixOp("-".to_owned()))
                .labelled("subtract")
                .to(InfixOp::Sub),
            just(Token::InfixOp("=".to_owned()))
                .labelled("assign")
                .to(InfixOp::Assign),
        )).then(op_arg.clone()).repeated()).foldl(|lhs, (op, rhs)| {
            Expr::InfixOp { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
        });
        
        choice((infix, call, list, access, atom))
    });

    let product = just(Token::Keyword("product".to_owned()))
        .ignore_then(ident)
        .map(|name| Expr::Product { name: name.to_owned() })
        .boxed();
    
    let products = expr.clone().separated_by(just(Token::Ctrl(',')));
    let recipe = just(Token::Keyword("recipe".to_owned())).ignore_then(ident)
        .then(products.clone().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .then_ignore(just(Token::Output))
        .then(products.clone().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))).or(products.clone()))
        .then_ignore(just(Token::InfixOp("/".to_owned())))
        .then(expr.clone())
        .boxed()
        .map(|(((name, inputs), outputs), period)| {
            Expr::Recipe { name, inputs, outputs, period: Box::new(period) }
        });
    let knowledge = just(Token::Keyword("knowledge".to_owned())).ignore_then(ident.clone())
        .then(ident.separated_by(just(Token::Ctrl(','))).allow_trailing().delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
        .map(|(name, outputs)| {
            Expr::Knowledge { name, outputs }
        });

    let stream = just(Token::Keyword("let".to_owned()))
        .ignore_then(ident)
        .then_ignore(just(Token::InfixOp("=".to_owned())))
        .then(expr.clone())
        .map(|(name, rhs)| {
            Expr::Assign { name, rhs: Box::new(rhs) }
        });

    choice((product, recipe, stream, expr, knowledge)).then_ignore(just(Token::Ctrl(';'))).repeated().at_least(1)
}