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

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    InfixOp { lhs: Box<Expr>, rhs: Box<Expr> },
    Assign { name: String, rhs: Box<Expr> },
    Item { name: String },
    Recipe { name: String, recipe: Recipe },
}

pub fn parser() -> impl Parser<Token, Vec<Expr>, Error = Simple<Token>> {
    use chumsky::prelude::*;

    recursive(|f| {
        let ident = select! { Token::Ident(ident) => ident }.labelled("ident");
        let item = just(Token::Keyword("item".to_owned()))
            .ignore_then(ident)
            .map(|name| Expr::Item { name })
            .then_ignore(just(Token::Ctrl(';')));
        item
    }).repeated().at_least(1)
}

// let mut lex = lexer::Token::lexer(source);
// let mut errors: Vec<ParseError> = Vec::with_capacity(2);
// let mut items = HashMap::new();
// let recipes = HashMap::new();
// let streams = HashMap::new();

// items.insert("__next".to_owned(), Item { kind: 0, module: 0 });

// while let Some(Ok(token)) = lex.next() {
//     match token {
//         Token::Item => {
//             match expect(&mut lex, Token::Ident) {
//                 Some(Ok(_)) => {},
//                 Some(Err(found)) => {
//                     errors.push(ParseError::UnexpectedToken { expected: Token::Ident, found, span: lex.span().into() });
//                     continue;
//                 },
//                 None => {
//                     errors.push(ParseError::UnexpectedToken { expected: Token::Ident, found: Token::EOF, span: lex.span().into() })
//                 }
//             }

//             let next = items.get_mut("__next").unwrap(); // we can unwrap here cause we inserted __next before the loop
//             let new_entry = *next;
//             next.kind += 1;

//             items.insert(lex.slice().to_owned(), new_entry);
//         }
//         Token::Recipe => {
//             match expect(&mut lex, Token::Ident) {
//                 Some(Ok(_)) => {},
//                 Some(Err(found)) => {
//                     errors.push(ParseError::UnexpectedToken { expected: Token::Ident, found, span: lex.span().into() });
//                     continue;
//                 },
//                 None => {
//                     errors.push(ParseError::UnexpectedToken { expected: Token::Ident, found: Token::EOF, span: lex.span().into() })
//                 }
//             }

//             let name = lex.slice();
//             lex
//         }
//         _ => {}
//     }
// }

// Ok(Self {
//     items,
//     recipes,
//     streams,
// });