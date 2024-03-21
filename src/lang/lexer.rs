mod tokens;

use chumsky::{error, Parser};
pub use tokens::Token;

#[derive(Clone, Copy, Debug)]
pub enum LexerError {

}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = error::Simple<char>> {
    use chumsky::prelude::*;

    // numbers
    //  floats
    let frac = just('.').chain(text::digits(10));
    let exp = just('e')
        .or(just('E'))
        .chain(just('+').or(just('-')).or_not())
        .chain::<char, _, _>(text::digits(10));

    let float = just('-')
        .or_not()
        .chain::<char, _, _>(text::int(10))
        .chain::<char, _, _>(frac)
        .chain::<char, _, _>(exp.or_not().flatten())
        .boxed()
        .collect::<String>()
        .map(Token::Float)
        .labelled("float");

    //  integers
    let int = just('-').or_not()
        .chain::<char, _, _>(text::digits(10))
        .then_ignore(just("ms").or_not())
        .collect::<String>()
        .map(|t| t.parse::<isize>().unwrap_or(0))
        .map(|t| Token::Int(t));

    // strings
    //  the d stands for double quotes
    let d_string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>();

    //  the s stands for single quotes
    let s_string = just('\'')
        .ignore_then(filter(|c| *c != '\\' && *c != '\'').repeated())
        .then_ignore(just('\''))
        .collect::<String>();

    let string = d_string.or(s_string).map(Token::String).labelled("string");

    // operators
    let op = one_of("+-*/!=<>&|.%@$")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|t| {
            if t == "->" {
                Token::Output
            } else { 
                Token::InfixOp(t)
            }
        });

    // control characters
    let ctrl = one_of("()[]{};:,").map(|c: char| Token::Ctrl(c));

    // identifiers
    let ident = text::ident().map(|ident: String| match ident.as_ref() {
        "if"
        | "else"
        | "let"
        | "blueprint"
        | "producer"
        | "machine"
        | "recipe"
        | "product" => Token::Keyword(ident),
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(ident),
    });

    // let token = choice((float, int, string, op, ctrl, ident));
    let token = choice((float, int, string, op, ctrl, ident));
    let comment = just("//").then(take_until(just('\n'))).padded().ignored();

    token
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}