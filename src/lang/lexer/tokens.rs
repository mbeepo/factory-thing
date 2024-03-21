use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // ===== general =====
    Ident(String),
    Ctrl(char),
    // Assign,
    // Semicolon,
    // Comma,
    // Dot,
    Whitespace,
    Comment,
    Output,
	EOF,

    // ===== keywords =====
    Keyword(String),
    // Let,
    // Blueprint,
    // Producer,
    // Machine,
    // Recipe,
    // Product,
    InfixOp(String),

    // ===== literal =====
    String(String),
    Int(isize),
    Float(String),
    True,
    False,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", String::from(self))
	}
}

impl From<&Token> for String {
	fn from(other: &Token) -> String {
        let out = match other {
            Token::Ctrl(c) => Some(format!("Ctrl({c})")),
            Token::Keyword(c) => Some(format!("Keyword({c})")),
            Token::InfixOp(c) => Some(format!("InfixOp({c})")),
            _ => None,
        };

        if let Some(out) = out {
            out
        } else {
            let out = match other {
                Token::Ident(_) => "Ident",
                Token::Whitespace => "",
                Token::Comment => "Comment",
                Token::Output => "Output",
                Token::EOF => "EOF",
                // Token::Let => "let",
                // Token::Blueprint => "blueprint",
                // Token::Producer => "producer",
                // Token::Machine => "machine",
                // Token::Recipe => "recipe",
                // Token::Product => "product",
                Token::String(_) => "String",
                Token::Int(_) => "Int",
                Token::Float(_) => "Float",
                Token::True => "True",
                Token::False => "False",
                _ => "Epic to_string fail"
            };

            out.to_owned()
        }
	}
}

impl AsRef<Token> for Token {
	fn as_ref(&self) -> &Token {
		&self
	}
}