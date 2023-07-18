use std::{iter::Peekable, str::Chars};

/// Almost the same as the one in the tutorial, but with a few changes:
/// - We store the identifier and num in the enum itself, instead of using a static
/// - No more `eof` token, since we're using an iterator which can just return `None`
/// - We encode the `char` in the tokens
#[derive(Debug, PartialEq)]
pub enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    Char(char),
    If,
    Then,
    Else,
    For,
    In,
    Binary,
    Unary,
    Var,
}

pub struct Lexer<'a> {
    char_iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            char_iter: source.chars().peekable(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.char_iter.peek() {
            Some(c) if c.is_whitespace() => {
                self.char_iter.next();
                self.next()
            }
            // [\p{Alphabetic}][\p{Alphanumeric}]*
            Some(c) if c.is_alphabetic() => {
                let mut identifier = String::new();
                while let Some(c) = self.char_iter.peek() {
                    if c.is_alphanumeric() {
                        identifier.push(*c);
                        self.char_iter.next();
                    } else {
                        break;
                    }
                }
                match identifier.as_str() {
                    "def" => Some(Token::Def),
                    "extern" => Some(Token::Extern),
                    "if" => Some(Token::If),
                    "then" => Some(Token::Then),
                    "else" => Some(Token::Else),
                    "for" => Some(Token::For),
                    "in" => Some(Token::In),
                    "binary" => Some(Token::Binary),
                    "unary" => Some(Token::Unary),
                    "var" => Some(Token::Var),
                    _ => Some(Token::Identifier(identifier)),
                }
            }
            // [0-9.]+
            Some(c) if c.is_numeric() || *c == '.' => {
                let mut num_str = String::new();
                while let Some(c) = self.char_iter.peek() {
                    if c.is_numeric() || *c == '.' {
                        num_str.push(*c);
                        self.char_iter.next();
                    } else {
                        break;
                    }
                }
                Some(Token::Number(num_str.parse::<f64>().unwrap()))
            }
            Some(c) if *c == '#' => {
                while let Some(c) = self.char_iter.peek() {
                    if *c == '\n' {
                        break;
                    } else {
                        self.char_iter.next();
                    }
                }
                self.next()
            }
            Some(c) => {
                let c = *c;
                self.char_iter.next();
                Some(Token::Char(c))
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fib() {
        let input = "# Compute the x'th fibonacci number.
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2)

# This expression will compute the 40th number.
fib(40)";
        let tokens: Vec<Token> = Lexer::new(input).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Def,
                Token::Identifier("fib".to_string()),
                Token::Char('('),
                Token::Identifier("x".to_string()),
                Token::Char(')'),
                Token::Identifier("if".to_string()),
                Token::Identifier("x".to_string()),
                Token::Char('<'),
                Token::Number(3.0),
                Token::Identifier("then".to_string()),
                Token::Number(1.0),
                Token::Identifier("else".to_string()),
                Token::Identifier("fib".to_string()),
                Token::Char('('),
                Token::Identifier("x".to_string()),
                Token::Char('-'),
                Token::Number(1.0),
                Token::Char(')'),
                Token::Char('+'),
                Token::Identifier("fib".to_string()),
                Token::Char('('),
                Token::Identifier("x".to_string()),
                Token::Char('-'),
                Token::Number(2.0),
                Token::Char(')'),
                Token::Identifier("fib".to_string()),
                Token::Char('('),
                Token::Number(40.0),
                Token::Char(')'),
            ]
        );
    }

    #[test]
    fn test_extern() {
        let input = "extern sin(arg);
extern cos(arg);
extern atan2(arg1 arg2);

atan2(sin(.4), cos(42))";
        let tokens: Vec<Token> = Lexer::new(input).collect();

        assert_eq!(
            tokens,
            vec![
                Token::Extern,
                Token::Identifier("sin".to_string()),
                Token::Char('('),
                Token::Identifier("arg".to_string()),
                Token::Char(')'),
                Token::Char(';'),
                Token::Extern,
                Token::Identifier("cos".to_string()),
                Token::Char('('),
                Token::Identifier("arg".to_string()),
                Token::Char(')'),
                Token::Char(';'),
                Token::Extern,
                Token::Identifier("atan2".to_string()),
                Token::Char('('),
                Token::Identifier("arg1".to_string()),
                Token::Identifier("arg2".to_string()),
                Token::Char(')'),
                Token::Char(';'),
                Token::Identifier("atan2".to_string()),
                Token::Char('('),
                Token::Identifier("sin".to_string()),
                Token::Char('('),
                Token::Number(0.4),
                Token::Char(')'),
                Token::Char(','),
                Token::Identifier("cos".to_string()),
                Token::Char('('),
                Token::Number(42.0),
                Token::Char(')'),
                Token::Char(')'),
            ]
        );
    }
}
