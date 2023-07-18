use std::{cell::RefCell, collections::HashMap, iter::Peekable, rc::Rc, sync::atomic::AtomicUsize};

use crate::{
    ast::{AstNode, ExprAst, FunctionAst, Operator, OperatorType, PrototypeAst},
    lexer::{Lexer, Token},
};

// fn binop_precedence(op: char) -> Option<i32> {
//     match op {
//         '<' => Some(10),
//         '+' => Some(20),
//         '-' => Some(20),
//         '*' => Some(40),
//         _ => None,
//     }
// }

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(String),
    Other(String),
}

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    binary_op_precedence: Rc<RefCell<HashMap<char, i32>>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, binary_op_precedence: Rc<RefCell<HashMap<char, i32>>>) -> Self {
        Parser {
            lexer: Lexer::new(source).peekable(),
            binary_op_precedence,
        }
    }
}

macro_rules! parse_expect {
    (
        $self:ident,
        $token:pat => $expr:expr
    ) => {
        match $self.lexer.next() {
            Some($token) => Ok($expr),
            _ => Err(ParserError::ExpectedToken(stringify!($token).to_string())),
        }
    };
}

impl Parser<'_> {
    /// numberexpr ::= number
    fn parse_number_expr(&mut self) -> ParserResult<ExprAst> {
        let value = parse_expect!(self, Token::Number(value) => value)?;
        Ok(ExprAst::new_number(value))
    }

    /// parenexpr ::= '(' expression ')'
    fn parse_paren_expr(&mut self) -> ParserResult<ExprAst> {
        parse_expect!(self, Token::Char('(') => ())?;

        let expr = self.parse_expr()?;

        parse_expect!(self, Token::Char(')') => ())?;

        Ok(expr)
    }

    /// identifierexpr
    ///   ::= identifier
    ///   ::= identifier '(' expression* ')'
    fn parse_identifier_expr(&mut self) -> ParserResult<ExprAst> {
        let name = parse_expect!(self, Token::Identifier(name) => name)?;

        if self.lexer.peek() != Some(&Token::Char('(')) {
            Ok(ExprAst::new_variable(name))
        } else {
            parse_expect!(self, Token::Char('(') => ())?;

            let mut args = Vec::new();

            while self.lexer.peek() != Some(&Token::Char(')')) {
                args.push(self.parse_expr()?);

                if self.lexer.peek() != Some(&Token::Char(')')) {
                    parse_expect!(self, Token::Char(',') => ())?;
                }
            }

            parse_expect!(self, Token::Char(')') => ())?;

            Ok(ExprAst::new_call_expr(name, args))
        }
    }

    /// ifexpr ::= 'if' expression 'then' expression 'else' expression
    fn parse_if_expr(&mut self) -> ParserResult<ExprAst> {
        parse_expect!(self, Token::If => ())?;

        let cond = self.parse_expr()?;

        parse_expect!(self, Token::Then => ())?;

        let then = self.parse_expr()?;

        parse_expect!(self, Token::Else => ())?;

        let else_ = self.parse_expr()?;

        Ok(ExprAst::new_if_expr(cond, then, else_))
    }

    /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
    fn parse_for_expr(&mut self) -> ParserResult<ExprAst> {
        parse_expect!(self, Token::For => ())?;

        let var_name = parse_expect!(self, Token::Identifier(name) => name)?;

        parse_expect!(self, Token::Char('=') => ())?;

        let start = self.parse_expr()?;

        parse_expect!(self, Token::Char(',') => ())?;

        let end = self.parse_expr()?;

        let step = match self.lexer.peek() {
            Some(Token::Char(',')) => {
                self.lexer.next();
                Some(self.parse_expr()?)
            }
            _ => None,
        };

        parse_expect!(self, Token::In => ())?;

        let body = self.parse_expr()?;

        Ok(ExprAst::new_for_expr(var_name, start, end, step, body))
    }

    fn parse_var_expr(&mut self) -> ParserResult<ExprAst> {
        parse_expect!(self, Token::Var => ())?;

        let mut var_names = Vec::new();

        loop {
            let name = parse_expect!(self, Token::Identifier(name) => name.to_owned())?;

            // if next token is `=` then we have an initializer
            let init = match self.lexer.peek() {
                Some(Token::Char('=')) => {
                    self.lexer.next();
                    self.parse_expr()?
                }
                _ => ExprAst::new_number(0.0),
            };

            var_names.push((name, init));

            match self.lexer.peek() {
                Some(Token::Char(',')) => {
                    self.lexer.next();
                }
                _ => break,
            }
        }

        parse_expect!(self, Token::In => ())?;

        let body = self.parse_expr()?;

        Ok(ExprAst::new_var_expr(var_names, body))
    }

    /// primary
    ///   ::= identifierexpr
    ///   ::= numberexpr
    ///   ::= parenexpr
    ///   ::= ifexpr
    ///   ::= forexpr
    ///   ::= varexpr
    fn parse_primary(&mut self) -> ParserResult<ExprAst> {
        match self.lexer.peek() {
            Some(Token::Number(_)) => self.parse_number_expr(),
            Some(Token::Char('(')) => self.parse_paren_expr(),
            Some(Token::Identifier(_)) => self.parse_identifier_expr(),
            Some(Token::If) => self.parse_if_expr(),
            Some(Token::For) => self.parse_for_expr(),
            Some(Token::Var) => self.parse_var_expr(),
            _ => Err(ParserError::ExpectedToken(
                "number, identifier or paren".to_string(),
            )),
        }
    }

    /// expression
    ///   ::= primary binoprhs
    ///
    fn parse_expr(&mut self) -> ParserResult<ExprAst> {
        let lhs = self.parse_unary_expr()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    /// binoprhs
    ///   ::= ('+' primary)*
    fn parse_bin_op_rhs(&mut self, expr_prec: i32, mut lhs: ExprAst) -> ParserResult<ExprAst> {
        loop {
            let prec = match self.lexer.peek() {
                Some(Token::Char(op)) => self.binary_op_precedence.borrow().get(op).copied(),
                _ => None,
            };

            let prec = match prec {
                Some(prec) if prec < expr_prec => return Ok(lhs),
                Some(prec) => prec,
                None => return Ok(lhs),
            };

            let bin_op = parse_expect!(self, Token::Char(op) => op)?;

            let mut rhs = self.parse_unary_expr()?;

            let next_prec = match self.lexer.peek() {
                Some(Token::Char(op)) => self.binary_op_precedence.borrow().get(op).copied(),
                _ => None,
            };

            match next_prec {
                Some(next_prec) if prec < next_prec => {
                    rhs = self.parse_bin_op_rhs(prec + 1, rhs)?;
                }
                _ => (),
            }

            lhs = ExprAst::new_binary_expr(bin_op, lhs, rhs);
        }
    }

    fn parse_unary_expr(&mut self) -> ParserResult<ExprAst> {
        match self.lexer.peek() {
            Some(Token::Char('(' | ',')) => self.parse_primary(),
            Some(Token::Char(c)) => {
                let op = *c;
                self.lexer.next();
                let operand = self.parse_unary_expr()?;
                Ok(ExprAst::new_unary_expr(op, operand))
            }
            _ => self.parse_primary(),
        }
    }

    /// prototype
    ///   ::= id '(' id* ')'
    fn parse_prototype(&mut self) -> ParserResult<PrototypeAst> {
        let (name, operator) = match self.lexer.next() {
            Some(Token::Identifier(name)) => (name, None),
            Some(Token::Unary) => {
                let op = parse_expect!(self, Token::Char(op) => op)?;
                (format!("unary{op}"), Some(Operator::new_unary(op)))
            }
            Some(Token::Binary) => {
                let op = parse_expect!(self, Token::Char(op) => op)?;

                let precedence = match self.lexer.peek() {
                    Some(Token::Number(prec)) => {
                        let prec = *prec as i32;
                        self.lexer.next();
                        prec
                    }
                    // Default to 30
                    _ => 30,
                };

                (
                    format!("binary{op}"),
                    Some(Operator::new_binary(op, precedence)),
                )
            }
            _ => return Err(ParserError::ExpectedToken("identifier".into())),
        };

        parse_expect!(self, Token::Char('(') => ())?;

        let mut args = Vec::new();

        while self.lexer.peek() != Some(&Token::Char(')')) {
            args.push(parse_expect!(self, Token::Identifier(name) => name.to_owned())?);
        }

        match operator {
            Some(Operator {
                operator_type: OperatorType::Unary,
                ..
            }) => {
                if args.len() != 1 {
                    return Err(ParserError::Other(
                        "unary operator must have one argument".into(),
                    ));
                }
            }
            Some(Operator {
                operator_type: OperatorType::Binary,
                ..
            }) => {
                if args.len() != 2 {
                    return Err(ParserError::Other(
                        "binary operator must have two arguments".into(),
                    ));
                }
            }
            _ => {}
        }

        parse_expect!(self, Token::Char(')') => ())?;

        Ok(PrototypeAst::new_operator(name, args, operator))
    }

    /// definition ::= 'def' prototype expression
    fn parse_def(&mut self) -> ParserResult<FunctionAst> {
        parse_expect!(self, Token::Def => ())?;

        let proto = self.parse_prototype()?;

        let body = self.parse_expr()?;

        Ok(FunctionAst::new(proto, body, false))
    }

    /// external ::= 'extern' prototype
    fn parse_extern(&mut self) -> ParserResult<PrototypeAst> {
        parse_expect!(self, Token::Extern => ())?;
        self.parse_prototype()
    }

    /// toplevelexpr ::= expression
    fn parse_top_level_expr(&mut self) -> ParserResult<FunctionAst> {
        static ANON_EXPR_COUNTER: AtomicUsize = AtomicUsize::new(0);

        let proto = PrototypeAst::new(
            format!(
                "__anon_expr_{}",
                ANON_EXPR_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            ),
            // "main".into(),
            Vec::new(),
        );
        let body = self.parse_expr()?;
        Ok(FunctionAst::new(proto, body, true))
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<AstNode, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.lexer.peek() {
                Some(Token::Def) => break Some(self.parse_def().map(AstNode::Function)),
                Some(Token::Extern) => break Some(self.parse_extern().map(AstNode::Prototype)),
                Some(Token::Char(c)) if c == &';' => {
                    // Consume the `;` and loop
                    self.lexer.next();
                }
                Some(_) => break Some(self.parse_top_level_expr().map(AstNode::Function)),
                None => break None,
            }
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_fib() {
//         let input = "
// def fib(x)
//   if x < 3 then
//     1
//   else
//     fib(x-1)+fib(x-2)";
//         let tokens = Parser::new(input).parse_def();

//         assert_eq!(
//             tokens,
//             vec![
//                 Token::Def,
//                 Token::Identifier("fib".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("x".to_string()),
//                 Token::Char(')'),
//                 Token::Identifier("if".to_string()),
//                 Token::Identifier("x".to_string()),
//                 Token::Char('<'),
//                 Token::Number(3.0),
//                 Token::Identifier("then".to_string()),
//                 Token::Number(1.0),
//                 Token::Identifier("else".to_string()),
//                 Token::Identifier("fib".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("x".to_string()),
//                 Token::Char('-'),
//                 Token::Number(1.0),
//                 Token::Char(')'),
//                 Token::Char('+'),
//                 Token::Identifier("fib".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("x".to_string()),
//                 Token::Char('-'),
//                 Token::Number(2.0),
//                 Token::Char(')'),
//                 Token::Identifier("fib".to_string()),
//                 Token::Char('('),
//                 Token::Number(40.0),
//                 Token::Char(')'),
//             ]
//         );
//     }

//     #[test]
//     fn test_extern() {
//         let input = "extern sin(arg);
// extern cos(arg);
// extern atan2(arg1 arg2);

// atan2(sin(.4), cos(42))";
//         let tokens: Vec<Token> = Lexer::new(input).collect();

//         assert_eq!(
//             tokens,
//             vec![
//                 Token::Extern,
//                 Token::Identifier("sin".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("arg".to_string()),
//                 Token::Char(')'),
//                 Token::Char(';'),
//                 Token::Extern,
//                 Token::Identifier("cos".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("arg".to_string()),
//                 Token::Char(')'),
//                 Token::Char(';'),
//                 Token::Extern,
//                 Token::Identifier("atan2".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("arg1".to_string()),
//                 Token::Identifier("arg2".to_string()),
//                 Token::Char(')'),
//                 Token::Char(';'),
//                 Token::Identifier("atan2".to_string()),
//                 Token::Char('('),
//                 Token::Identifier("sin".to_string()),
//                 Token::Char('('),
//                 Token::Number(0.4),
//                 Token::Char(')'),
//                 Token::Char(','),
//                 Token::Identifier("cos".to_string()),
//                 Token::Char('('),
//                 Token::Number(42.0),
//                 Token::Char(')'),
//                 Token::Char(')'),
//             ]
//         );
//     }
// }
