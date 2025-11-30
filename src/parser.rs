use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    ast::{
        BinaryOp, Block, Declaration, Expression, GlobalDeclaration, LogicalOp, Program, Statement,
        UnaryOp,
    },
    error::Result,
    lex::{Lexer, Token, TokenKind},
};

#[derive(Error, Debug, Diagnostic)]
pub enum SyntaxError {
    #[error("expected {expected} {context}")]
    #[diagnostic(code(compile::expected_token))]
    ExpectedToken {
        expected: TokenKind,
        #[label("here")]
        span: SourceSpan,
        context: String,
    },

    #[error("unexpected token {context}")]
    #[diagnostic(code(compile::unexpected_token))]
    UnexpectedToken {
        #[label("here")]
        span: SourceSpan,
        context: String,
    },

    #[error("invalid number")]
    #[diagnostic(code(compile::invalid_number))]
    InvalidNumber {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid assignment target; target must be an identifier")]
    #[diagnostic(code(compile::invalid_assignment_target))]
    InvalidAssignmentTarget {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid call target; target must be an identifier")]
    #[diagnostic(code(compile::invalid_call_target))]
    InvalidCallTarget {
        #[label("here")]
        span: SourceSpan,
    },
}

pub fn parse<'src>(source: &'src str) -> Result<Program<'src>> {
    let mut parser = Parser::new(source)?;
    let program = parser.program()?;
    return Ok(program);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment, // =
    LogicalOr,  // or
    LogicalAnd, // and
    Equality,   // == !=
    Comparison, // < <= > >=
    Term,       // + -
    Factor,     // * / (yet unused)
    Unary,      // ! - & *
    Call,       // () []
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::LogicalOr,
            Precedence::LogicalOr => Precedence::LogicalAnd,
            Precedence::LogicalAnd => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => unreachable!(),
        }
    }
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Eq => Precedence::Assignment,
            TokenKind::Or => Precedence::LogicalOr,
            TokenKind::And => Precedence::LogicalAnd,
            TokenKind::EqEq | TokenKind::BangEq => Precedence::Equality,
            TokenKind::Less | TokenKind::LessEq | TokenKind::Greater | TokenKind::GreaterEq => {
                Precedence::Comparison
            }
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::LeftParen => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug)]
struct Parser<'src> {
    source: &'src str,
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Result<Parser<'src>> {
        Ok(Parser {
            source,
            lexer: Lexer::new(source),
            current: Token::invalid(),
            previous: Token::invalid(),
        })
    }

    pub fn program(&mut self) -> Result<Program<'src>> {
        let mut program = vec![];

        self.advance()?;

        loop {
            if self.matches(TokenKind::Eoi)? {
                return Ok(program);
            }
            program.push(self.global_declaration()?);
        }
    }

    fn global_declaration(&mut self) -> Result<GlobalDeclaration<'src>> {
        if self.matches(TokenKind::Fn)? {
            self.function()
        } else if self.matches(TokenKind::Let)? {
            self.global_variable()
        } else {
            return Err(SyntaxError::UnexpectedToken {
                span: SourceSpan::new((self.source.len() - 1).into(), 0),
                context: "at global level".into(),
            }
            .into());
        }
    }

    fn function(&mut self) -> Result<GlobalDeclaration<'src>> {
        self.consume(TokenKind::Identifier, "in function declaration")?;
        let name = self.previous.lexeme;
        self.consume(TokenKind::LeftParen, "after function name")?;
        let arguments = self.function_arguments()?;
        self.consume(TokenKind::LeftBrace, "after function declaration")?;
        let body = self.block()?;
        Ok(GlobalDeclaration::Function {
            name,
            arguments,
            body,
        })
    }

    fn function_arguments(&mut self) -> Result<Vec<&'src str>> {
        let mut arguments = vec![];
        while self.matches(TokenKind::Identifier)? {
            arguments.push(self.previous.lexeme);

            if !self.matches(TokenKind::Comma)? {
                break;
            }
        }
        self.consume(TokenKind::RightParen, "after argument list")?;
        Ok(arguments)
    }

    fn global_variable(&mut self) -> Result<GlobalDeclaration<'src>> {
        self.consume(TokenKind::Identifier, "in global variable declaration")?;
        let name = self.previous.lexeme;
        let value =
            if self.matches(TokenKind::Eq)? {
                self.consume(TokenKind::Number, "as global variable value")?;
                Some(self.previous.lexeme.parse::<isize>().map_err(|_| {
                    SyntaxError::InvalidNumber {
                        span: self.previous.into(),
                    }
                })?)
            } else {
                None
            };
        self.consume(TokenKind::Semi, "after global variable declaration")?;
        Ok(GlobalDeclaration::Variable { name, value })
    }

    fn block(&mut self) -> Result<Block<'src>> {
        let mut declarations = vec![];

        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eoi) {
            declarations.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "after block")?;
        Ok(declarations)
    }

    fn declaration(&mut self) -> Result<Declaration<'src>> {
        if self.matches(TokenKind::Let)? {
            self.variable()
        } else {
            Ok(Declaration::Statement(self.statement()?))
        }
    }

    fn variable(&mut self) -> Result<Declaration<'src>> {
        self.consume(TokenKind::Identifier, "in variable declaration")?;
        let name = self.previous.lexeme;
        let value = if self.matches(TokenKind::Eq)? {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenKind::Semi, "after variable declaration")?;
        Ok(Declaration::Variable { name, value })
    }

    fn statement(&mut self) -> Result<Statement<'src>> {
        if self.matches(TokenKind::RightBrace)? {
            Ok(Statement::Block(self.block()?))
        } else if self.matches(TokenKind::If)? {
            self.if_statement()
        } else if self.matches(TokenKind::While)? {
            self.while_statement()
        } else if self.matches(TokenKind::Return)? {
            self.return_statement()
        } else {
            let expression = self.expression()?;
            self.consume(TokenKind::Semi, "after expression")?;
            Ok(Statement::Expression(expression))
        }
    }

    fn if_statement(&mut self) -> Result<Statement<'src>> {
        self.consume(TokenKind::LeftParen, "in if statement")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "after condition")?;
        let then_stmt = self.statement()?;
        let else_stmt = if self.matches(TokenKind::Else)? {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Statement::If {
            condition,
            then_stmt: Box::new(then_stmt),
            else_stmt: else_stmt.map(Box::new),
        })
    }

    fn while_statement(&mut self) -> Result<Statement<'src>> {
        self.consume(TokenKind::LeftParen, "in while statement")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "after condition")?;
        let body = self.statement()?;
        Ok(Statement::While {
            condition,
            body: Box::new(body),
        })
    }

    fn return_statement(&mut self) -> Result<Statement<'src>> {
        let expression = if self.matches(TokenKind::Semi)? {
            None
        } else {
            let expression = self.expression()?;
            self.consume(TokenKind::Semi, "after return statement")?;
            Some(expression)
        };
        Ok(Statement::Return(expression))
    }

    fn expression(&mut self) -> Result<Expression<'src>> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expression<'src>> {
        let mut expr = self.prefix()?;

        while precedence <= self.current.kind.into() {
            expr = self.infix(expr)?;
        }

        Ok(expr)
    }

    fn infix(&mut self, left: Expression<'src>) -> Result<Expression<'src>> {
        self.advance()?;
        match self.previous.kind {
            TokenKind::Eq => self.assignment(left),
            TokenKind::And | TokenKind::Or => self.logical(left),
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Less
            | TokenKind::LessEq
            | TokenKind::Greater
            | TokenKind::GreaterEq
            | TokenKind::Plus
            | TokenKind::Minus => self.binary(left),
            TokenKind::LeftParen => self.call(left),
            _ => Err(SyntaxError::UnexpectedToken {
                span: self.current.into(),
                context: "expected operator or end of expression".into(),
            }
            .into()),
        }
    }

    fn assignment(&mut self, left: Expression<'src>) -> Result<Expression<'src>> {
        match left {
            Expression::Identifier(target) => {
                let right = self.parse_precedence(Precedence::Assignment)?;
                Ok(Expression::Assign {
                    target,
                    value: Box::new(right),
                })
            }
            _ => Err(SyntaxError::InvalidAssignmentTarget {
                span: self.previous.into(),
            }
            .into()),
        }
    }

    fn logical(&mut self, left: Expression<'src>) -> Result<Expression<'src>> {
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::from(op).next())?;
        Ok(Expression::Logical {
            left: Box::new(left),
            op: match op {
                TokenKind::Or => LogicalOp::Or,
                TokenKind::And => LogicalOp::And,
                _ => unreachable!(),
            },
            right: Box::new(right),
        })
    }

    fn binary(&mut self, left: Expression<'src>) -> Result<Expression<'src>> {
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::from(op).next())?;
        Ok(Expression::Binary {
            left: Box::new(left),
            op: match op {
                TokenKind::EqEq => BinaryOp::Equal,
                TokenKind::BangEq => BinaryOp::NotEqual,
                TokenKind::Less => BinaryOp::LessThan,
                TokenKind::LessEq => BinaryOp::LessEqual,
                TokenKind::Greater => BinaryOp::GreaterThan,
                TokenKind::GreaterEq => BinaryOp::GreaterEqual,
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            },
            right: Box::new(right),
        })
    }

    fn call(&mut self, left: Expression<'src>) -> Result<Expression<'src>> {
        match left {
            Expression::Identifier(name) => {
                let arguments = self.call_arguments()?;
                Ok(Expression::Call { name, arguments })
            }
            _ => Err(SyntaxError::InvalidCallTarget {
                span: self.previous.into(),
            }
            .into()),
        }
    }

    fn call_arguments(&mut self) -> Result<Vec<Expression<'src>>> {
        let mut arguments = vec![];
        if !self.check(TokenKind::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.matches(TokenKind::Comma)? {
                    break;
                }
            }
        }
        self.consume(TokenKind::RightParen, "after function arguments")?;
        Ok(arguments)
    }

    fn prefix(&mut self) -> Result<Expression<'src>> {
        self.advance()?;
        match self.previous.kind {
            TokenKind::Minus | TokenKind::Bang | TokenKind::Star => self.unary(),
            TokenKind::Number => self.number(),
            TokenKind::Identifier => self.identifier(),
            TokenKind::Ampersand => self.address(),
            TokenKind::LeftParen => self.grouping(),
            _ => Err(SyntaxError::UnexpectedToken {
                span: self.previous.into(),
                context: "in expression".into(),
            }
            .into()),
        }
    }

    fn unary(&mut self) -> Result<Expression<'src>> {
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expression::Unary {
            op: match op {
                TokenKind::Minus => UnaryOp::Negate,
                TokenKind::Bang => UnaryOp::Not,
                TokenKind::Star => UnaryOp::Dereference,
                _ => unreachable!(),
            },
            operand: Box::new(right),
        })
    }

    fn number(&mut self) -> Result<Expression<'src>> {
        self.previous
            .lexeme
            .parse::<isize>()
            .map(|value| Expression::Integer(value))
            .map_err(|_| {
                SyntaxError::InvalidNumber {
                    span: self.previous.into(),
                }
                .into()
            })
    }

    fn identifier(&mut self) -> Result<Expression<'src>> {
        Ok(Expression::Identifier(self.previous.lexeme))
    }

    fn address(&mut self) -> Result<Expression<'src>> {
        self.consume(TokenKind::Identifier, "after address operator")?;
        Ok(Expression::AddressOf(self.previous.lexeme))
    }

    fn grouping(&mut self) -> Result<Expression<'src>> {
        let expression = self.expression()?;
        self.consume(TokenKind::RightParen, "after grouping")?;
        Ok(expression)
    }

    fn advance(&mut self) -> Result<()> {
        self.previous = self.current;
        self.current = self.lexer.scan_token()?;
        Ok(())
    }

    fn matches(&mut self, kind: TokenKind) -> Result<bool> {
        if !self.check(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn consume(&mut self, kind: TokenKind, context: &str) -> Result<()> {
        if self.current.kind == kind {
            self.advance()
        } else {
            Err(SyntaxError::ExpectedToken {
                expected: kind,
                span: self.current.into(),
                context: context.into(),
            }
            .into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bad_global_declarations() {
        let sources = ["let f =", "fn a() {", "fn ()"];
        for source in sources {
            assert!(parse(source).is_err());
        }
    }

    #[test]
    fn good_global_declarations() {
        let sources = ["let a = 10;", "let a;", "fn a() {}", "fn f(a, b) {}"];
        for source in sources {
            assert!(parse(source).is_ok());
        }
    }

    #[test]
    fn expressions() {
        let sources = [
            "fn main() { x = 5; }",
            "fn main() { x = y = 5; }",
            "fn main() { a + b - c; }",
            "fn main() { !x; }",
            "fn main() { -5; }",
            "fn main() { *p; }",
            "fn main() { &x; }",
            "fn main() { foo(1, 2, 3); }",
            "fn main() { x and y or z; }",
            "fn main() { (a + b) - c; }",
            "fn main() { *&x; }",
        ];

        for source in sources {
            println!("Parsing: {}", source);
            let result = parse(source);
            if let Err(e) = &result {
                println!("Error: {:?}", e);
            }
            assert!(result.is_ok());
        }
    }

    #[test]
    fn bad_expressions() {
        let sources = [
            "fn main() { &(x + 1); }",
            "fn main() { (x + y) = 5; }",
            "fn main() { 5(); }",
        ];

        for source in sources {
            println!("Should fail: {}", source);
            assert!(parse(source).is_err());
        }
    }
}
