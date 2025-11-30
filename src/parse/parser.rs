use miette::SourceSpan;

use crate::{
    ast::{
        BinaryOp, Block, Declaration, Expression, GlobalDeclaration, Identifier, LogicalOp,
        Program, Spanned, Statement, UnaryOp,
    },
    error::Result,
    lex::{Lexer, Token, TokenKind},
    parse::{error::SyntaxError, prec::Precedence},
};

#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current: Token<'src>,
    previous: Token<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Result<Parser<'src>> {
        Ok(Parser {
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
                return Ok(Program(program));
            }
            program.push(self.global_declaration()?);
        }
    }

    fn global_declaration(&mut self) -> Result<Spanned<GlobalDeclaration<'src>>> {
        if self.matches(TokenKind::Fn)? {
            self.function()
        } else if self.matches(TokenKind::Let)? {
            self.global_variable()
        } else {
            return Err(SyntaxError::UnexpectedToken {
                span: self.current.span,
                context: "at global level".into(),
            }
            .into());
        }
    }

    fn function(&mut self) -> Result<Spanned<GlobalDeclaration<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::Identifier, "in function declaration")?;
        let name = self.previous.into();
        self.consume(TokenKind::LeftParen, "after function name")?;
        let arguments = self.function_arguments()?;
        self.consume(TokenKind::LeftBrace, "after function declaration")?;
        let body = self.block()?;

        Ok(Spanned::new(
            GlobalDeclaration::Function {
                name,
                arguments,
                body,
            },
            self.span_from(start),
        ))
    }

    fn function_arguments(&mut self) -> Result<Vec<Identifier<'src>>> {
        let mut arguments = vec![];
        while self.matches(TokenKind::Identifier)? {
            arguments.push(self.previous.into());

            if !self.matches(TokenKind::Comma)? {
                break;
            }
        }
        self.consume(TokenKind::RightParen, "after argument list")?;
        Ok(arguments)
    }

    fn global_variable(&mut self) -> Result<Spanned<GlobalDeclaration<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::Identifier, "in global variable declaration")?;
        let name = self.previous.into();
        let value =
            if self.matches(TokenKind::Eq)? {
                self.consume(TokenKind::Number, "as global variable value")?;
                Some(self.previous.lexeme.parse::<isize>().map_err(|_| {
                    SyntaxError::InvalidNumber {
                        span: self.previous.span,
                    }
                })?)
            } else {
                None
            };
        self.consume(TokenKind::Semi, "after global variable declaration")?;

        Ok(Spanned::new(
            GlobalDeclaration::Variable { name, value },
            self.span_from(start),
        ))
    }

    fn block(&mut self) -> Result<Block<'src>> {
        let mut declarations = vec![];

        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eoi) {
            declarations.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "after block")?;
        Ok(Block(declarations))
    }

    fn declaration(&mut self) -> Result<Spanned<Declaration<'src>>> {
        if self.matches(TokenKind::Let)? {
            self.variable()
        } else {
            let stmt = self.statement()?;
            let span = stmt.span;
            Ok(Spanned::new(Declaration::Statement(stmt), span))
        }
    }

    fn variable(&mut self) -> Result<Spanned<Declaration<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::Identifier, "in variable declaration")?;
        let name = self.previous.into();
        let value = if self.matches(TokenKind::Eq)? {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenKind::Semi, "after variable declaration")?;
        Ok(Spanned::new(
            Declaration::Variable { name, value },
            self.span_from(start),
        ))
    }

    fn statement(&mut self) -> Result<Spanned<Statement<'src>>> {
        if self.matches(TokenKind::LeftBrace)? {
            let start = self.previous.span;
            let body = self.block()?;
            Ok(Spanned::new(Statement::Block(body), self.span_from(start)))
        } else if self.matches(TokenKind::If)? {
            self.if_statement()
        } else if self.matches(TokenKind::While)? {
            self.while_statement()
        } else if self.matches(TokenKind::Return)? {
            self.return_statement()
        } else {
            let start = self.current.span;
            let expression = self.expression()?;
            self.consume(TokenKind::Semi, "after expression")?;
            Ok(Spanned::new(
                Statement::Expression(expression),
                self.span_from(start),
            ))
        }
    }

    fn if_statement(&mut self) -> Result<Spanned<Statement<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::LeftParen, "in if statement")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "after condition")?;
        let then_stmt = self.statement()?;
        let else_stmt = if self.matches(TokenKind::Else)? {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Spanned::new(
            Statement::If {
                condition,
                then_stmt: Box::new(then_stmt),
                else_stmt: else_stmt.map(Box::new),
            },
            self.span_from(start),
        ))
    }

    fn while_statement(&mut self) -> Result<Spanned<Statement<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::LeftParen, "in while statement")?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "after condition")?;
        let body = self.statement()?;
        Ok(Spanned::new(
            Statement::While {
                condition,
                body: Box::new(body),
            },
            self.span_from(start),
        ))
    }

    fn return_statement(&mut self) -> Result<Spanned<Statement<'src>>> {
        let start = self.previous.span;
        let expression = if self.matches(TokenKind::Semi)? {
            None
        } else {
            let expression = self.expression()?;
            self.consume(TokenKind::Semi, "after return statement")?;
            Some(expression)
        };
        Ok(Spanned::new(
            Statement::Return(expression),
            self.span_from(start),
        ))
    }

    fn expression(&mut self) -> Result<Spanned<Expression<'src>>> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Spanned<Expression<'src>>> {
        let mut expr = self.prefix()?;

        while precedence <= self.current.kind.into() {
            expr = self.infix(expr)?;
        }

        Ok(expr)
    }

    fn infix(&mut self, left: Spanned<Expression<'src>>) -> Result<Spanned<Expression<'src>>> {
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
                span: self.current.span,
                context: "expected operator or end of expression".into(),
            }
            .into()),
        }
    }

    fn assignment(&mut self, left: Spanned<Expression<'src>>) -> Result<Spanned<Expression<'src>>> {
        match left.node {
            Expression::Identifier(target) => {
                let right = self.parse_precedence(Precedence::Assignment)?;
                let span = Self::combine_spans(left.span, right.span);
                Ok(Spanned::new(
                    Expression::Assign {
                        target,
                        value: Box::new(right),
                    },
                    span,
                ))
            }
            _ => Err(SyntaxError::InvalidAssignmentTarget { span: left.span }.into()),
        }
    }

    fn logical(&mut self, left: Spanned<Expression<'src>>) -> Result<Spanned<Expression<'src>>> {
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::from(op).next())?;
        let span = Self::combine_spans(left.span, right.span);
        Ok(Spanned::new(
            Expression::Logical {
                left: Box::new(left),
                op: match op {
                    TokenKind::Or => LogicalOp::Or,
                    TokenKind::And => LogicalOp::And,
                    _ => unreachable!(),
                },
                right: Box::new(right),
            },
            span,
        ))
    }

    fn binary(&mut self, left: Spanned<Expression<'src>>) -> Result<Spanned<Expression<'src>>> {
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::from(op).next())?;
        let span = Self::combine_spans(left.span, right.span);
        Ok(Spanned::new(
            Expression::Binary {
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
            },
            span,
        ))
    }

    fn call(&mut self, left: Spanned<Expression<'src>>) -> Result<Spanned<Expression<'src>>> {
        match left.node {
            Expression::Identifier(name) => {
                let arguments = self.call_arguments()?;
                let span = Self::combine_spans(left.span, self.previous.span);
                Ok(Spanned::new(Expression::Call { name, arguments }, span))
            }
            _ => Err(SyntaxError::InvalidCallTarget { span: left.span }.into()),
        }
    }

    fn call_arguments(&mut self) -> Result<Vec<Spanned<Expression<'src>>>> {
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

    fn prefix(&mut self) -> Result<Spanned<Expression<'src>>> {
        self.advance()?;
        match self.previous.kind {
            TokenKind::Minus | TokenKind::Bang | TokenKind::Star => self.unary(),
            TokenKind::Number => self.number(),
            TokenKind::Identifier => self.identifier(),
            TokenKind::Ampersand => self.address(),
            TokenKind::LeftParen => self.grouping(),
            _ => Err(SyntaxError::UnexpectedToken {
                span: self.previous.span,
                context: "in expression".into(),
            }
            .into()),
        }
    }

    fn unary(&mut self) -> Result<Spanned<Expression<'src>>> {
        let start = self.previous.span;
        let op = self.previous.kind;
        let right = self.parse_precedence(Precedence::Unary)?;
        let span = Self::combine_spans(start, right.span);
        Ok(Spanned::new(
            Expression::Unary {
                op: match op {
                    TokenKind::Minus => UnaryOp::Negate,
                    TokenKind::Bang => UnaryOp::LogicalNot,
                    TokenKind::Star => UnaryOp::Dereference,
                    _ => unreachable!(),
                },
                right: Box::new(right),
            },
            span,
        ))
    }

    fn number(&mut self) -> Result<Spanned<Expression<'src>>> {
        let span = self.previous.span;
        self.previous
            .lexeme
            .parse::<isize>()
            .map(|value| Spanned::new(Expression::Integer(value), span))
            .map_err(|_| SyntaxError::InvalidNumber { span }.into())
    }

    fn identifier(&mut self) -> Result<Spanned<Expression<'src>>> {
        let span = self.previous.span;
        Ok(Spanned::new(
            Expression::Identifier(self.previous.into()),
            span,
        ))
    }

    fn address(&mut self) -> Result<Spanned<Expression<'src>>> {
        let start = self.previous.span;
        self.consume(TokenKind::Identifier, "after address operator")?;
        let span = Self::combine_spans(start, self.previous.span);
        Ok(Spanned::new(
            Expression::AddressOf(self.previous.into()),
            span,
        ))
    }

    fn grouping(&mut self) -> Result<Spanned<Expression<'src>>> {
        let expression = self.expression()?;
        self.consume(TokenKind::RightParen, "after grouping")?;
        Ok(expression)
    }

    fn span_from(&self, start: SourceSpan) -> SourceSpan {
        let start_offset: usize = start.offset().into();
        let end_offset: usize = self.previous.span.offset().into();
        let end_len: usize = self.previous.span.len().into();

        SourceSpan::new(start_offset.into(), end_offset + end_len - start_offset)
    }

    fn combine_spans(left: SourceSpan, right: SourceSpan) -> SourceSpan {
        let left_offset: usize = left.offset().into();
        let right_offset: usize = right.offset().into();
        let right_len: usize = right.len().into();

        SourceSpan::new(left_offset.into(), right_offset + right_len - left_offset)
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
                span: self.current.span,
                context: context.into(),
            }
            .into())
        }
    }
}
