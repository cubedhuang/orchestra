use std::fmt::Display;

use itertools::Itertools;
use miette::SourceSpan;

use crate::lex::Token;

fn indent(s: &str) -> String {
    let prefix = "    ";
    s.lines()
        .map(|line| {
            if line.is_empty() {
                line.to_string()
            } else {
                format!("{prefix}{line}")
            }
        })
        .join("\n")
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: SourceSpan) -> Self {
        Self { node, span }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub span: SourceSpan,
}

impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'src> From<Token<'src>> for Identifier<'src> {
    fn from(value: Token<'src>) -> Self {
        Self {
            name: value.lexeme,
            span: value.span,
        }
    }
}

#[derive(Debug)]
pub struct Program<'src>(pub Vec<Spanned<GlobalDeclaration<'src>>>);

impl Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join("\n"))
    }
}

#[derive(Debug)]
pub struct Block<'src>(pub Vec<Spanned<Declaration<'src>>>);

impl Display for Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join("\n"))
    }
}

#[derive(Debug)]
pub enum GlobalDeclaration<'src> {
    Variable {
        name: Identifier<'src>,
        value: Option<isize>,
    },
    Function {
        name: Identifier<'src>,
        arguments: Vec<Identifier<'src>>,
        body: Block<'src>,
    },
}

impl Display for GlobalDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalDeclaration::Variable { name, value } => {
                if let Some(value) = value {
                    write!(f, "global {name} = {value}")
                } else {
                    write!(f, "global {name}")
                }
            }
            GlobalDeclaration::Function {
                name,
                arguments,
                body,
            } => write!(
                f,
                "fn {name} ({}) {{\n{}\n}}",
                arguments.iter().join(", "),
                indent(&body.to_string())
            ),
        }
    }
}

#[derive(Debug)]
pub enum Declaration<'src> {
    Variable {
        name: Identifier<'src>,
        value: Option<Spanned<Expression<'src>>>,
    },
    Statement(Spanned<Statement<'src>>),
}

impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Variable { name, value } => {
                if let Some(value) = value {
                    write!(f, "local {name} = {value}")
                } else {
                    write!(f, "local {name}")
                }
            }
            Declaration::Statement(statement) => write!(f, "{statement}"),
        }
    }
}

#[derive(Debug)]
pub enum Statement<'src> {
    Block(Block<'src>),
    If {
        condition: Spanned<Expression<'src>>,
        then_stmt: Box<Spanned<Statement<'src>>>,
        else_stmt: Option<Box<Spanned<Statement<'src>>>>,
    },
    While {
        condition: Spanned<Expression<'src>>,
        body: Box<Spanned<Statement<'src>>>,
    },
    Return(Option<Spanned<Expression<'src>>>),
    Expression(Spanned<Expression<'src>>),
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Block(block) => write!(f, "{block}"),
            Statement::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                if let Some(else_stmt) = else_stmt {
                    write!(
                        f,
                        "if {condition} {{\n{}\n}} else {{\n{}\n}}",
                        indent(&then_stmt.to_string()),
                        indent(&else_stmt.to_string()),
                    )
                } else {
                    write!(
                        f,
                        "if {condition} {{\n{}\n}}",
                        indent(&then_stmt.to_string())
                    )
                }
            }
            Statement::While { condition, body } => {
                write!(f, "while {condition} {{\n{}\n}}", indent(&body.to_string()))
            }
            Statement::Return(expression) => {
                if let Some(expression) = expression {
                    write!(f, "return {expression}")
                } else {
                    write!(f, "return")
                }
            }
            Statement::Expression(expression) => write!(f, "{expression}"),
        }
    }
}

#[derive(Debug)]
pub enum Expression<'src> {
    Assign {
        target: Identifier<'src>,
        value: Box<Spanned<Expression<'src>>>,
    },
    Logical {
        left: Box<Spanned<Expression<'src>>>,
        op: LogicalOp,
        right: Box<Spanned<Expression<'src>>>,
    },
    Binary {
        left: Box<Spanned<Expression<'src>>>,
        op: BinaryOp,
        right: Box<Spanned<Expression<'src>>>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Spanned<Expression<'src>>>,
    },

    Integer(isize),
    Identifier(Identifier<'src>),
    AddressOf(Identifier<'src>),
    Call {
        name: Identifier<'src>,
        arguments: Vec<Spanned<Expression<'src>>>,
    },
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Assign { target, value } => write!(f, "({target} = {value})"),
            Expression::Logical { left, op, right } => write!(f, "({left} {op} {right})"),
            Expression::Binary { left, op, right } => write!(f, "({left} {op} {right})"),
            Expression::Unary { op, right } => write!(f, "({op} {right})"),
            Expression::Integer(value) => write!(f, "{value}"),
            Expression::Identifier(identifier) => write!(f, "{identifier}"),
            Expression::AddressOf(identifier) => write!(f, "(&{identifier})"),
            Expression::Call { name, arguments } => {
                write!(f, "({name} ({}))", arguments.iter().join(", "))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::And => write!(f, "and"),
            LogicalOp::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,

    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),

            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
    Dereference,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Dereference => write!(f, "*"),
        }
    }
}
