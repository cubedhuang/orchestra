pub type Program<'src> = Vec<GlobalDeclaration<'src>>;
pub type Block<'src> = Vec<Declaration<'src>>;

#[derive(Debug)]
pub enum GlobalDeclaration<'src> {
    Variable {
        name: &'src str,
        value: Option<isize>,
    },
    Function {
        name: &'src str,
        arguments: Vec<&'src str>,
        body: Block<'src>,
    },
}

#[derive(Debug)]
pub enum Declaration<'src> {
    Variable {
        name: &'src str,
        value: Option<Expression<'src>>,
    },
    Statement(Statement<'src>),
}

#[derive(Debug)]
pub enum Statement<'src> {
    Block(Block<'src>),
    If {
        condition: Expression<'src>,
        then_stmt: Box<Statement<'src>>,
        else_stmt: Option<Box<Statement<'src>>>,
    },
    While {
        condition: Expression<'src>,
        body: Box<Statement<'src>>,
    },
    Return(Option<Expression<'src>>),
    Expression(Expression<'src>),
}

#[derive(Debug)]
pub enum Expression<'src> {
    Assign {
        target: &'src str,
        value: Box<Expression<'src>>,
    },
    Logical {
        left: Box<Expression<'src>>,
        op: LogicalOp,
        right: Box<Expression<'src>>,
    },
    Binary {
        left: Box<Expression<'src>>,
        op: BinaryOp,
        right: Box<Expression<'src>>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expression<'src>>,
    },

    Integer(isize),
    Identifier(&'src str),
    AddressOf(&'src str),
    Call {
        name: &'src str,
        arguments: Vec<Expression<'src>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
    Dereference,
}
