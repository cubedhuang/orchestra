use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    // values
    Push(isize),
    Pop,
    GetLocal(usize),
    GetLocalAddress(usize),
    SetLocal(usize),
    GetParameter(usize),
    GetParameterAddress(usize),
    SetParameter(usize),
    GetGlobal(usize),
    GetGlobalAddress(usize),
    SetGlobal(usize),

    // binary
    Add,
    Subtract,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    // unary
    Negate,
    LogicalNot,
    Dereference,

    // control flow
    Label(usize),
    Jump(usize),
    JumpIfZero(usize),
    JumpIfNotZero(usize),

    // functions
    Call(String),
    Return,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // values
            Op::Push(value) => write!(f, "Push({value})"),
            Op::Pop => write!(f, "Pop"),
            Op::GetLocal(index) => write!(f, "GetLocal({index})"),
            Op::GetLocalAddress(index) => write!(f, "GetLocalAddress({index})"),
            Op::SetLocal(index) => write!(f, "SetLocal({index})"),
            Op::GetParameter(index) => write!(f, "GetParameter({index})"),
            Op::GetParameterAddress(index) => write!(f, "GetParameterAddress({index})"),
            Op::SetParameter(index) => write!(f, "SetParameter({index})"),
            Op::GetGlobal(index) => write!(f, "GetGlobal({index})"),
            Op::GetGlobalAddress(index) => write!(f, "GetGlobalAddress({index})"),
            Op::SetGlobal(index) => write!(f, "SetGlobal({index})"),
            Op::Add => write!(f, "Add"),
            Op::Subtract => write!(f, "Subtract"),
            Op::Equal => write!(f, "Equal"),
            Op::NotEqual => write!(f, "NotEqual"),
            Op::LessThan => write!(f, "LessThan"),
            Op::LessEqual => write!(f, "LessEqual"),
            Op::GreaterThan => write!(f, "GreaterThan"),
            Op::GreaterEqual => write!(f, "GreaterEqual"),
            Op::Negate => write!(f, "Negate"),
            Op::LogicalNot => write!(f, "LogicalNot"),
            Op::Dereference => write!(f, "Dereference"),
            Op::Label(index) => write!(f, "Label({index})"),
            Op::Jump(index) => write!(f, "Jump({index})"),
            Op::JumpIfZero(index) => write!(f, "JumpIfZero({index})"),
            Op::JumpIfNotZero(index) => write!(f, "JumpIfNotZero({index})"),
            Op::Call(value) => write!(f, "Call({value})"),
            Op::Return => write!(f, "Return"),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub locals: usize,
    pub code: Vec<Op>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "=== <fn {}({}) [{}]> ===\n",
            &self.name, &self.arity, &self.locals
        )?;
        for op in &self.code {
            write!(f, "  {op}\n")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct IR {
    pub globals: Vec<isize>,
    pub functions: HashMap<String, Function>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "=== GLOBALS ===\n")?;
        for value in &self.globals {
            write!(f, "  {value}\n")?;
        }
        for (_, function) in &self.functions {
            write!(f, "{function}")?;
        }
        Ok(())
    }
}
