use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOp, Block, Declaration, Expression, GlobalDeclaration, Identifier, LogicalOp,
        Program, Statement, UnaryOp,
    },
    compile::error::CompileError,
    error::Result,
    ir::{Function, IR, Op},
};

enum IdentifierKind {
    Function,
    Global(usize),
    Parameter(usize),
    Local(usize),
}

#[derive(Default)]
struct FunctionCompiler {
    name: String,
    parameters: Vec<String>,
    max_locals: usize,
    code: Vec<Op>,

    scopes: Vec<HashMap<String, usize>>,
    current_locals: usize,
    next_label: usize,
}

impl FunctionCompiler {
    fn new(name: String, parameters: Vec<String>) -> Self {
        let mut param_map = HashMap::new();
        for (i, param) in parameters.iter().enumerate() {
            param_map.insert(param.clone(), i);
        }

        Self {
            name,
            parameters,
            max_locals: 0,
            code: vec![],
            scopes: vec![param_map],
            current_locals: 0,
            next_label: 0,
        }
    }

    fn end(self) -> Function {
        Function {
            name: self.name,
            arity: self.parameters.len(),
            locals: self.max_locals,
            code: self.code,
        }
    }

    fn declare_local(&mut self, name: &Identifier) -> Result<usize> {
        if self.scopes.last().unwrap().contains_key(name.name) {
            return Err(CompileError::DuplicateIdentifier {
                identifier: name.name.into(),
                span: name.span,
            }
            .into());
        }

        let slot = self.current_locals;
        self.current_locals += 1;
        self.max_locals = self.max_locals.max(self.current_locals);

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), slot);
        Ok(slot)
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().skip(1).rev() {
            if let Some(&slot) = scope.get(name) {
                return Some(slot);
            }
        }
        None
    }

    fn resolve_parameter(&self, name: &str) -> Option<usize> {
        self.scopes
            .first()
            .and_then(|scope| scope.get(name).copied())
    }
}

pub struct Compiler {
    globals: Vec<isize>,
    global_map: HashMap<String, usize>,
    functions: HashMap<String, Function>,
    current_function: FunctionCompiler,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            global_map: HashMap::new(),
            functions: HashMap::new(),
            current_function: FunctionCompiler::default(),
        }
    }

    pub fn register_globals(&mut self, program: &Program) -> Result<()> {
        for global_declaration in &program.0 {
            match &global_declaration.node {
                GlobalDeclaration::Variable { name: ident, value } => {
                    let name = ident.name.to_string();

                    if self.global_map.contains_key(&name) || self.functions.contains_key(&name) {
                        return Err(CompileError::DuplicateIdentifier {
                            identifier: name,
                            span: ident.span,
                        }
                        .into());
                    }

                    let index = self.globals.len();
                    self.globals.push(value.unwrap_or(0));
                    self.global_map.insert(name, index);
                }
                GlobalDeclaration::Function {
                    name: ident,
                    arguments,
                    ..
                } => {
                    let name = ident.name.to_string();

                    if self.global_map.contains_key(&name) || self.functions.contains_key(&name) {
                        return Err(CompileError::DuplicateIdentifier {
                            identifier: name,
                            span: ident.span,
                        }
                        .into());
                    }

                    self.functions.insert(
                        name.clone(),
                        Function {
                            name,
                            arity: arguments.len(),
                            code: vec![],
                            locals: 0,
                        },
                    );
                }
            }
        }

        if !self.functions.contains_key("main") {
            Err(CompileError::MissingMain.into())
        } else {
            Ok(())
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<()> {
        for global_declaration in &program.0 {
            if let GlobalDeclaration::Function {
                name,
                arguments,
                body,
            } = &global_declaration.node
            {
                self.function(name, arguments, body)?;
            }
        }
        Ok(())
    }

    pub fn end(self) -> IR {
        IR {
            globals: self.globals,
            functions: self.functions,
        }
    }

    fn function(
        &mut self,
        name: &Identifier,
        arguments: &[Identifier],
        body: &Block,
    ) -> Result<()> {
        let params: Vec<String> = arguments.iter().map(|id| id.name.to_string()).collect();
        self.current_function = FunctionCompiler::new(name.name.to_string(), params);

        self.block(body)?;

        let function = std::mem::take(&mut self.current_function).end();
        self.functions.insert(name.name.to_string(), function);
        Ok(())
    }

    fn block(&mut self, block: &Block) -> Result<()> {
        self.begin_scope();

        for declaration in &block.0 {
            self.declaration(&declaration.node)?;
        }

        self.end_scope();
        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> Result<()> {
        match declaration {
            Declaration::Variable { name, value } => {
                if let Some(expr) = value {
                    self.expression(&expr.node)?;
                } else {
                    self.emit(Op::Push(0));
                }

                // shadowing from previous scopes while accessing the previous scope's value is allowed
                let slot = self.current_function.declare_local(name)?;
                self.emit(Op::SetLocal(slot));

                Ok(())
            }
            Declaration::Statement(statement) => self.statement(&statement.node),
        }
    }

    fn statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Block(block) => self.block(block),
            Statement::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let else_label = self.next_label();
                let end_label = self.next_label();

                self.expression(&condition.node)?;

                self.emit(Op::JumpIfZero(else_label));
                self.emit(Op::Pop);
                self.statement(&then_stmt.node)?;

                self.emit(Op::Jump(end_label));

                self.emit(Op::Label(else_label));
                self.emit(Op::Pop);
                if let Some(else_stmt) = else_stmt {
                    self.statement(&else_stmt.node)?;
                }

                self.emit(Op::Label(end_label));
                Ok(())
            }
            Statement::While { condition, body } => {
                let start = self.next_label();
                let end = self.next_label();
                self.emit(Op::Label(start));

                self.expression(&condition.node)?;
                self.emit(Op::JumpIfZero(end));
                self.emit(Op::Pop);

                self.statement(&body.node)?;

                self.emit(Op::Jump(start));
                self.emit(Op::Label(end));
                self.emit(Op::Pop);

                Ok(())
            }
            Statement::Return(expression) => {
                if let Some(expression) = expression {
                    self.expression(&expression.node)?;
                }
                self.emit(Op::Return);
                Ok(())
            }
            Statement::Expression(expression) => {
                self.expression(&expression.node)?;
                self.emit(Op::Pop);
                Ok(())
            }
        }
    }

    fn expression(&mut self, expression: &Expression) -> Result<()> {
        match expression {
            Expression::Assign { target, value } => match self.resolve_identifier(target)? {
                IdentifierKind::Function => Err(CompileError::InvalidAssignmentTarget {
                    identifier: target.name.into(),
                    span: target.span,
                }
                .into()),
                IdentifierKind::Global(index) => {
                    self.expression(&value.node)?;
                    self.emit(Op::SetGlobal(index));
                    Ok(())
                }
                IdentifierKind::Parameter(index) => {
                    self.expression(&value.node)?;
                    self.emit(Op::SetParameter(index));
                    Ok(())
                }
                IdentifierKind::Local(index) => {
                    self.expression(&value.node)?;
                    self.emit(Op::SetLocal(index));
                    Ok(())
                }
            },
            Expression::Logical { left, op, right } => match op {
                LogicalOp::And => {
                    let label = self.next_label();
                    self.expression(&left.node)?;
                    self.emit(Op::JumpIfZero(label));
                    self.emit(Op::Pop);
                    self.expression(&right.node)?;
                    self.emit(Op::Label(label));
                    Ok(())
                }
                LogicalOp::Or => {
                    let label = self.next_label();
                    self.expression(&left.node)?;
                    self.emit(Op::JumpIfNotZero(label));
                    self.emit(Op::Pop);
                    self.expression(&right.node)?;
                    self.emit(Op::Label(label));
                    Ok(())
                }
            },
            Expression::Binary { left, op, right } => {
                self.expression(&left.node)?;
                self.expression(&right.node)?;
                self.emit(match op {
                    BinaryOp::Add => Op::Add,
                    BinaryOp::Subtract => Op::Subtract,
                    BinaryOp::Equal => Op::Equal,
                    BinaryOp::NotEqual => Op::NotEqual,
                    BinaryOp::LessThan => Op::LessThan,
                    BinaryOp::LessEqual => Op::LessEqual,
                    BinaryOp::GreaterThan => Op::GreaterThan,
                    BinaryOp::GreaterEqual => Op::GreaterEqual,
                });
                Ok(())
            }
            Expression::Unary { op, right } => {
                self.expression(&right.node)?;
                self.emit(match op {
                    UnaryOp::Negate => Op::Negate,
                    UnaryOp::LogicalNot => Op::LogicalNot,
                    UnaryOp::Dereference => Op::Dereference,
                });
                Ok(())
            }

            Expression::Integer(value) => {
                self.emit(Op::Push(*value));
                Ok(())
            }
            Expression::Identifier(identifier) => match self.resolve_identifier(identifier)? {
                IdentifierKind::Function => Err(CompileError::InvalidVariable {
                    identifier: identifier.name.into(),
                    span: identifier.span,
                }
                .into()),
                IdentifierKind::Global(index) => {
                    self.emit(Op::GetGlobal(index));
                    Ok(())
                }
                IdentifierKind::Parameter(index) => {
                    self.emit(Op::GetParameter(index));
                    Ok(())
                }
                IdentifierKind::Local(index) => {
                    self.emit(Op::GetLocal(index));
                    Ok(())
                }
            },
            Expression::AddressOf(identifier) => match self.resolve_identifier(identifier)? {
                IdentifierKind::Function => Err(CompileError::InvalidVariable {
                    identifier: identifier.name.into(),
                    span: identifier.span,
                }
                .into()),
                IdentifierKind::Global(index) => {
                    self.emit(Op::GetGlobalAddress(index));
                    Ok(())
                }
                IdentifierKind::Parameter(index) => {
                    self.emit(Op::GetParameterAddress(index));
                    Ok(())
                }
                IdentifierKind::Local(index) => {
                    self.emit(Op::GetLocalAddress(index));
                    Ok(())
                }
            },
            Expression::Call { name, arguments } => match self.resolve_identifier(name)? {
                IdentifierKind::Function => {
                    let function = self.functions.get(name.name).unwrap();
                    if arguments.len() != function.arity {
                        return Err(CompileError::ArityMismatch {
                            function: name.name.into(),
                            expected: function.arity,
                            got: arguments.len(),
                            span: name.span,
                        }
                        .into());
                    }
                    // push arguments in reverse order
                    for argument in arguments.iter().rev() {
                        self.expression(&argument.node)?;
                    }
                    self.emit(Op::Call(name.name.into()));
                    Ok(())
                }
                _ => Err(CompileError::InvalidFunction {
                    identifier: name.name.into(),
                    span: name.span,
                }
                .into()),
            },
        }
    }

    fn begin_scope(&mut self) {
        self.current_function.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        if let Some(scope) = self.current_function.scopes.pop() {
            self.current_function.current_locals -= scope.len();
        } else {
            unreachable!()
        }
    }

    fn emit(&mut self, op: Op) {
        self.current_function.code.push(op);
    }

    fn next_label(&mut self) -> usize {
        let label = self.current_function.next_label;
        self.current_function.next_label += 1;
        label
    }

    fn resolve_identifier(&self, name: &Identifier) -> Result<IdentifierKind> {
        if let Some(index) = self.current_function.resolve_local(name.name) {
            Ok(IdentifierKind::Local(index))
        } else if let Some(index) = self.current_function.resolve_parameter(name.name) {
            Ok(IdentifierKind::Parameter(index))
        } else if let Some(index) = self.global_map.get(name.name) {
            Ok(IdentifierKind::Global(*index))
        } else if self.functions.contains_key(name.name) {
            Ok(IdentifierKind::Function)
        } else {
            Err(CompileError::UnknownVariable {
                identifier: name.name.into(),
                span: name.span,
            }
            .into())
        }
    }
}
