use crate::{compile::compiler::Compiler, error::Result, ir::IR, parse::parse};

mod compiler;
pub mod error;

pub fn compile<'src>(source: &'src str) -> Result<IR> {
    let ast = parse(source)?;

    let mut compiler = Compiler::new();
    compiler.register_globals(&ast)?;
    compiler.compile(&ast)?;

    Ok(compiler.end())
}
