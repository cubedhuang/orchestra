use clap::Parser;
use miette::{IntoDiagnostic, Result, miette};

mod ast;
mod error;
mod ir;
mod lex;
mod parser;

/// orchestra to LC3 assembly compiler
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of input orchestra file
    input: String,

    /// Name of output assembly file
    output: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let source = std::fs::read_to_string(args.input).into_diagnostic()?;

    let ir = match parser::parse(&source) {
        Ok(ir) => ir,
        Err(err) => return Err(miette!(err).with_source_code(source).into()),
    };
    dbg!(ir);

    Ok(())
}
