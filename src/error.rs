use miette::Diagnostic;
use thiserror::Error;

use crate::{compile::error::CompileError, lex::LexError, parse::error::SyntaxError};

#[derive(Error, Debug, Diagnostic)]
pub enum OrchestraError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexError(#[from] LexError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SyntaxError(#[from] SyntaxError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    CompileError(#[from] CompileError),
}

pub type Result<T> = std::result::Result<T, OrchestraError>;
