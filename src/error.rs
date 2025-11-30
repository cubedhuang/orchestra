use miette::Diagnostic;
use thiserror::Error;

use crate::{lex::LexError, parser::SyntaxError};

#[derive(Error, Debug, Diagnostic)]
pub enum OrchestraError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexError(#[from] LexError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SyntaxError(#[from] SyntaxError),
}

pub type Result<T> = std::result::Result<T, OrchestraError>;
