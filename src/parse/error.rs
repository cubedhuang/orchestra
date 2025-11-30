use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lex::TokenKind;

#[derive(Error, Debug, Diagnostic)]
pub enum SyntaxError {
    #[error("expected {expected} {context}")]
    #[diagnostic(code(parse::expected_token))]
    ExpectedToken {
        expected: TokenKind,
        #[label("here")]
        span: SourceSpan,
        context: String,
    },

    #[error("unexpected token {context}")]
    #[diagnostic(code(parse::unexpected_token))]
    UnexpectedToken {
        #[label("here")]
        span: SourceSpan,
        context: String,
    },

    #[error("invalid number")]
    #[diagnostic(code(parse::invalid_number))]
    InvalidNumber {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid assignment target; target must be an identifier")]
    #[diagnostic(code(parse::invalid_assignment_target))]
    InvalidAssignmentTarget {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid call target; target must be an identifier")]
    #[diagnostic(code(parse::invalid_call_target))]
    InvalidCallTarget {
        #[label("here")]
        span: SourceSpan,
    },
}
