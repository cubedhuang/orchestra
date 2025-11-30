use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum CompileError {
    #[error("duplicate identifier '{identifier}'")]
    #[diagnostic(code(compile::duplicate_identifier))]
    DuplicateIdentifier {
        identifier: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("missing main function; main is required!")]
    #[diagnostic(code(compile::missing_main))]
    MissingMain,

    #[error("unknown variable '{identifier}'")]
    #[diagnostic(code(compile::unknown_variable))]
    UnknownVariable {
        identifier: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid assignment target '{identifier}'; functions cannot be assigned")]
    #[diagnostic(code(compile::invalid_assignment_target))]
    InvalidAssignmentTarget {
        identifier: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("invalid variable '{identifier}'; functions cannot be used as values")]
    #[diagnostic(code(compile::invalid_variable))]
    InvalidVariable {
        identifier: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("'{identifier}' is not a function")]
    #[diagnostic(code(compile::invalid_function))]
    InvalidFunction {
        identifier: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("'{function}' expects {expected} argument(s), got {got}")]
    #[diagnostic(code(compile::arity_mismatch))]
    ArityMismatch {
        function: String,
        expected: usize,
        got: usize,
        #[label("here")]
        span: SourceSpan,
    },
}
