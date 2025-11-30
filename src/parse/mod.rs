use crate::{ast::Program, error::Result, parse::parser::Parser};

pub mod error;
mod parser;
mod prec;

pub fn parse<'src>(source: &'src str) -> Result<Program<'src>> {
    let mut parser = Parser::new(source)?;
    let program = parser.program()?;
    return Ok(program);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bad_global_declarations() {
        let sources = ["let f =", "fn a() {", "fn ()"];
        for source in sources {
            assert!(parse(source).is_err());
        }
    }

    #[test]
    fn good_global_declarations() {
        let sources = ["let a = 10;", "let a;", "fn a() {}", "fn f(a, b) {}"];
        for source in sources {
            assert!(parse(source).is_ok());
        }
    }

    #[test]
    fn expressions() {
        let sources = [
            "fn main() { x = 5; }",
            "fn main() { x = y = 5; }",
            "fn main() { a + b - c; }",
            "fn main() { !x; }",
            "fn main() { -5; }",
            "fn main() { *p; }",
            "fn main() { &x; }",
            "fn main() { foo(1, 2, 3); }",
            "fn main() { x and y or z; }",
            "fn main() { (a + b) - c; }",
            "fn main() { *&x; }",
        ];

        for source in sources {
            assert!(parse(source).is_ok());
        }
    }

    #[test]
    fn bad_expressions() {
        let sources = [
            "fn main() { &(x + 1); }",
            "fn main() { (x + y) = 5; }",
            "fn main() { 5(); }",
        ];

        for source in sources {
            assert!(parse(source).is_err());
        }
    }
}
