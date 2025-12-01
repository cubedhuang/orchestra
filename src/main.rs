use clap::Parser;
use miette::{IntoDiagnostic, Result, miette};

use crate::backend::lc3::generate_lc3;

mod ast;
mod backend;
mod compile;
mod error;
mod ir;
mod lex;
mod parse;

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

    let ir = match compile::compile(&source) {
        Ok(ir) => ir,
        Err(err) => return Err(miette!(err).with_source_code(source).into()),
    };
    println!("{ir}");

    std::fs::write(args.output, generate_lc3(&ir)).into_diagnostic()?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use lc3_ensemble::{asm::assemble, ast::Reg, parse::parse_ast, sim::Simulator};

    use super::*;

    fn run_simulator(source: &str) -> Simulator {
        let ir = compile::compile(&source).expect("compilation error");
        let assembly = generate_lc3(&ir);

        let ast = parse_ast(&assembly).expect("failed to parse generated assembly");
        let obj_file = assemble(ast).expect("failed to assemble object file");

        let mut sim = Simulator::new(Default::default());
        sim.load_obj_file(&obj_file)
            .expect("failed to load object file");
        sim.run_with_limit(10000)
            .expect("program took too long to finish");
        assert_eq!(
            sim.reg_file[Reg::R6].get(),
            0xEFFF,
            "expected stack to point to main return value"
        );
        sim
    }

    fn run_get_return(source: &str) -> u16 {
        let sim = run_simulator(source);
        sim.mem[sim.reg_file[Reg::R6].get()].get()
    }

    fn read_memory_range(sim: &Simulator, start: u16, len: u16) -> Vec<u16> {
        (start..start + len).map(|i| sim.mem[i].get()).collect()
    }

    #[test]
    fn fib() {
        let source = "
            fn main() {
                return fib(10);
            }

            fn fib(n) {
                if (n <= 1) {
                    return n;
                } else {
                    return fib(n - 1) + fib(n - 2);
                }
            }
        ";
        let value = run_get_return(source);
        assert_eq!(value, 55);
    }

    #[test]
    fn shadowing() {
        let source = "
            let array = 0x4000;
            let x = 1;

            fn main() {
                {
                    push(x);
                    let x = 2;
                    push(x);
                    {
                        let x = x + 1; // = 3
                        push(x);
                    }
                    {
                        push(x);
                        let x = 4;
                        push(x);
                    }
                    push(x);
                }
                push(x);
            }

            fn push(value) {
                array @= value;
                array = array + 1;
            }
        ";
        let sim = run_simulator(source);
        assert_eq!(
            read_memory_range(&sim, 0x4000, 7),
            vec![1, 2, 3, 2, 4, 2, 1]
        );
    }
}
