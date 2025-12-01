use std::cmp;

use itertools::Itertools;

use crate::ir::{Function, IR, Op};

pub fn generate_lc3(ir: &IR) -> String {
    let backend = LC3Backend::new();
    backend.generate(ir)
}

pub struct LC3Backend {
    output: String,
    current_function_name: String,
    constants: Vec<isize>,
    label_counter: usize,
}

impl LC3Backend {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            constants: vec![],
            current_function_name: "".into(),
            label_counter: 0,
        }
    }

    pub fn generate(mut self, ir: &IR) -> String {
        self.emit_header();
        self.emit_globals(&ir.globals);

        for (_, function) in &ir.functions {
            self.current_function_name = function.name.clone();
            self.constants.clear();
            self.label_counter = 0;
            self.emit_function(function);
        }

        self.writeln(".end");

        self.output
    }

    fn emit_header(&mut self) {
        self.writeln(".orig x3000");
        self.writeln("  LEA R4, globals");
        self.writeln("  LD R6, stack_base");
        self.writeln("  ADD R5, R6, #0");
        self.writeln("  JSR fn_main");
        self.writeln("  HALT");
        self.writeln("stack_base  .fill xF000");
    }

    fn emit_globals(&mut self, globals: &[isize]) {
        self.writeln("globals");
        for value in globals {
            self.writeln(&format!("  .fill {value}"));
        }
    }

    fn emit_function(&mut self, function: &Function) {
        self.writeln(&format!("fn_{}", function.name));

        self.emit_buildup(function);

        for op in &function.code {
            self.emit_op(op, function);
        }

        self.emit_teardown(function);
        if !self.constants.is_empty() {
            self.emit_constants();
        }
    }

    fn emit_buildup(&mut self, function: &Function) {
        // reserve return value space
        self.writeln("  ADD R6, R6, #-1");

        // save return address
        self.writeln("  ADD R6, R6, #-1");
        self.writeln("  STR R7, R6, #0");

        // save old frame pointer
        self.writeln("  ADD R6, R6, #-1");
        self.writeln("  STR R5, R6, #0");

        // set new frame pointer
        self.writeln("  ADD R5, R6, #-1");

        // locals (always at least 1)
        self.emit_add_immediate("R6", "R6", -(cmp::max(function.locals, 1) as isize));
    }

    fn emit_teardown(&mut self, function: &Function) {
        self.writeln(&format!("fn_{}_teardown", function.name));

        // restore old frame pointer
        self.writeln("  ADD R6, R5, #1");
        self.writeln("  LDR R5, R6, #0");

        // restore return address
        self.writeln("  ADD R6, R6, #1");
        self.writeln("  LDR R7, R6, #0");

        // pop return address and return
        self.writeln("  ADD R6, R6, #1");
        self.writeln("  RET");
    }

    fn emit_constants(&mut self) {
        let constants = self
            .constants
            .iter()
            .enumerate()
            .map(|(i, value)| {
                format!(
                    "fn_{}_const_{i}  .fill {value}",
                    &self.current_function_name
                )
            })
            .join("\n");
        self.writeln(&constants);
    }

    fn emit_op(&mut self, op: &Op, function: &Function) {
        self.writeln(&format!("  ;; {op}"));

        match op {
            Op::Push(value) => {
                self.load_constant("R0", *value);
                self.push("R0");
            }

            Op::Pop => {
                self.pop("R0");
            }

            Op::GetLocal(index) => {
                self.load_reg_offset("R5", "R0", -(*index as isize));
                self.push("R0");
            }

            Op::SetLocal(index) => {
                self.peek("R0");
                self.store_reg_offset("R5", "R0", -(*index as isize), "R1");
            }

            Op::GetParameter(index) => {
                self.load_reg_offset("R5", "R0", *index as isize + 4);
                self.push("R0");
            }

            Op::SetParameter(index) => {
                self.peek("R0");
                self.store_reg_offset("R5", "R0", *index as isize + 4, "R1");
            }

            Op::GetGlobal(index) => {
                self.load_reg_offset("R4", "R0", *index as isize);
                self.push("R0");
            }

            Op::SetGlobal(index) => {
                self.peek("R0");
                self.store_reg_offset("R4", "R0", *index as isize, "R1");
            }

            Op::GetLocalAddress(index) => {
                self.emit_add_immediate("R0", "R5", -(*index as isize));
                self.push("R0");
            }

            Op::GetParameterAddress(index) => {
                self.emit_add_immediate("R0", "R5", *index as isize + 4);
                self.push("R0");
            }

            Op::GetGlobalAddress(index) => {
                self.emit_add_immediate("R0", "R4", *index as isize);
                self.push("R0");
            }

            Op::Add => {
                self.pop("R0");
                self.pop("R1");
                self.writeln("  ADD R0, R0, R1");
                self.push("R0");
            }

            Op::Subtract => {
                self.pop("R1"); // right
                self.pop("R0"); // left
                self.writeln("  NOT R1, R1");
                self.writeln("  ADD R1, R1, #1");
                self.writeln("  ADD R0, R0, R1");
                self.push("R0");
            }

            Op::BitwiseOr => {
                self.pop("R0");
                self.pop("R1");
                self.writeln("  NOT R0, R0");
                self.writeln("  NOT R1, R1");
                self.writeln("  AND R0, R0, R1");
                self.writeln("  NOT R0, R0");
                self.push("R0");
            }

            Op::BitwiseXor => {
                self.pop("R0");
                self.pop("R1");
                self.writeln("  NOT R2, R0");
                self.writeln("  AND R2, R2, R1");
                self.writeln("  NOT R3, R1");
                self.writeln("  AND R3, R3, R0");
                self.writeln("  ADD R0, R2, R3");
                self.push("R0");
            }

            Op::BitwiseAnd => {
                self.pop("R0");
                self.pop("R1");
                self.writeln("  AND R0, R0, R1");
                self.push("R0");
            }

            Op::Negate => {
                self.pop("R0");
                self.writeln("  NOT R0, R0");
                self.writeln("  ADD R0, R0, #1");
                self.push("R0");
            }

            Op::LogicalNot => {
                let true_label = self.fresh_label(&function.name, "not_true");
                let end_label = self.fresh_label(&function.name, "not_end");
                self.pop("R0");
                // cc is set
                self.writeln(&format!("  BRz {true_label}"));
                // it is nonzero
                self.writeln("  AND R0, R0, #0");
                self.writeln(&format!("  BR {end_label}"));
                self.writeln(&true_label);
                // it is zero
                self.writeln("  AND R0, R0, #0");
                self.writeln("  ADD R0, R0, #1");
                self.writeln(&end_label);
                self.push("R0");
            }

            Op::BitwiseNot => {
                self.pop("R0");
                self.writeln("  NOT R0, R0");
                self.push("R0");
            }

            Op::Dereference => {
                self.pop("R0");
                self.writeln("  LDR R0, R0, #0");
                self.push("R0");
            }

            Op::Equal => {
                self.emit_comparison(&function.name, "np");
            }
            Op::NotEqual => {
                self.emit_comparison(&function.name, "z");
            }
            Op::LessThan => {
                self.emit_comparison(&function.name, "zp");
            }
            Op::LessEqual => {
                self.emit_comparison(&function.name, "p");
            }
            Op::GreaterThan => {
                self.emit_comparison(&function.name, "nz");
            }
            Op::GreaterEqual => {
                self.emit_comparison(&function.name, "n");
            }

            Op::StoreIndirect => {
                self.pop("R1"); // value
                self.pop("R0"); // address
                self.writeln("  STR R1, R0, #0");
                self.push("R1"); // put value back on for future assignments
            }

            Op::Label(id) => {
                self.writeln(&format!("fn_{}_label_{id}", function.name));
            }

            Op::Jump(id) => {
                self.writeln(&format!("  BR fn_{}_label_{id}", function.name));
            }

            Op::JumpIfZero(id) => {
                self.peek("R0");
                self.writeln(&format!("  BRz fn_{}_label_{id}", function.name));
            }

            Op::JumpIfNotZero(id) => {
                self.peek("R0");
                self.writeln(&format!("  BRnp fn_{}_label_{id}", function.name));
            }

            Op::Call { function, arity } => {
                self.writeln(&format!("  JSR fn_{function}"));
                if *arity > 0 {
                    self.pop("R0");
                    self.emit_add_immediate("R6", "R6", *arity as isize);
                    self.push("R0");
                }
            }

            Op::Return => {
                self.pop("R0");
                self.writeln("  STR R0, R5, #3");
                self.writeln(&format!("  BR fn_{}_teardown", function.name));
            }
        }
    }

    fn emit_comparison(&mut self, name: &str, branch_false: &str) {
        self.pop("R1"); // right
        self.pop("R0"); // left

        self.writeln("  NOT R1, R1");
        self.writeln("  ADD R1, R1, #1");
        self.writeln("  ADD R0, R0, R1");

        let false_label = self.fresh_label(name, "comp_false");
        let end_label = self.fresh_label(name, "comp_end");

        self.writeln(&format!("  BR{branch_false} {false_label}"));
        // true
        self.writeln("  AND R0, R0, #0");
        self.writeln("  ADD R0, R0, #1");
        self.writeln(&format!("  BR {end_label}"));
        self.writeln(&format!("{false_label}"));
        // false
        self.writeln("  AND R0, R0, #0");
        self.writeln(&format!("{end_label}"));

        self.push("R0");
    }

    fn push(&mut self, reg: &str) {
        self.writeln(&format!("  ADD R6, R6, #-1"));
        self.writeln(&format!("  STR {reg}, R6, #0"));
    }

    fn pop(&mut self, reg: &str) {
        // this way preserves condition codes
        self.writeln(&format!("  ADD R6, R6, #1"));
        self.writeln(&format!("  LDR {reg}, R6, #-1"));
    }

    fn peek(&mut self, reg: &str) {
        self.writeln(&format!("  LDR {reg}, R6, #0"));
    }

    fn load_constant(&mut self, reg: &str, value: isize) {
        if value >= -16 && value <= 15 {
            self.writeln(&format!("  AND {reg}, {reg}, #0"));
            if value != 0 {
                self.writeln(&format!("  ADD {reg}, {reg}, #{value}"));
            }
        } else {
            let index = if let Some(index) = self.constants.iter().position(|&v| v == value) {
                index
            } else {
                self.constants.push(value);
                self.constants.len() - 1
            };

            self.writeln(&format!(
                "  LD {reg}, fn_{}_const_{index}",
                &self.current_function_name
            ));
        }
    }

    fn load_reg_offset(&mut self, base: &str, dest: &str, offset: isize) {
        if offset >= -32 && offset <= 31 {
            self.writeln(&format!("  LDR {dest}, {base}, #{offset}"));
        } else {
            self.load_constant(dest, offset as isize);
            self.writeln(&format!("  ADD {dest}, {base}, {dest}"));
            self.writeln(&format!("  LDR {dest}, {dest}, #0"));
        }
    }

    fn store_reg_offset(&mut self, base: &str, src: &str, offset: isize, temp: &str) {
        if offset >= -32 && offset <= 31 {
            self.writeln(&format!("  STR {src}, {base}, #{offset}"));
        } else {
            self.load_constant(temp, offset as isize);
            self.writeln(&format!("  ADD {temp}, {base}, {temp}"));
            self.writeln(&format!("  STR R0, {temp}, #0"));
        }
    }

    fn emit_add_immediate(&mut self, dest: &str, src: &str, value: isize) {
        if value == 0 {
            if dest != src {
                self.writeln(&format!("  ADD {dest}, {src}, #0"));
            }
        } else if value >= -16 && value <= 15 {
            self.writeln(&format!("  ADD {dest}, {src}, #{value}"));
        } else {
            let mut remaining = value;

            let chunk = remaining.clamp(-16, 15);
            self.writeln(&format!("  ADD {dest}, {src}, #{chunk}"));
            remaining -= chunk;

            while remaining != 0 {
                let chunk = remaining.clamp(-16, 15);
                self.writeln(&format!("  ADD {dest}, {dest}, #{chunk}"));
                remaining -= chunk;
            }
        }
    }

    fn fresh_label(&mut self, function_name: &str, description: &str) -> String {
        let label = format!("fn_{function_name}_{description}_{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn writeln(&mut self, line: &str) {
        self.output.push_str(line);
        self.output.push('\n');
    }
}
