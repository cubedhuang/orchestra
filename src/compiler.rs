use crate::ir::IR;

pub fn compile<'src>(source: &'src str) -> IR {
    _ = source;
    IR { code: vec![] }
}
