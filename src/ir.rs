pub type Value = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Constant(Value),
    Pop,
    Return,
}

#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<Op>,
}

// #[derive(Debug)]
pub type IR = Chunk;
