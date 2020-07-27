pub mod cond;
mod construction;
mod deconstruction;
pub mod expr;
pub mod stmt;
mod value_space;

pub use cond::Cond;
pub use construction::transform_to_ssa;
pub use deconstruction::transform_out_of_ssa;
pub use expr::Expr;
pub use stmt::{LoopKind, Stmt};
pub use value_space::ValueSpace;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var {
    pub index: u32,
    pub subscript: u32,
}

impl Var {
    pub const fn new(index: u32, subscript: u32) -> Self {
        Var { index, subscript }
    }

    pub const fn no_sub(index: u32) -> Self {
        Var { index, subscript: 0 }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.subscript == 0 {
            write!(f, "{}", self.index)
        } else {
            write!(f, "{}_{}", self.index, self.subscript)
        }
    }
}
