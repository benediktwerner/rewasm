use std::collections::{HashMap, HashSet};

use crate::cfg::InstrPos;
use crate::ssa::Var;

mod dead_code_elimination;
mod expression_propagation;
pub mod used_vars;

pub use dead_code_elimination::eliminate_dead_code;
pub use expression_propagation::propagate_expressions;

#[derive(Default)]
pub struct DefUseMap(pub HashMap<Var, InstrPos>, pub HashMap<Var, HashSet<InstrPos>>);

impl DefUseMap {
    pub fn new() -> Self {
        DefUseMap::default()
    }
}
