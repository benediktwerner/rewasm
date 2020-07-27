use std::collections::HashSet;
use std::rc::Rc;

use slab::Slab;

use crate::fmt::CodeDisplay;
use crate::ssa::Stmt;
use crate::wasm;

mod builder;
pub use builder::CfgBuildError;

pub type NodeId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InstrPos {
    pub node: usize,
    pub instr: usize,
}

impl InstrPos {
    pub const fn new(node: usize, instr: usize) -> Self {
        Self { node, instr }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EdgeType {
    Unconditional,
    Conditional(bool),
    CaseRange(u32, u32), // [start, end)
    Default(u32),
}

#[derive(Debug, Clone, Copy)]
pub struct EdgeCond {
    pub expr_index: u32,
    pub edge_type: EdgeType,
}

impl EdgeCond {
    pub const fn unconditional() -> Self {
        EdgeCond {
            expr_index: 0,
            edge_type: EdgeType::Unconditional,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Edge {
    pub cond: EdgeCond,
    pub node: NodeId,
    pub back_edge: bool,
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub code: Vec<Stmt>,
    pub next: Vec<Edge>,
    pub prev: Vec<Edge>,
}

impl BasicBlock {
    fn new() -> Self {
        BasicBlock::default()
    }

    /// Iterator over all successors including back edges
    pub fn succs<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.next.iter().map(|e| e.node)
    }

    /// Iterator over all successors but excluding back edges
    pub fn forward_succs<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.next
            .iter()
            .filter_map(|e| if e.back_edge { None } else { Some(e.node) })
    }

    pub fn preds<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.prev.iter().map(|e| e.node)
    }
}

pub struct Cfg {
    pub func_index: u32,
    pub wasm: Rc<wasm::Instance>,
    pub nodes: Slab<BasicBlock>,
}

impl Cfg {
    pub fn dot_string(&self) -> String {
        let nodes = self
            .nodes
            .iter()
            .map(|(i, bb)| {
                let instrs = (&bb.code[..])
                    .create_str(Rc::clone(&self.wasm), self.func_index)
                    .replace("\n", "\\n");
                if bb.code.is_empty() {
                    format!("\t{}", i)
                } else {
                    format!("\t{} [shape=box, label=\"{}\\n{}\"]", i, i, instrs)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");
        let edges = self
            .nodes
            .iter()
            .flat_map(|(i, bb)| {
                bb.next.iter().map(move |e| {
                    format!(
                        "\t{}:s -> {}:n [label=\"{}\", color=\"{}\"]",
                        i,
                        e.node,
                        match e.cond.edge_type {
                            EdgeType::Unconditional => "".to_string(),
                            EdgeType::Conditional(true) => "true".to_string(),
                            EdgeType::Conditional(false) => "false".to_string(),
                            EdgeType::CaseRange(start, end) => format!("{}..{}", start, end),
                            EdgeType::Default(_) => "default".to_string(),
                        },
                        if e.back_edge { "red" } else { "black" }
                    )
                })
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("digraph G {{\n{}\n\n\tstart -> 0:n\n{}\n}}", nodes, edges)
    }

    pub fn build(wasm: Rc<wasm::Instance>, func_index: u32) -> Result<Cfg, CfgBuildError> {
        builder::build(wasm, func_index)
    }

    pub fn region_successors(&self, region_nodes: &HashSet<NodeId>) -> HashSet<NodeId> {
        let mut result = HashSet::new();
        for &n in region_nodes {
            for s in self.nodes[n].succs() {
                if !region_nodes.contains(&s) {
                    result.insert(s);
                }
            }
        }
        result
    }

    pub fn stmt(&self, pos: InstrPos) -> &Stmt {
        &self.nodes[pos.node].code[pos.instr]
    }

    pub fn stmt_mut(&mut self, pos: InstrPos) -> &mut Stmt {
        &mut self.nodes[pos.node].code[pos.instr]
    }

    pub fn graph_slice(&self, head: NodeId, targets: HashSet<NodeId>) -> HashSet<NodeId> {
        GraphSliceBuilder::dfs(self, head, targets)
    }
}

struct GraphSliceBuilder {
    targets: HashSet<NodeId>,
    visited: HashSet<NodeId>,
}

impl GraphSliceBuilder {
    /// Find all nodes between the head and target nodes.
    /// The head and target nodes are included.
    fn dfs(cfg: &Cfg, head: NodeId, targets: HashSet<NodeId>) -> HashSet<NodeId> {
        let mut performer = GraphSliceBuilder {
            targets,
            visited: HashSet::new(),
        };
        performer.visit(cfg, head);
        performer.targets
    }

    // returns true if the visited node is in the slice
    fn visit(&mut self, cfg: &Cfg, n: usize) -> bool {
        let mut inserted = self.targets.contains(&n);
        for u in cfg.nodes[n].forward_succs() {
            if !self.visited.contains(&u) {
                self.visited.insert(u);
                if self.visit(cfg, u) && !inserted {
                    self.targets.insert(n);
                    inserted = true;
                }
            } else if !inserted && self.targets.contains(&u) {
                self.targets.insert(n);
                inserted = true;
            }
        }
        inserted
    }
}
