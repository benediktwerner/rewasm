use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

use crate::cfg::{Cfg, Edge};
use crate::ssa::dominance::DomTree;
use crate::ssa::{Expr, Stmt};

struct Structurer {
    cfg: Cfg,
    loops: HashMap<usize, HashSet<usize>>,
    post_order: Vec<usize>,
    dom_tree: DomTree,
}

impl Structurer {
    #[allow(clippy::map_entry)]
    fn structure(mut self) -> Vec<Stmt> {
        for i in 0..self.post_order.len() {
            let head = self.post_order[i];

            if self.loops.contains_key(&head) {
                let mut loop_nodes = LoopDFS::dfs(&self.cfg, head, &self.loops[&head]);
                let mut loop_succs = self.cfg.region_successors(&loop_nodes);

                while loop_succs.len() > 1 {
                    let mut new_loop_nodes = Vec::new();
                    let mut new_loop_succs = Vec::new();
                    for &n in &loop_succs {
                        if self.cfg.nodes[n].preds().all(|p| loop_nodes.contains(&p)) {
                            loop_nodes.insert(n);
                            new_loop_nodes.push(n);
                            for s in self.cfg.nodes[n].succs() {
                                if !loop_nodes.contains(&s) && !loop_succs.contains(&s) {
                                    new_loop_succs.push(s);
                                }
                            }
                        }
                    }
                    if new_loop_nodes.is_empty() && new_loop_succs.is_empty() {
                        break;
                    }
                    for n in new_loop_nodes {
                        loop_succs.remove(&n);
                    }
                    loop_succs.extend(new_loop_succs.drain(..));
                }

                if loop_succs.len() > 1 {
                    // TODO: remove abnormal exits
                    unimplemented!();
                }

                let loop_succ = loop_succs.into_iter().next();
                if let Some(succ) = loop_succ {
                    for &n in &loop_nodes {
                        let mut cond = Vec::new();
                        self.cfg.nodes[n].next.retain(|e| {
                            if e.node == succ {
                                cond.push(e.cond.clone());
                                false
                            } else {
                                e.node != head
                            }
                        });
                        if !cond.is_empty() {
                            let break_stmt =
                                Stmt::If(Expr::and(cond.into_iter()), vec![Stmt::Break]);
                            self.cfg.nodes[n].code.push(break_stmt);
                        }
                    }
                    self.cfg.nodes[head].next.push(Edge {
                        cond: Expr::True,
                        node: succ,
                    });
                    self.cfg.nodes[head]
                        .prev
                        .retain(|e| !loop_nodes.contains(&e.node));
                    self.cfg.nodes[succ]
                        .prev
                        .retain(|e| !loop_nodes.contains(&e.node));
                    self.cfg.nodes[succ].prev.push(Edge {
                        cond: Expr::True,
                        node: head,
                    });
                }

                self.loops.remove(&head);

                if loop_nodes.len() > 1 {
                    self.structure_acyclic(head, i, &loop_nodes, loop_succ);
                }

                let code = &mut self.cfg.nodes[head].code;
                let old_code = std::mem::replace(code, Vec::new());
                code.push(Stmt::While(Expr::True, old_code));
            } else {
                let nodes = self.dom_tree.dominated_by(head);
                if nodes.len() == 1 {
                    continue;
                }
                if self
                    .loops
                    .iter()
                    .any(|(_, latch_nodes)| latch_nodes.iter().any(|n| nodes.contains(n)))
                {
                    continue;
                }

                let region_succs = self.cfg.region_successors(&nodes);
                if region_succs.len() < 2 {
                    self.structure_acyclic(head, i, &nodes, region_succs.into_iter().next());
                }
            }
        }

        if self.cfg.nodes.len() > 1 {
            self.structure_acyclic(0, 0, &self.dom_tree.dominated_by(0), None);
            if self.cfg.nodes.len() > 1 {
                panic!("Structuring failed. More than one node left.");
            }
        }

        self.cfg.nodes.remove(0).code
    }

    fn structure_acyclic(
        &mut self,
        head: usize,
        i: usize,
        nodes: &HashSet<usize>,
        succ: Option<usize>,
    ) {
        let mut reaching_cond = HashMap::new();
        reaching_cond.insert(head, Expr::True);

        let mut nodes_done = 1;
        for &n in self.post_order[..i].iter().rev() {
            if !nodes.contains(&n) {
                continue;
            }
            let mut cond = Vec::new();
            for e in &self.cfg.nodes[n].prev {
                cond.push(reaching_cond[&e.node].clone());
                cond.push(e.cond.clone());
            }
            let cond = Expr::and(cond.into_iter());
            reaching_cond.insert(n, cond.clone());
            let n = self.cfg.nodes.remove(n);
            if cond.is_const_true() {
                let mut n_code = n.code;
                self.cfg.nodes[head].code.append(&mut n_code);
            } else {
                self.cfg.nodes[head].code.push(Stmt::If(cond, n.code));
            }

            nodes_done += 1;
            if nodes_done == nodes.len() {
                break;
            }
        }

        // TODO: refine if-stmts

        self.cfg.nodes[head].next.clear();
        if let Some(succ) = succ {
            self.cfg.nodes[head].next.push(Edge {
                cond: Expr::True,
                node: succ,
            });
            self.cfg.nodes[succ]
                .prev
                .retain(|e| !nodes.contains(&e.node));
            self.cfg.nodes[succ].prev.push(Edge {
                cond: Expr::True,
                node: head,
            });
        }

        self.dom_tree.succs[head].retain(|n| !nodes.contains(n));
    }
}

pub fn structure(cfg: Cfg) -> Vec<Stmt> {
    let (loops, post_order) = InitDFS::dfs(&cfg);
    let dom_tree = DomTree::build(&cfg);
    Structurer {
        cfg,
        loops,
        post_order,
        dom_tree,
    }
    .structure()
}

struct InitDFS {
    loops: HashMap<usize, HashSet<usize>>,
    post_order: Vec<usize>,
    visited: HashSet<usize>,
    stack: HashSet<usize>,
}

impl InitDFS {
    fn dfs(cfg: &Cfg) -> (HashMap<usize, HashSet<usize>>, Vec<usize>) {
        let mut performer = InitDFS {
            loops: HashMap::new(),
            post_order: Vec::with_capacity(cfg.nodes.len()),
            visited: HashSet::new(),
            stack: HashSet::new(),
        };
        performer.visited.insert(0);
        performer.visit(cfg, 0);
        (performer.loops, performer.post_order)
    }

    fn visit(&mut self, cfg: &Cfg, n: usize) {
        self.stack.insert(n);

        for u in cfg.nodes[n].succs() {
            if !self.visited.contains(&u) {
                self.visited.insert(u);
                self.visit(cfg, u);
            } else if self.stack.contains(&u) {
                self.loops.entry(u).or_default().insert(n);
            }
        }

        self.post_order.push(n);
        self.stack.remove(&n);
    }
}

struct LoopDFS {
    loop_header: usize,
    loop_nodes: HashSet<usize>,
    visited: HashSet<usize>,
}

impl LoopDFS {
    fn dfs(cfg: &Cfg, loop_header: usize, latch_nodes: &HashSet<usize>) -> HashSet<usize> {
        let mut performer = LoopDFS {
            loop_header,
            loop_nodes: HashSet::from_iter(latch_nodes.iter().copied()),
            visited: HashSet::new(),
        };
        performer.visit(cfg, loop_header);
        performer.loop_nodes.insert(loop_header);
        performer.loop_nodes
    }

    fn visit(&mut self, cfg: &Cfg, n: usize) {
        for u in cfg.nodes[n].succs() {
            if u == self.loop_header {
                return;
            } else if self.loop_nodes.contains(&u) {
                self.loop_nodes.insert(n);
            } else if !self.visited.contains(&u) {
                self.visited.insert(u);
                self.visit(cfg, u);
            }
        }
    }
}
