use std::collections::{HashMap, HashSet};

use crate::cfg::{Cfg, Edge, EdgeCond, EdgeType};
use crate::dominance::DomTree;
use crate::ssa::{Cond, Expr, LoopKind, Stmt, Var};
use bwasm::ValueType;

mod condition_refinement;
mod loop_refinement;
mod rename_vars;
mod sidefx_remover;

struct Structurer<'a> {
    cfg: Cfg,
    post_order: Vec<usize>,
    dom_tree: DomTree,
    expr_map: &'a mut HashMap<u32, Expr>,
    next_expr_index: u32,
}

impl<'a> Structurer<'a> {
    fn structure(mut self) -> Vec<Stmt> {
        for i in 0..self.post_order.len() {
            let head = self.post_order[i];

            if self.cfg.nodes[head].prev.iter().any(|e| e.back_edge) {
                self.structure_cyclic(head, i);
            }

            let nodes = self.dom_tree.dominated_by(head);
            if nodes.len() == 1 {
                continue;
            }

            let region_succs = self.cfg.region_successors(&nodes);
            if region_succs.len() < 2 {
                self.structure_acyclic(head, i, &nodes);

                self.dom_tree.remove(head, &nodes);
                self.cfg.nodes[head].next.clear();

                if let Some(succ) = region_succs.into_iter().next() {
                    let prev = &mut self.cfg.nodes[succ].prev;
                    let back_edge = prev.iter().any(|edge| edge.back_edge && nodes.contains(&edge.node));

                    prev.retain(|edge| !nodes.contains(&edge.node));
                    prev.push(Edge {
                        node: head,
                        cond: EdgeCond::unconditional(),
                        back_edge,
                    });

                    self.cfg.nodes[head].next.push(Edge {
                        node: succ,
                        cond: EdgeCond::unconditional(),
                        back_edge,
                    });
                }
            }
        }

        if self.cfg.nodes.len() > 1 {
            unreachable!("More than one node left after structuring");
        }

        self.cfg.nodes.remove(0).code
    }

    fn structure_acyclic(&mut self, head: usize, i: usize, nodes: &HashSet<usize>) {
        let mut reaching_conds = HashMap::new();
        reaching_conds.insert(head, Cond::True);

        let mut nodes_done = 1;

        for &n in self.post_order[..i].iter().rev() {
            if !nodes.contains(&n) {
                continue;
            }

            let mut cond = Cond::False;
            for edge in &self.cfg.nodes[n].prev {
                cond = cond.or(reaching_conds[&edge.node].clone().and(edge.cond.into()));
            }
            cond.simplify();
            reaching_conds.insert(n, cond.clone());

            let code = self.cfg.nodes.remove(n).code;
            // TODO: change this when conds can have sidefx
            //       In that case the condition needs to run, even if the code is empty
            if !code.is_empty() {
                if cond.is_const_true() {
                    self.cfg.nodes[head].code.extend(code);
                } else if !cond.is_const_false() {
                    self.cfg.nodes[head].code.push(Stmt::If(cond, code));
                }
            }

            nodes_done += 1;
            if nodes_done == nodes.len() {
                break;
            }
        }

        condition_refinement::apply(&mut self.cfg.nodes[head].code);
    }

    fn structure_cyclic(&mut self, head: usize, i: usize) {
        let (nodes, succs) = self.find_loop_region(head);

        // TODO: what if no loop succ???
        if succs.len() == 1 {
            let succ = succs.into_iter().next().unwrap();
            self.insert_breaks_single_succ(head, &nodes, succ);
        } else {
            self.insert_breaks_multi_succs(head, &nodes, succs);
        }

        if nodes.len() > 1 {
            for j in 0..i {
                let new_head = self.post_order[j];

                if !nodes.contains(&new_head) {
                    continue;
                }

                let mut new_nodes = self.dom_tree.dominated_by(new_head);
                new_nodes.retain(|n| nodes.contains(n));
                if new_nodes.len() == 1 {
                    continue;
                }

                let region_succs = self.cfg.region_successors(&new_nodes);
                if region_succs.len() < 2 {
                    self.structure_acyclic(new_head, j, &new_nodes);

                    self.dom_tree.remove(new_head, &new_nodes);
                    self.cfg.nodes[new_head].next.clear();

                    if let Some(succ) = region_succs.into_iter().next() {
                        let prev = &mut self.cfg.nodes[succ].prev;
                        let back_edge = prev.iter().any(|edge| edge.back_edge && new_nodes.contains(&edge.node));

                        prev.retain(|edge| !new_nodes.contains(&edge.node));
                        prev.push(Edge {
                            node: new_head,
                            cond: EdgeCond::unconditional(),
                            back_edge,
                        });

                        self.cfg.nodes[new_head].next.push(Edge {
                            node: succ,
                            cond: EdgeCond::unconditional(),
                            back_edge,
                        });
                    }
                }
            }

            let mut new_nodes = self.dom_tree.dominated_by(head);
            new_nodes.retain(|n| nodes.contains(n));
            if new_nodes.len() > 1 {
                self.structure_acyclic(head, i, &new_nodes);
                self.dom_tree.remove(head, &new_nodes);
            }
        }

        self.cfg.nodes[head].next.retain(|e| !nodes.contains(&e.node));

        let code = &mut self.cfg.nodes[head].code;
        let old_code = std::mem::replace(code, Vec::new());
        code.push(Stmt::While(Cond::True, old_code, LoopKind::While));
    }

    fn insert_breaks_single_succ(&mut self, head: usize, nodes: &HashSet<usize>, succ: usize) {
        // Insert break stmts
        for &n in nodes {
            let mut break_conds = Vec::new();

            self.cfg.nodes[n].next.retain(|e| {
                if e.node == succ {
                    break_conds.push(e.cond);
                    false
                } else {
                    e.node != head
                }
            });

            if !break_conds.is_empty() {
                let mut break_cond = Cond::False;
                for cond in break_conds {
                    break_cond = break_cond.or(cond.into());
                }
                break_cond.simplify();
                let break_stmt = if break_cond.is_const_true() {
                    Stmt::Break
                } else {
                    Stmt::If(break_cond, vec![Stmt::Break])
                };
                self.cfg.nodes[n].code.push(break_stmt);
            }
        }

        // Remove stale edges
        let back_edge = self.cfg.nodes[succ]
            .prev
            .iter()
            .any(|edge| edge.back_edge && nodes.contains(&edge.node));
        self.cfg.nodes[head].prev.retain(|e| !e.back_edge);
        self.cfg.nodes[succ].prev.retain(|e| !nodes.contains(&e.node));

        // Add edge from loop to succ
        self.cfg.nodes[head].next.push(Edge {
            cond: EdgeCond::unconditional(),
            node: succ,
            back_edge,
        });
        self.cfg.nodes[succ].prev.push(Edge {
            cond: EdgeCond::unconditional(),
            node: head,
            back_edge,
        });
    }

    fn insert_breaks_multi_succs(&mut self, head: usize, nodes: &HashSet<usize>, succs: HashSet<usize>) {
        let exit_indices: HashMap<_, _> = succs.iter().copied().zip(0..).collect();

        // Insert break stmts
        for &n in nodes {
            let mut break_conds = Vec::new();

            self.cfg.nodes[n].next.retain(|e| {
                if let Some(idx) = exit_indices.get(&e.node) {
                    break_conds.push((idx, e.cond));
                    false
                } else {
                    e.node != head
                }
            });

            if !break_conds.is_empty() {
                for (idx, cond) in break_conds {
                    let break_stmts = vec![
                        Stmt::SetLocal(Var::no_sub(1338_0000), Expr::I32Const(*idx)),
                        Stmt::Break,
                    ];
                    let mut cond: Cond = cond.into();
                    cond.simplify();
                    if cond.is_const_true() {
                        self.cfg.nodes[n].code.extend(break_stmts);
                    } else {
                        self.cfg.nodes[n].code.push(Stmt::If(cond, break_stmts));
                    };
                }
            }
        }

        // Remove stale edges
        self.cfg.nodes[head].prev.retain(|e| !e.back_edge);
        let mut back_edges = HashSet::new();
        for succ in succs {
            if self.cfg.nodes[succ]
                .prev
                .iter()
                .any(|edge| edge.back_edge && nodes.contains(&edge.node))
            {
                back_edges.insert(succ);
            }
            self.cfg.nodes[succ].prev.retain(|e| !nodes.contains(&e.node));
        }

        // Add edges from loop to succs
        let succ_count = exit_indices.len() as u32;
        for (succ, idx) in exit_indices {
            self.next_expr_index += 1;
            self.expr_map
                .insert(self.next_expr_index, Expr::GetLocal(Var::no_sub(1338_0000)));
            let cond = EdgeCond {
                edge_type: if idx == succ_count - 1 {
                    EdgeType::Default(idx)
                } else {
                    EdgeType::CaseRange(idx, idx + 1)
                },
                expr_index: self.next_expr_index,
            };
            self.cfg.nodes[head].next.push(Edge {
                cond,
                node: succ,
                back_edge: back_edges.contains(&succ),
            });
            self.cfg.nodes[succ].prev.push(Edge {
                cond,
                node: head,
                back_edge: back_edges.contains(&succ),
            });
        }
    }

    fn find_loop_region(&self, head: usize) -> (HashSet<usize>, HashSet<usize>) {
        let latch_nodes = self.cfg.nodes[head]
            .prev
            .iter()
            .filter(|e| e.back_edge)
            .map(|e| e.node)
            .collect();
        let mut loop_nodes = self.cfg.graph_slice(head, latch_nodes);
        let mut loop_succs = self.cfg.region_successors(&loop_nodes);

        // TODO: try to find smallest set of nodes so that loop has single succ
        // or alternative: only add the shortest node in each iteration
        while loop_succs.len() > 1 {
            let mut new_loop_nodes = Vec::new();
            let mut new_loop_succs = Vec::new();
            // TODO: visit loops in deterministic order?
            // post_order => include as many as possible?
            // or reverse => as few as possible??
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

        (loop_nodes, loop_succs)
    }
}

pub fn structure(mut cfg: Cfg) -> (Vec<(Var, ValueType)>, Vec<Stmt>) {
    let func_index = cfg.func_index;
    let wasm = std::rc::Rc::clone(&cfg.wasm);
    let post_order = InitDFS::dfs(&cfg);
    let dom_tree = DomTree::build(&cfg);
    let mut expr_map = build_expr_map(&mut cfg);
    let mut code = Structurer {
        cfg,
        post_order,
        dom_tree,
        next_expr_index: expr_map.iter().map(|(idx, _)| *idx).max().unwrap_or(0),
        expr_map: &mut expr_map,
    }
    .structure();

    sidefx_remover::apply(&mut code, &mut expr_map);
    insert_cond_exprs(&mut code, &expr_map);
    flatten_seq(&mut code);

    loop_refinement::apply(&mut code);

    let decls = rename_vars::apply(&mut code, wasm.module(), func_index);
    (decls, code)
}

fn build_expr_map(cfg: &mut Cfg) -> HashMap<u32, Expr> {
    let mut map = HashMap::new();

    for (i, node) in cfg.nodes.iter_mut() {
        if let Some(Stmt::Branch(_)) = node.code.last() {
            if let Some(Stmt::Branch(expr)) = node.code.pop() {
                map.insert(i as u32, expr);
            } else {
                unreachable!();
            }
        }
    }

    map
}

fn insert_cond_exprs(code: &mut Vec<Stmt>, expr_map: &HashMap<u32, Expr>) {
    for stmt in code {
        insert_cond_exprs_stmt(stmt, expr_map);
    }
}

fn insert_cond_exprs_stmt(stmt: &mut Stmt, expr_map: &HashMap<u32, Expr>) {
    use Stmt::*;
    match stmt {
        While(cond, body, _) => {
            cond.insert_exprs(expr_map);
            insert_cond_exprs(body, expr_map);
        }
        If(cond, body) => {
            cond.insert_exprs(expr_map);
            insert_cond_exprs(body, expr_map);
        }
        IfElse(cond, true_body, false_body) => {
            cond.insert_exprs(expr_map);
            insert_cond_exprs(true_body, expr_map);
            insert_cond_exprs(false_body, expr_map);
        }
        SwitchCase(expr, cases, default) => {
            expr.insert_exprs(expr_map);
            for (_, stmt) in cases {
                insert_cond_exprs_stmt(stmt, expr_map);
            }
            if let Some(default_stmt) = default {
                insert_cond_exprs_stmt(default_stmt, expr_map);
            }
        }
        Seq(body) => {
            insert_cond_exprs(body, expr_map);
        }
        _ => (),
    }
}

fn flatten_seq(code: &mut Vec<Stmt>) {
    let mut has_seq = false;

    for stmt in code.iter_mut() {
        match stmt {
            Stmt::While(_, body, _) | Stmt::If(_, body) => flatten_seq(body),
            Stmt::IfElse(_, true_body, false_body) => {
                flatten_seq(true_body);
                flatten_seq(false_body);
            }
            Stmt::Seq(body) => {
                flatten_seq(body);
                has_seq = true
            }
            _ => (),
        }
    }

    if has_seq {
        let old = std::mem::take(code);
        for stmt in old {
            if let Stmt::Seq(body) = stmt {
                for seq_stmt in body {
                    code.push(seq_stmt);
                }
            } else {
                code.push(stmt)
            }
        }
    }
}

struct InitDFS {
    post_order: Vec<usize>,
    visited: HashSet<usize>,
}

impl InitDFS {
    fn dfs(cfg: &Cfg) -> Vec<usize> {
        let mut performer = InitDFS {
            post_order: Vec::with_capacity(cfg.nodes.len()),
            visited: HashSet::new(),
        };
        performer.visited.insert(0);
        performer.visit(cfg, 0);
        performer.post_order
    }

    fn visit(&mut self, cfg: &Cfg, n: usize) {
        for u in cfg.nodes[n].succs() {
            if !self.visited.contains(&u) {
                self.visited.insert(u);
                self.visit(cfg, u);
            }
        }
        self.post_order.push(n);
    }
}
