use std::collections::HashSet;

use crate::cfg::{Cfg, NodeId};

pub struct DomTree {
    pub idom: Vec<usize>,
    pub succs: Vec<Vec<usize>>,
}

impl DomTree {
    /// Find nodes that are dominated by n
    pub fn dominated_by(&self, n: usize) -> HashSet<usize> {
        let mut result = HashSet::new();
        let mut todo = vec![n];

        while let Some(u) = todo.pop() {
            result.insert(u);
            for &s in &self.succs[u] {
                todo.push(s);
            }
        }

        result
    }

    /// compute if n dominates w strictly
    pub fn dominates_strictly(&self, n: usize, mut w: usize) -> bool {
        if n == 0 {
            return w != 0;
        }
        while w != 0 {
            w = self.idom[w];
            if n == w {
                return true;
            }
        }
        false
    }

    pub fn remove(&mut self, head: usize, nodes: &HashSet<usize>) {
        self.succs[head] = self.find_nodes_to_keep(head, nodes);
        for &succ in &self.succs[head] {
            self.idom[succ] = head;
        }
    }

    fn find_nodes_to_keep(&self, head: usize, nodes: &HashSet<usize>) -> Vec<usize> {
        if !nodes.contains(&head) {
            return vec![head];
        }

        let mut nodes_to_keep = Vec::new();
        for &succ in &self.succs[head] {
            nodes_to_keep.extend(self.find_nodes_to_keep(succ, nodes));
        }

        nodes_to_keep
    }

    pub fn build(cfg: &Cfg) -> Self {
        DomTreeBuilder::new(cfg).build()
    }
}

struct DomTreeBuilder<'a> {
    cfg: &'a Cfg,
    rev_edges: Vec<Vec<usize>>,
    bucket: Vec<Vec<usize>>,
    index: Vec<usize>,
    rev_index: Vec<usize>,
    parent: Vec<usize>,
    idom: Vec<usize>,
    sdom: Vec<usize>,
    dsu: Vec<usize>,
    label: Vec<usize>,
    next_index: usize,
}

impl<'a> DomTreeBuilder<'a> {
    fn new(cfg: &'a Cfg) -> DomTreeBuilder<'a> {
        DomTreeBuilder {
            cfg,
            rev_edges: vec![Vec::new(); cfg.nodes.capacity()],
            bucket: vec![Vec::new(); cfg.nodes.capacity()],
            index: vec![0; cfg.nodes.capacity()],
            rev_index: vec![0; cfg.nodes.capacity()],
            parent: vec![0; cfg.nodes.capacity()],
            idom: vec![0; cfg.nodes.capacity()],
            sdom: vec![0; cfg.nodes.capacity()],
            dsu: vec![0; cfg.nodes.capacity()],
            label: vec![0; cfg.nodes.capacity()],
            next_index: 0,
        }
    }

    fn initial_dfs(&mut self, u: usize) {
        let index = self.next_index;
        self.next_index += 1;
        self.index[u] = index;
        self.rev_index[index] = u;
        self.idom[index] = index;
        self.sdom[index] = index;
        self.dsu[index] = index;
        self.label[index] = index;

        for w in self.cfg.nodes[u].succs() {
            if self.index[w] == 0 && w != 0 {
                self.initial_dfs(w);
                self.parent[self.index[w]] = index;
            }
            self.rev_edges[self.index[w]].push(index);
        }
    }

    fn find(&mut self, u: usize) -> usize {
        self.find_internal(u, 0).unwrap()
    }

    fn find_internal(&mut self, u: usize, x: usize) -> Option<usize> {
        if u == self.dsu[u] {
            return if x == 0 { Some(u) } else { None };
        }

        let v = self.find_internal(self.dsu[u], x + 1);
        if v.is_none() {
            return Some(u);
        }
        let v = v.unwrap();

        if self.sdom[self.label[self.dsu[u]]] < self.sdom[self.label[u]] {
            self.label[u] = self.label[self.dsu[u]];
        }

        self.dsu[u] = v;

        Some(if x == 0 { self.label[u] } else { v })
    }

    fn build(mut self) -> DomTree {
        self.initial_dfs(0);

        for i in (0..self.cfg.nodes.len()).rev() {
            for j in 0..self.rev_edges[i].len() {
                let v = self.find(self.rev_edges[i][j]);
                let x = self.sdom[v];
                if x < self.sdom[i] {
                    self.sdom[i] = x;
                }
            }

            if i > 0 {
                self.bucket[self.sdom[i]].push(i);
            }

            for j in 0..self.bucket[i].len() {
                let w = self.bucket[i][j];
                let v = self.find(w);
                if self.sdom[v] == self.sdom[w] {
                    self.idom[w] = self.sdom[w];
                } else {
                    self.idom[w] = v;
                }
            }

            if i > 0 {
                self.dsu[i] = self.parent[i];
            }
        }

        let mut dom_tree = vec![Vec::new(); self.cfg.nodes.capacity()];

        for i in 1..self.cfg.nodes.len() {
            if self.idom[i] != self.sdom[i] {
                self.idom[i] = self.idom[self.idom[i]];
            }

            dom_tree[self.rev_index[self.idom[i]]].push(self.rev_index[i]);
        }

        let mut idom = vec![0; self.idom.len()];
        for (i, dom) in self.idom.iter().enumerate() {
            idom[self.rev_index[i]] = self.rev_index[*dom];
        }
        idom[0] = std::usize::MAX;

        DomTree { idom, succs: dom_tree }
    }
}

struct DomFrontierBuilder<'a> {
    cfg: &'a Cfg,
    dom_tree: &'a DomTree,
    df: Vec<Vec<NodeId>>,
}

impl<'a> DomFrontierBuilder<'a> {
    fn new(cfg: &'a Cfg, dom_tree: &'a DomTree) -> DomFrontierBuilder<'a> {
        DomFrontierBuilder {
            cfg,
            dom_tree,
            df: vec![Vec::new(); cfg.nodes.capacity()],
        }
    }

    fn compute_df(&mut self, n: NodeId) {
        for succ in self.cfg.nodes[n].succs() {
            if self.dom_tree.idom[succ] != n {
                self.df[n].push(succ);
            }
        }

        for i in 0..self.dom_tree.succs[n].len() {
            let dom = self.dom_tree.succs[n][i];
            self.compute_df(dom);
            for j in 0..self.df[dom].len() {
                let w = self.df[dom][j];
                if !self.dom_tree.dominates_strictly(n, w) {
                    self.df[n].push(w);
                }
            }
        }
    }

    fn build(mut self) -> Vec<Vec<NodeId>> {
        self.compute_df(0);
        self.df
    }
}

pub fn compute_dom_frontier(cfg: &Cfg, dom_tree: &DomTree) -> Vec<Vec<NodeId>> {
    DomFrontierBuilder::new(cfg, dom_tree).build()
}
