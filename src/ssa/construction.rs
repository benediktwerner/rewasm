use std::collections::{HashMap, HashSet};

use crate::analysis::DefUseMap;
use crate::cfg::{Cfg, InstrPos};
use crate::dominance::{self, DomTree};

use super::Expr;
use super::Stmt;
use super::Var;

fn add_var_def(defs: &mut HashMap<u32, Vec<usize>>, var_index: u32, def_index: usize) {
    defs.entry(var_index).or_default().push(def_index);
}

fn compute_var_defs(cfg: &Cfg) -> HashMap<u32, Vec<usize>> {
    let mut result = HashMap::new();

    for (i, node) in cfg.nodes.iter() {
        for instr in &node.code {
            if let Stmt::SetLocal(var, _) = instr {
                add_var_def(&mut result, var.index, i);
            }
        }
    }

    result
}

struct SSATransformer {
    dom_tree: DomTree,
    dom_frontier: Vec<Vec<usize>>,
    count: HashMap<u32, u32>,
    stack: HashMap<u32, Vec<u32>>,
    pop_stack: Vec<HashSet<u32>>,
    def_use_map: DefUseMap,
}

impl SSATransformer {
    fn place_phi_nodes(&mut self, cfg: &mut Cfg, defs: &HashMap<u32, Vec<usize>>) {
        let mut phi: HashMap<u32, HashSet<usize>> = HashMap::new();
        let arg_count = cfg.wasm.module().func(cfg.func_index).param_count();

        for (var, defsites) in defs {
            if *var >= arg_count && defsites.len() == 1 {
                continue;
            }

            phi.insert(*var, HashSet::new());
            let mut worklist = defsites.clone();
            while let Some(n) = worklist.pop() {
                for y in &self.dom_frontier[n] {
                    if cfg.nodes[*y].prev.len() <= 1 && *y != 0 {
                        continue;
                    }
                    if !phi[var].contains(y) {
                        cfg.nodes[*y].code.insert(0, Stmt::phi(*var as u32));
                        phi.get_mut(var).unwrap().insert(*y);

                        if !defsites.contains(y) {
                            worklist.push(*y);
                        }
                    }
                }
            }
        }
    }

    fn rename_in_expr(&mut self, expr: &mut Expr, pos: InstrPos) {
        use Expr::*;
        match expr {
            True => (),
            Select(cond, true_expr, false_expr) => {
                self.rename_in_expr(cond, pos);
                self.rename_in_expr(true_expr, pos);
                self.rename_in_expr(false_expr, pos);
            }
            Call(_, args) => {
                for arg in args {
                    self.rename_in_expr(arg, pos);
                }
            }
            CallIndirect(expr, args, _) => {
                self.rename_in_expr(expr, pos);
                for arg in args {
                    self.rename_in_expr(arg, pos);
                }
            }
            MemorySize => (),
            MemoryGrow(expr) | I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr)
            | I32Load8U(expr) | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr)
            | I64Load16S(expr) | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => {
                self.rename_in_expr(expr, pos)
            }

            GetLocal(ref mut var) => {
                let i = self.stack.get(&var.index).map_or(0, |s| *s.last().unwrap());
                var.subscript = i;
                self.def_use_map.1.entry(*var).or_default().insert(pos);
            }
            GetGlobal(_) => (),
            I32Const(_) | I64Const(_) | F32Const(_) | F64Const(_) => (),

            I32Eqz(expr)
            | I64Eqz(expr)
            | I32Clz(expr)
            | I32Ctz(expr)
            | I32Popcnt(expr)
            | I32Neg(expr)
            | I64Clz(expr)
            | I64Ctz(expr)
            | I64Popcnt(expr)
            | I64Neg(expr)
            | F32Abs(expr)
            | F32Neg(expr)
            | F32Ceil(expr)
            | F32Floor(expr)
            | F32Trunc(expr)
            | F32Nearest(expr)
            | F32Sqrt(expr)
            | F64Abs(expr)
            | F64Neg(expr)
            | F64Ceil(expr)
            | F64Floor(expr)
            | F64Trunc(expr)
            | F64Nearest(expr)
            | F64Sqrt(expr)
            | I32WrapI64(expr)
            | I32TruncSF32(expr)
            | I32TruncUF32(expr)
            | I32TruncSF64(expr)
            | I32TruncUF64(expr)
            | I64ExtendSI32(expr)
            | I64ExtendUI32(expr)
            | I64TruncSF32(expr)
            | I64TruncUF32(expr)
            | I64TruncSF64(expr)
            | I64TruncUF64(expr)
            | F32ConvertSI32(expr)
            | F32ConvertUI32(expr)
            | F32ConvertSI64(expr)
            | F32ConvertUI64(expr)
            | F32DemoteF64(expr)
            | F64ConvertSI32(expr)
            | F64ConvertUI32(expr)
            | F64ConvertSI64(expr)
            | F64ConvertUI64(expr)
            | F64PromoteF32(expr)
            | I32ReinterpretF32(expr)
            | I64ReinterpretF64(expr)
            | F32ReinterpretI32(expr)
            | F64ReinterpretI64(expr) => self.rename_in_expr(expr, pos),

            I32Eq(left, right)
            | I32Ne(left, right)
            | I32LtS(left, right)
            | I32LtU(left, right)
            | I32GtS(left, right)
            | I32GtU(left, right)
            | I32LeS(left, right)
            | I32LeU(left, right)
            | I32GeS(left, right)
            | I32GeU(left, right)
            | I64Eq(left, right)
            | I64Ne(left, right)
            | I64LtS(left, right)
            | I64LtU(left, right)
            | I64GtS(left, right)
            | I64GtU(left, right)
            | I64LeS(left, right)
            | I64LeU(left, right)
            | I64GeS(left, right)
            | I64GeU(left, right)
            | F32Eq(left, right)
            | F32Ne(left, right)
            | F32Lt(left, right)
            | F32Gt(left, right)
            | F32Le(left, right)
            | F32Ge(left, right)
            | F64Eq(left, right)
            | F64Ne(left, right)
            | F64Lt(left, right)
            | F64Gt(left, right)
            | F64Le(left, right)
            | F64Ge(left, right)
            | I32Add(left, right)
            | I32Sub(left, right)
            | I32Mul(left, right)
            | I32DivS(left, right)
            | I32DivU(left, right)
            | I32RemS(left, right)
            | I32RemU(left, right)
            | I32And(left, right)
            | I32Or(left, right)
            | I32Xor(left, right)
            | I32Shl(left, right)
            | I32ShrS(left, right)
            | I32ShrU(left, right)
            | I32Rotl(left, right)
            | I32Rotr(left, right)
            | I64Add(left, right)
            | I64Sub(left, right)
            | I64Mul(left, right)
            | I64DivS(left, right)
            | I64DivU(left, right)
            | I64RemS(left, right)
            | I64RemU(left, right)
            | I64And(left, right)
            | I64Or(left, right)
            | I64Xor(left, right)
            | I64Shl(left, right)
            | I64ShrS(left, right)
            | I64ShrU(left, right)
            | I64Rotl(left, right)
            | I64Rotr(left, right)
            | F32Add(left, right)
            | F32Sub(left, right)
            | F32Mul(left, right)
            | F32Div(left, right)
            | F32Min(left, right)
            | F32Max(left, right)
            | F32Copysign(left, right)
            | F64Add(left, right)
            | F64Sub(left, right)
            | F64Mul(left, right)
            | F64Div(left, right)
            | F64Min(left, right)
            | F64Max(left, right)
            | F64Copysign(left, right) => {
                self.rename_in_expr(left, pos);
                self.rename_in_expr(right, pos);
            }
        }
    }

    fn rename_var(&mut self, var: &mut Var, pos: InstrPos) {
        let i = self.count[&var.index] + 1;
        *self.count.get_mut(&var.index).unwrap() = i;
        var.subscript = i;

        self.def_use_map.0.insert(*var, pos);

        let pop_stack = self.pop_stack.last_mut().unwrap();
        if pop_stack.contains(&var.index) {
            *self.stack.get_mut(&var.index).unwrap().last_mut().unwrap() = i;
        } else {
            self.stack.get_mut(&var.index).unwrap().push(i);
            self.pop_stack.last_mut().unwrap().insert(var.index);
        }
    }

    fn rename_in_stmt(&mut self, stmt: &mut Stmt, pos: InstrPos) {
        use Stmt::*;
        match stmt {
            Expr(expr) | Return(expr) | Branch(expr) => self.rename_in_expr(expr, pos),
            SetLocal(ref mut var, expr) => {
                self.rename_in_expr(expr, pos);
                self.rename_var(var, pos);
            }
            Phi(ref mut var, _) => self.rename_var(var, pos),
            SetGlobal(_, expr) => self.rename_in_expr(expr, pos),
            I32Store(location, value)
            | I64Store(location, value)
            | F32Store(location, value)
            | F64Store(location, value)
            | I32Store8(location, value)
            | I32Store16(location, value)
            | I64Store8(location, value)
            | I64Store16(location, value)
            | I64Store32(location, value) => {
                self.rename_in_expr(location, pos);
                self.rename_in_expr(value, pos);
            }
            Unreachable => (),
            ReturnVoid => (),
            Nop => (),
            While(..) => unreachable!(),
            ForLoop(..) => unreachable!(),
            Break => unreachable!(),
            If(..) => unreachable!(),
            IfElse(..) => unreachable!(),
            SwitchCase(..) => unreachable!(),
            Seq(..) => unreachable!(),
        }
    }

    fn rename(&mut self, cfg: &mut Cfg, n: usize) {
        self.pop_stack.push(HashSet::new());

        // Rename vars in node n
        for (i, stmt) in cfg.nodes[n].code.iter_mut().enumerate() {
            self.rename_in_stmt(stmt, InstrPos::new(n, i));
        }

        // Rename vars in phi nodes in successors of n
        let succs: Vec<_> = cfg.nodes[n].succs().collect();
        for succ in succs {
            // let pred_idx = cfg.nodes[succ].preds().position(|u| u == n).unwrap();
            for (i, stmt) in cfg.nodes[succ].code.iter_mut().enumerate() {
                if let Stmt::Phi(var, ref mut args) = stmt {
                    let j = self.stack[&var.index].last().unwrap();
                    // args[pred_idx] = *j;
                    // TODO: this ignores the order of the predecessors. is this ok?
                    args.push(*j);
                    self.def_use_map
                        .1
                        .entry(Var::new(var.index, *j))
                        .or_default()
                        .insert(InstrPos::new(succ, i));
                } else {
                    break;
                }
            }
        }

        // Continue in nodes dominated by n
        for i in 0..self.dom_tree.succs[n].len() {
            let succ = self.dom_tree.succs[n][i];
            self.rename(cfg, succ);
        }

        for var in self.pop_stack.pop().unwrap() {
            self.stack.get_mut(&var).unwrap().pop();
        }
    }
}

pub fn transform_to_ssa(cfg: &mut Cfg) -> DefUseMap {
    let var_defs = compute_var_defs(cfg);
    let dom_tree = DomTree::build(cfg);
    let dom_frontier = dominance::compute_dom_frontier(cfg, &dom_tree);

    let mut stack = HashMap::new();
    let mut count = HashMap::new();
    for var in var_defs.keys() {
        stack.insert(*var, vec![0]);
        count.insert(*var, 0);
    }

    let mut transformer = SSATransformer {
        dom_tree,
        dom_frontier,
        count,
        stack,
        pop_stack: Vec::new(),
        def_use_map: DefUseMap::new(),
    };

    transformer.place_phi_nodes(cfg, &var_defs);
    transformer.rename(cfg, 0);

    transformer.def_use_map
}
