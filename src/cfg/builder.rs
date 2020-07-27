use std::collections::HashSet;
use std::rc::Rc;

use bwasm::{self, BlockType, Function, Instruction, ValueType};
use slab::Slab;

use super::{BasicBlock, Cfg, Edge, EdgeCond, EdgeType, NodeId};

use crate::ssa::{Expr, Stmt, Var};
use crate::wasm;

#[derive(Debug)]
pub enum CfgBuildError {
    NoSuchFunc,
    FuncIsImported,
}

#[derive(Debug, Clone, Copy)]
struct Reloc {
    node: NodeId,
    edge: usize,
}

impl Reloc {
    const fn new(node: NodeId, edge: usize) -> Self {
        Reloc { node, edge }
    }
    const fn br(node: NodeId) -> Self {
        Reloc::new(node, 0)
    }
    const fn br_true(node: NodeId) -> Self {
        Reloc::new(node, 1)
    }
    const fn br_false(node: NodeId) -> Self {
        Reloc::new(node, 0)
    }
}

#[derive(Debug)]
enum Label {
    /// stack_size, result_type, target
    Bound(usize, BlockType, NodeId),
    /// stack_size, result_type, relocs
    Unbound(usize, BlockType, Vec<Reloc>),
}

#[allow(dead_code)]
enum ResultVars {
    Uninit,
    Init { _i32: Var, _i64: Var, _f32: Var, _f64: Var },
}

impl ResultVars {
    pub fn init(&mut self, idx: u32) -> u32 {
        *self = Self::Init {
            _i32: Var::no_sub(idx),
            _i64: Var::no_sub(idx + 1),
            _f32: Var::no_sub(idx + 2),
            _f64: Var::no_sub(idx + 3),
        };
        idx + 4
    }

    pub fn get(&self, result_type: ValueType) -> Var {
        if let Self::Init { _i32, _i64, _f32, _f64 } = self {
            match result_type {
                ValueType::I32 => *_i32,
                ValueType::I64 => *_i64,
                ValueType::F32 => *_f32,
                ValueType::F64 => *_f64,
            }
        } else {
            panic!("get() on uninitialized ResultVar");
        }
    }
}

struct CfgBuilder {
    nodes: Slab<BasicBlock>,
    stack: Vec<Expr>,
    curr_id: usize,
    next_id: usize,
    unreachable: bool,
    result_vars: ResultVars,
    next_tmp_var_index: u32,
}

impl CfgBuilder {
    fn new() -> Self {
        let mut nodes = Slab::new();
        let curr_id = nodes.insert(BasicBlock::new());
        let next_id = nodes.insert(BasicBlock::new());
        CfgBuilder {
            nodes,
            stack: Vec::new(),
            curr_id,
            next_id,
            unreachable: false,
            result_vars: ResultVars::Uninit,
            next_tmp_var_index: 0,
        }
    }

    /// Add a conditional edge from the current node to succ
    fn push_succ(&mut self, edge_type: EdgeType, succ: NodeId, back_edge: bool) {
        self.nodes[self.curr_id].next.push(Edge {
            cond: EdgeCond {
                expr_index: self.curr_id as u32,
                edge_type,
            },
            node: succ,
            back_edge,
        });
    }

    /// Push the current node into the cfg and create a new current node
    fn push_node(&mut self) {
        self.curr_id = self.next_id;
        self.next_id = self.nodes.insert(BasicBlock::new());
    }

    /// Get the code of the current node
    fn curr_code(&self) -> &[Stmt] {
        &self.nodes[self.curr_id].code
    }

    /// Add a statement to the current node
    fn push_code(&mut self, stmt: Stmt) {
        if !self.unreachable {
            self.nodes[self.curr_id].code.push(stmt);
        }
    }

    /// Push an expression onto the value stack
    fn push(&mut self, expr: Expr) {
        if !self.unreachable {
            self.stack.push(expr);
        }
    }

    /// Pop an expression from the value stack
    fn pop(&mut self) -> Expr {
        if self.unreachable {
            Expr::I32Const(0)
        } else {
            self.stack.pop().unwrap()
        }
    }

    /// Perform and push a load expression onto the value stack
    fn push_load<F: Fn(Box<Expr>) -> Expr>(&mut self, offset: u32, constructor: F) {
        let mut target = self.pop();
        if offset != 0 {
            if let Expr::I32Const(0) = target {
                target = Expr::I32Const(offset);
            } else {
                target = Expr::I32Add(Box::new(target), Box::new(Expr::I32Const(offset)));
            }
        }
        self.tee_tmp(constructor(Box::new(target)));
    }

    /// Perform and push a store expression onto the value stack
    fn push_store<F: Fn(Expr, Expr) -> Stmt>(&mut self, offset: u32, constructor: F) {
        let value = self.pop();
        let mut target = self.pop();
        if offset != 0 {
            if let Expr::I32Const(0) = target {
                target = Expr::I32Const(offset);
            } else {
                target = Expr::I32Add(Box::new(target), Box::new(Expr::I32Const(offset)));
            }
        }
        self.push_code(constructor(target, value));
    }

    /// Perform and push a unariy operation onto the value stack
    fn unop<F: Fn(Box<Expr>) -> Expr>(&mut self, fun: F) {
        let arg = Box::new(self.pop());
        self.push(fun(arg));
    }

    /// Perform and push a binary operation onto the value stack
    fn binop<F: Fn(Box<Expr>, Box<Expr>) -> Expr>(&mut self, fun: F) {
        let b = Box::new(self.pop());
        let a = Box::new(self.pop());
        self.push(fun(a, b));
    }

    /// Store an expression to a temporary var and push that var onto the value stack
    fn tee_tmp(&mut self, expr: Expr) {
        self.push_code(Stmt::SetLocal(Var::no_sub(self.next_tmp_var_index), expr));
        self.push(Expr::GetLocal(Var::no_sub(self.next_tmp_var_index)));
        self.next_tmp_var_index += 1;
    }

    /// Perform a relocation
    fn reloc(&mut self, reloc: Reloc, target: NodeId) {
        self.nodes[reloc.node].next[reloc.edge].node = target;
    }

    fn init_locals(&mut self, func: &Function) {
        for (i, local) in func.locals().iter().enumerate() {
            let i = i + func.func_type().params().len();
            let val = match local {
                ValueType::I32 => Expr::I32Const(0),
                ValueType::I64 => Expr::I64Const(0),
                ValueType::F32 => Expr::F32Const(0.0_f32.to_bits()),
                ValueType::F64 => Expr::F64Const(0.0_f64.to_bits()),
            };
            self.push_code(Stmt::SetLocal(Var::no_sub(i as u32), val));
        }
    }

    fn store_result(&mut self, result_type: ValueType) {
        let result = self.pop();
        let result_var = self.result_vars.get(result_type);
        self.push_code(Stmt::SetLocal(result_var, result));
        self.push(Expr::GetLocal(result_var));
    }

    fn add_br_table_edge(
        &mut self,
        label_stack: &mut Vec<Label>,
        target: u32,
        result_type: &mut BlockType,
        range_start: usize,
        range_end: usize,
        edge_index: usize,
    ) {
        let index = label_stack.len() - target as usize - 1;
        let edge_type = EdgeType::CaseRange(range_start as u32, range_end as u32);
        match label_stack[index] {
            Label::Bound(_, _, target) => self.push_succ(edge_type, target, true),
            Label::Unbound(_, block_type, ref mut relocs) => {
                if let BlockType::Value(_) = block_type {
                    *result_type = block_type;
                }
                relocs.push(Reloc::new(self.curr_id, edge_index));
                self.push_succ(edge_type, 0, false);
            }
        }
    }

    /// Build the cfg for the function `func_index` in `module`
    #[allow(clippy::cognitive_complexity)]
    fn build(mut self, wasm: Rc<wasm::Instance>, func_index: u32) -> Result<Cfg, CfgBuildError> {
        let func = if let Some(func) = wasm.module().get_func(func_index) {
            if func.is_imported() {
                return Err(CfgBuildError::FuncIsImported);
            }
            func
        } else {
            return Err(CfgBuildError::NoSuchFunc);
        };

        let code = func.instructions();
        let return_type = func.func_type().return_type();
        let mut label_stack = Vec::new();

        self.init_locals(func);
        let nxt_var_idx = func.func_type().params().len() + func.locals().len();
        let nxt_var_idx = self.result_vars.init(nxt_var_idx as u32);
        self.next_tmp_var_index = nxt_var_idx;

        for instr in code {
            use self::Instruction::*;

            match instr {
                Nop => (),
                Block(block_type) => {
                    label_stack.push(Label::Unbound(self.stack.len(), *block_type, Vec::new()));
                    if !self.curr_code().is_empty() {
                        self.push_succ(EdgeType::Unconditional, self.next_id, false);
                        self.push_node();
                    }
                }
                Loop(block_type) => {
                    if !self.curr_code().is_empty() {
                        self.push_succ(EdgeType::Unconditional, self.next_id, false);
                        self.push_node();
                    }
                    label_stack.push(Label::Bound(self.stack.len(), *block_type, self.curr_id));
                }

                If(block_type) => {
                    label_stack.push(Label::Unbound(
                        self.stack.len(),
                        *block_type,
                        vec![Reloc::br_false(self.curr_id)],
                    ));
                    let cond = self.pop();
                    self.push_code(Stmt::Branch(cond));
                    self.push_succ(EdgeType::Conditional(false), 0, false);
                    self.push_succ(EdgeType::Conditional(true), self.next_id, false);
                    self.push_node();
                }
                Else => match label_stack.last_mut().unwrap() {
                    Label::Unbound(stack_size, block_type, ref mut relocs) => {
                        if let BlockType::Value(result_type) = block_type {
                            self.store_result(*result_type);
                            self.pop();
                            self.stack.truncate(*stack_size);
                        }
                        self.reloc(relocs[0], self.next_id);
                        relocs[0] = Reloc::br(self.curr_id);
                        self.push_succ(EdgeType::Unconditional, 0, false);
                        self.push_node();
                        self.unreachable = false;
                    }
                    _ => unreachable!(),
                },

                End => {
                    if let Some(label) = label_stack.pop() {
                        match label {
                            Label::Bound(stack_size, block_type, _) => {
                                if let BlockType::Value(result_type) = block_type {
                                    self.store_result(result_type);
                                    let result = self.pop();
                                    self.stack.truncate(stack_size);
                                    self.unreachable = false;
                                    self.push(result);
                                }
                                if !self.curr_code().is_empty() {
                                    self.push_succ(EdgeType::Unconditional, self.next_id, false);
                                    self.push_node();
                                }
                            }
                            Label::Unbound(stack_size, block_type, relocs) => {
                                if let BlockType::Value(result_type) = block_type {
                                    self.store_result(result_type);
                                    let result = self.pop();
                                    self.stack.truncate(stack_size);
                                    self.unreachable = false;
                                    self.push(result);
                                }
                                if !self.curr_code().is_empty() {
                                    self.push_succ(EdgeType::Unconditional, self.next_id, false);
                                    self.push_node();
                                }
                                if !relocs.is_empty() {
                                    let target = self.curr_id;
                                    relocs.iter().for_each(|reloc| self.reloc(*reloc, target));
                                }
                            }
                        }
                    } else if !self.unreachable && return_type.is_some() {
                        let result = self.pop();
                        self.push_code(Stmt::Return(result));
                    }
                    self.unreachable = false;
                }

                Br(target) => {
                    let index = label_stack.len() - *target as usize - 1;
                    match label_stack[index] {
                        Label::Bound(_, _, target) => {
                            self.push_succ(EdgeType::Unconditional, target, true);
                        }
                        Label::Unbound(_, block_type, ref mut relocs) => {
                            if let BlockType::Value(result_type) = block_type {
                                self.store_result(result_type);
                            }
                            relocs.push(Reloc::br(self.curr_id));
                            self.push_succ(EdgeType::Unconditional, 0, false);
                        }
                    }
                    self.push_node();
                    self.unreachable = true;
                }
                BrIf(target) => {
                    let index = label_stack.len() - *target as usize - 1;
                    let cond = self.pop();
                    match label_stack[index] {
                        Label::Bound(_, _, target) => {
                            self.push_code(Stmt::Branch(cond));
                            self.push_succ(EdgeType::Conditional(false), self.next_id, false);
                            self.push_succ(EdgeType::Conditional(true), target, true);
                        }
                        Label::Unbound(_, block_type, ref mut relocs) => {
                            if let BlockType::Value(result_type) = block_type {
                                self.store_result(result_type);
                            }
                            self.push_code(Stmt::Branch(cond));
                            relocs.push(Reloc::br_true(self.curr_id));
                            self.push_succ(EdgeType::Conditional(false), self.next_id, false);
                            self.push_succ(EdgeType::Conditional(true), 0, false);
                        }
                    }
                    self.push_node();
                }
                BrTable(table_data) => {
                    let mut result_type = BlockType::NoResult;
                    let mut last_target = table_data.table.get(0).copied().unwrap_or_default();
                    let mut curr_start = 0;
                    let mut curr_edge = 0;
                    let mut default_lower_bound = table_data.table.len() as u32;

                    for (i, &target) in table_data.table.iter().enumerate() {
                        if target == last_target {
                            continue;
                        } else {
                            self.add_br_table_edge(
                                &mut label_stack,
                                last_target,
                                &mut result_type,
                                curr_start,
                                i,
                                curr_edge,
                            );
                            curr_edge += 1;
                            curr_start = i;
                        }

                        last_target = target;
                    }

                    if let Some(&target) = table_data.table.last() {
                        if target == table_data.default {
                            default_lower_bound = curr_start as u32;
                        } else {
                            self.add_br_table_edge(
                                &mut label_stack,
                                target,
                                &mut result_type,
                                curr_start,
                                table_data.table.len(),
                                curr_edge,
                            );
                            curr_edge += 1;
                        }
                    }

                    let index = label_stack.len() - table_data.default as usize - 1;
                    match label_stack[index] {
                        Label::Bound(_, _, target) => {
                            self.push_succ(EdgeType::Default(default_lower_bound), target, true);
                        }
                        Label::Unbound(_, block_type, ref mut reloc) => {
                            if let BlockType::Value(_) = block_type {
                                result_type = block_type;
                            }
                            reloc.push(Reloc::new(self.curr_id, curr_edge));
                            self.push_succ(EdgeType::Default(default_lower_bound), 0, false);
                        }
                    }

                    let cond = self.pop();
                    if let BlockType::Value(result_type) = result_type {
                        self.store_result(result_type);
                    }
                    self.push_code(Stmt::Branch(cond));
                    self.push_node();
                    self.unreachable = true;
                }
                Return => {
                    if return_type.is_some() {
                        let result = self.pop();
                        self.push_code(Stmt::Return(result));
                    } else {
                        self.push_code(Stmt::ReturnVoid);
                    }
                    self.push_node();
                    self.unreachable = true;
                }
                Unreachable => {
                    self.push_code(Stmt::Unreachable);
                    self.push_node();
                    self.unreachable = true;
                }

                Call(index) => {
                    let func_type = wasm.module().func(*index).func_type();
                    let params_count = func_type.params().len();
                    let mut args = Vec::with_capacity(params_count);

                    for _ in 0..params_count {
                        args.push(self.pop());
                    }
                    args.reverse();

                    if func_type.return_type().is_none() {
                        self.push_code(Stmt::Expr(Expr::Call(*index, args)));
                    } else {
                        self.tee_tmp(Expr::Call(*index, args));
                    }
                }
                CallIndirect(signature, _) => {
                    let index_expr = Box::new(self.pop());
                    let func_type = &wasm.module().types()[*signature as usize];
                    let params_count = func_type.params().len();
                    let mut args = Vec::with_capacity(params_count);

                    for _ in 0..params_count {
                        args.push(self.pop());
                    }
                    args.reverse();

                    let call_expr = Expr::CallIndirect(index_expr, args, *signature);
                    if func_type.return_type().is_none() {
                        self.push_code(Stmt::Expr(call_expr));
                    } else {
                        self.tee_tmp(call_expr);
                    }
                }

                Drop => {
                    self.pop();
                }
                Select => {
                    let cond = Box::new(self.pop());
                    let b = Box::new(self.pop());
                    let a = Box::new(self.pop());
                    self.push(Expr::Select(cond, a, b));
                }

                GetLocal(index) => self.tee_tmp(Expr::GetLocal(Var::no_sub(*index))),
                SetLocal(index) => {
                    let expr = self.pop();
                    self.push_code(Stmt::SetLocal(Var::no_sub(*index), expr));
                }
                TeeLocal(index) => {
                    let expr = self.pop();
                    self.push_code(Stmt::SetLocal(Var::no_sub(*index), expr));
                    self.tee_tmp(Expr::GetLocal(Var::no_sub(*index)));
                }

                GetGlobal(index) => self.tee_tmp(Expr::GetGlobal(*index)),
                SetGlobal(index) => {
                    let expr = self.pop();
                    self.push_code(Stmt::SetGlobal(*index, expr));
                }

                I32Load(_flag, offset) => self.push_load(*offset, Expr::I32Load),
                I64Load(_flag, offset) => self.push_load(*offset, Expr::I64Load),
                F32Load(_flag, offset) => self.push_load(*offset, Expr::F32Load),
                F64Load(_flag, offset) => self.push_load(*offset, Expr::F64Load),
                I32Load8S(_flag, offset) => self.push_load(*offset, Expr::I32Load8S),
                I32Load8U(_flag, offset) => self.push_load(*offset, Expr::I32Load8U),
                I32Load16S(_flag, offset) => self.push_load(*offset, Expr::I32Load16S),
                I32Load16U(_flag, offset) => self.push_load(*offset, Expr::I32Load16U),
                I64Load8S(_flag, offset) => self.push_load(*offset, Expr::I64Load8S),
                I64Load8U(_flag, offset) => self.push_load(*offset, Expr::I64Load8U),
                I64Load16S(_flag, offset) => self.push_load(*offset, Expr::I64Load16S),
                I64Load16U(_flag, offset) => self.push_load(*offset, Expr::I64Load16U),
                I64Load32S(_flag, offset) => self.push_load(*offset, Expr::I64Load32S),
                I64Load32U(_flag, offset) => self.push_load(*offset, Expr::I64Load32U),

                I32Store(_flag, offset) => self.push_store(*offset, Stmt::I32Store),
                I64Store(_flag, offset) => self.push_store(*offset, Stmt::I64Store),
                F32Store(_flag, offset) => self.push_store(*offset, Stmt::F32Store),
                F64Store(_flag, offset) => self.push_store(*offset, Stmt::F64Store),
                I32Store8(_flag, offset) => self.push_store(*offset, Stmt::I32Store8),
                I32Store16(_flag, offset) => self.push_store(*offset, Stmt::I32Store16),
                I64Store8(_flag, offset) => self.push_store(*offset, Stmt::I64Store8),
                I64Store16(_flag, offset) => self.push_store(*offset, Stmt::I64Store16),
                I64Store32(_flag, offset) => self.push_store(*offset, Stmt::I64Store32),

                CurrentMemory(_) => self.tee_tmp(Expr::MemorySize),
                GrowMemory(_) => {
                    let diff = Box::new(self.pop());
                    self.tee_tmp(Expr::MemoryGrow(diff));
                }

                I32Const(val) => self.push(Expr::I32Const(*val as u32)),
                I64Const(val) => self.push(Expr::I64Const(*val as u64)),
                F32Const(val) => self.push(Expr::F32Const(*val as u32)),
                F64Const(val) => self.push(Expr::F64Const(*val as u64)),

                I32Eqz => self.unop(Expr::I32Eqz),
                I32Eq => self.binop(Expr::I32Eq),
                I32Ne => self.binop(Expr::I32Ne),
                I32LtS => self.binop(Expr::I32LtS),
                I32LtU => self.binop(Expr::I32LtU),
                I32GtS => self.binop(Expr::I32GtS),
                I32GtU => self.binop(Expr::I32GtU),
                I32LeS => self.binop(Expr::I32LeS),
                I32LeU => self.binop(Expr::I32LeU),
                I32GeS => self.binop(Expr::I32GeS),
                I32GeU => self.binop(Expr::I32GeU),
                I64Eqz => self.unop(Expr::I64Eqz),
                I64Eq => self.binop(Expr::I64Eq),
                I64Ne => self.binop(Expr::I64Ne),
                I64LtS => self.binop(Expr::I64LtS),
                I64LtU => self.binop(Expr::I64LtU),
                I64GtS => self.binop(Expr::I64GtS),
                I64GtU => self.binop(Expr::I64GtU),
                I64LeS => self.binop(Expr::I64LeS),
                I64LeU => self.binop(Expr::I64LeU),
                I64GeS => self.binop(Expr::I64GeS),
                I64GeU => self.binop(Expr::I64GeU),

                F32Eq => self.binop(Expr::F32Eq),
                F32Ne => self.binop(Expr::F32Ne),
                F32Lt => self.binop(Expr::F32Lt),
                F32Gt => self.binop(Expr::F32Gt),
                F32Le => self.binop(Expr::F32Le),
                F32Ge => self.binop(Expr::F32Ge),

                F64Eq => self.binop(Expr::F64Eq),
                F64Ne => self.binop(Expr::F64Ne),
                F64Lt => self.binop(Expr::F64Lt),
                F64Gt => self.binop(Expr::F64Gt),
                F64Le => self.binop(Expr::F64Le),
                F64Ge => self.binop(Expr::F64Ge),

                I32Clz => self.unop(Expr::I32Clz),
                I32Ctz => self.unop(Expr::I32Ctz),
                I32Popcnt => self.unop(Expr::I32Popcnt),
                I32Add => self.binop(Expr::I32Add),
                I32Sub => {
                    let b = Box::new(self.pop());
                    let a = self.pop();
                    if let Expr::I32Const(0) = a {
                        self.push(Expr::I32Neg(b));
                    } else {
                        self.push(Expr::I32Sub(Box::new(a), b));
                    }
                }
                I32Mul => self.binop(Expr::I32Mul),
                I32DivS => self.binop(Expr::I32DivS),
                I32DivU => self.binop(Expr::I32DivU),
                I32RemS => self.binop(Expr::I32RemS),
                I32RemU => self.binop(Expr::I32RemU),
                I32And => self.binop(Expr::I32And),
                I32Or => self.binop(Expr::I32Or),
                I32Xor => self.binop(Expr::I32Xor),
                I32Shl => self.binop(Expr::I32Shl),
                I32ShrS => self.binop(Expr::I32ShrS),
                I32ShrU => self.binop(Expr::I32ShrU),
                I32Rotl => self.binop(Expr::I32Rotl),
                I32Rotr => self.binop(Expr::I32Rotr),

                I64Clz => self.unop(Expr::I64Clz),
                I64Ctz => self.unop(Expr::I64Ctz),
                I64Popcnt => self.unop(Expr::I64Popcnt),
                I64Add => self.binop(Expr::I64Add),
                I64Sub => {
                    let b = Box::new(self.pop());
                    let a = self.pop();
                    if let Expr::I64Const(0) = a {
                        self.push(Expr::I64Neg(b));
                    } else {
                        self.push(Expr::I64Sub(Box::new(a), b));
                    }
                }
                I64Mul => self.binop(Expr::I64Mul),
                I64DivS => self.binop(Expr::I64DivS),
                I64DivU => self.binop(Expr::I64DivU),
                I64RemS => self.binop(Expr::I64RemS),
                I64RemU => self.binop(Expr::I64RemU),
                I64And => self.binop(Expr::I64And),
                I64Or => self.binop(Expr::I64Or),
                I64Xor => self.binop(Expr::I64Xor),
                I64Shl => self.binop(Expr::I64Shl),
                I64ShrS => self.binop(Expr::I64ShrS),
                I64ShrU => self.binop(Expr::I64ShrU),
                I64Rotl => self.binop(Expr::I64Rotl),
                I64Rotr => self.binop(Expr::I64Rotr),

                F32Abs => self.unop(Expr::F32Abs),
                F32Neg => self.unop(Expr::F32Neg),
                F32Ceil => self.unop(Expr::F32Ceil),
                F32Floor => self.unop(Expr::F32Floor),
                F32Trunc => self.unop(Expr::F32Trunc),
                F32Nearest => self.unop(Expr::F32Nearest),
                F32Sqrt => self.unop(Expr::F32Sqrt),
                F32Add => self.binop(Expr::F32Add),
                F32Sub => self.binop(Expr::F32Sub),
                F32Mul => self.binop(Expr::F32Mul),
                F32Div => self.binop(Expr::F32Div),
                F32Min => self.binop(Expr::F32Min),
                F32Max => self.binop(Expr::F32Max),
                F32Copysign => self.binop(Expr::F32Copysign),

                F64Abs => self.unop(Expr::F64Abs),
                F64Neg => self.unop(Expr::F64Neg),
                F64Ceil => self.unop(Expr::F64Ceil),
                F64Floor => self.unop(Expr::F64Floor),
                F64Trunc => self.unop(Expr::F64Trunc),
                F64Nearest => self.unop(Expr::F64Nearest),
                F64Sqrt => self.unop(Expr::F64Sqrt),
                F64Add => self.binop(Expr::F64Add),
                F64Sub => self.binop(Expr::F64Sub),
                F64Mul => self.binop(Expr::F64Mul),
                F64Div => self.binop(Expr::F64Div),
                F64Min => self.binop(Expr::F64Min),
                F64Max => self.binop(Expr::F64Max),
                F64Copysign => self.binop(Expr::F64Copysign),

                I32WrapI64 => self.unop(Expr::I32WrapI64),
                I32TruncSF32 => self.unop(Expr::I32TruncSF32),
                I32TruncUF32 => self.unop(Expr::I32TruncUF32),
                I32TruncSF64 => self.unop(Expr::I32TruncSF64),
                I32TruncUF64 => self.unop(Expr::I32TruncUF64),
                I64ExtendSI32 => self.unop(Expr::I64ExtendSI32),
                I64ExtendUI32 => self.unop(Expr::I64ExtendUI32),
                I64TruncSF32 => self.unop(Expr::I64TruncSF32),
                I64TruncUF32 => self.unop(Expr::I64TruncUF32),
                I64TruncSF64 => self.unop(Expr::I64TruncSF64),
                I64TruncUF64 => self.unop(Expr::I64TruncUF64),
                F32ConvertSI32 => self.unop(Expr::F32ConvertSI32),
                F32ConvertUI32 => self.unop(Expr::F32ConvertUI32),
                F32ConvertSI64 => self.unop(Expr::F32ConvertSI64),
                F32ConvertUI64 => self.unop(Expr::F32ConvertUI64),
                F32DemoteF64 => self.unop(Expr::F32DemoteF64),
                F64ConvertSI32 => self.unop(Expr::F64ConvertSI32),
                F64ConvertUI32 => self.unop(Expr::F64ConvertUI32),
                F64ConvertSI64 => self.unop(Expr::F64ConvertSI64),
                F64ConvertUI64 => self.unop(Expr::F64ConvertUI64),
                F64PromoteF32 => self.unop(Expr::F64PromoteF32),

                I32ReinterpretF32 => self.unop(Expr::I32ReinterpretF32),
                I64ReinterpretF64 => self.unop(Expr::I64ReinterpretF64),
                F32ReinterpretI32 => self.unop(Expr::F32ReinterpretI32),
                F64ReinterpretI64 => self.unop(Expr::F64ReinterpretI64),
            }
        }

        self.remove_unreachable();
        self.compute_prev();
        self.remove_empty();
        Ok(Cfg {
            wasm,
            func_index,
            nodes: self.nodes,
        })
    }

    fn remove_unreachable(&mut self) {
        let mut visited = HashSet::new();
        let mut todo = Vec::new();
        visited.insert(0);
        todo.push(0);

        while let Some(n) = todo.pop() {
            for u in self.nodes[n].succs() {
                if !visited.contains(&u) {
                    visited.insert(u);
                    todo.push(u);
                }
            }
        }

        self.nodes.retain(|id, _| visited.contains(&id));
        self.nodes.shrink_to_fit();
    }

    fn remove_empty(&mut self) {
        for i in 1..self.nodes.capacity() {
            if let Some(n) = self.nodes.get(i) {
                if n.code.is_empty() && n.next.len() == 1 {
                    let edge = n.next[0];
                    let succ = edge.node;
                    let back_edge = edge.back_edge;
                    let mut new_edges = Vec::new();

                    if succ == i {
                        continue;
                    }

                    for pred in 0..n.prev.len() {
                        let pred = self.nodes[i].prev[pred].node;
                        for edge in &mut self.nodes[pred].next {
                            if edge.node == i {
                                edge.back_edge = back_edge;
                                edge.node = succ;
                                new_edges.push(Edge {
                                    cond: edge.cond,
                                    node: pred,
                                    back_edge,
                                });
                            }
                        }
                    }

                    self.nodes[succ].prev.retain(|e| e.node != i);
                    self.nodes[succ].prev.extend(new_edges);
                    self.nodes.remove(i);
                }
            }
        }
    }

    fn compute_prev(&mut self) {
        let ids: Vec<NodeId> = self.nodes.iter().map(|(i, _)| i).collect();
        for n in ids {
            for edge in self.nodes[n].next.clone() {
                self.nodes[edge.node].prev.push(Edge {
                    cond: edge.cond,
                    node: n,
                    back_edge: edge.back_edge,
                });
            }
        }
    }
}

pub fn build(wasm: Rc<wasm::Instance>, func_index: u32) -> Result<Cfg, CfgBuildError> {
    CfgBuilder::new().build(wasm, func_index)
}
