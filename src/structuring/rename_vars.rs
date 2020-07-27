use std::collections::HashMap;

use crate::ssa::{cond::MappedExpr, Cond, Expr, Stmt, Var};
use bwasm::{Module, ValueType};

pub fn apply(code: &mut Vec<Stmt>, module: &Module, func_index: u32) -> Vec<(Var, ValueType)> {
    let mut renamer = Renamer::new(module, func_index);
    renamer.rename(code);
    renamer.decls
}

struct Renamer<'a> {
    decls: Vec<(Var, ValueType)>,
    var_types: HashMap<Var, ValueType>,
    name_map: HashMap<Var, Var>,
    module: &'a Module,
    next_index: u32,
}

impl<'a> Renamer<'a> {
    pub fn new(module: &'a Module, func_index: u32) -> Self {
        let func_type = module.func(func_index).func_type();
        let mut var_types = HashMap::new();
        let mut name_map = HashMap::new();
        for (i, arg) in func_type.params().iter().enumerate() {
            let var = Var::no_sub(i as u32);
            var_types.insert(var, *arg);
            name_map.insert(var, var);
        }
        Renamer {
            decls: Vec::new(),
            var_types,
            name_map,
            next_index: func_type.params().len() as u32,
            module,
        }
    }

    fn rename_var(&mut self, var: &mut Var, var_type: ValueType) {
        *var = if let Some(new_var) = self.name_map.get(var) {
            *new_var
        } else {
            let new_var = Var::no_sub(self.next_index);
            self.next_index += 1;
            self.name_map.insert(*var, new_var);
            self.var_types.insert(new_var, var_type);
            self.decls.push((new_var, var_type));
            new_var
        };
    }

    pub fn rename(&mut self, code: &mut Vec<Stmt>) {
        for stmt in code {
            self.rename_stmt(stmt);
        }
    }

    pub fn rename_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Expr(expr) | Return(expr) | Branch(expr) => self.rename_expr(expr),
            SetLocal(ref mut var, expr) => {
                self.rename_expr(expr);
                let var_type = expr.result_type(self.module, &self.var_types);
                self.rename_var(var, var_type);
            }
            SetGlobal(_, expr) => self.rename_expr(expr),
            I32Store(location, value)
            | I64Store(location, value)
            | F32Store(location, value)
            | F64Store(location, value)
            | I32Store8(location, value)
            | I32Store16(location, value)
            | I64Store8(location, value)
            | I64Store16(location, value)
            | I64Store32(location, value) => {
                self.rename_expr(location);
                self.rename_expr(value);
            }
            While(cond, body, _) | If(cond, body) => {
                // Rename body first in case of do-while
                self.rename(body);
                self.rename_cond(cond);
            }
            ForLoop(var, init, cond, post, body) => {
                if let Some(init) = init {
                    self.rename_expr(init);
                    let var_type = init.result_type(self.module, &self.var_types);
                    self.rename_var(var, var_type);
                }
                self.rename_expr(post);
                self.rename(body);
                self.rename_cond(cond);
            }
            IfElse(cond, if_body, else_body) => {
                self.rename_cond(cond);
                self.rename(if_body);
                self.rename(else_body);
            }
            SwitchCase(expr, cases, default) => {
                match expr {
                    MappedExpr::Expr(expr) => self.rename_expr(expr),
                    MappedExpr::Const(_) => (),
                    MappedExpr::Mapped(_) => unreachable!(),
                }
                for (_, stmt) in cases.iter_mut() {
                    self.rename_stmt(stmt);
                }
                if let Some(default_stmt) = default {
                    self.rename_stmt(default_stmt);
                }
            }
            Seq(body) => self.rename(body),
            Nop => (),
            Break => (),
            ReturnVoid => (),
            Unreachable => (),
            Phi(..) => unreachable!(),
        }
    }

    fn rename_cond(&mut self, cond: &mut Cond) {
        use Cond::*;
        match cond {
            True | False => (),
            Not(cond) => self.rename_cond(cond),
            And(a, b) | Or(a, b) => {
                self.rename_cond(a);
                self.rename_cond(b);
            }
            Cmp(a, _, b) => {
                if let MappedExpr::Expr(expr) = a {
                    self.rename_expr(expr);
                }
                if let MappedExpr::Expr(expr) = b {
                    self.rename_expr(expr);
                }
            }
            Expr(expr) => match expr {
                MappedExpr::Expr(expr) => self.rename_expr(expr),
                MappedExpr::Const(_) => (),
                MappedExpr::Mapped(_) => unreachable!(),
            },
        }
    }

    fn rename_expr(&mut self, expr: &mut Expr) {
        use Expr::*;
        match expr {
            True => (),
            Select(cond, true_expr, false_expr) => {
                self.rename_expr(cond);
                self.rename_expr(true_expr);
                self.rename_expr(false_expr);
            }

            Call(_, args) => {
                for arg in args {
                    self.rename_expr(arg);
                }
            }
            CallIndirect(expr, args, _) => {
                self.rename_expr(expr);
                for arg in args {
                    self.rename_expr(arg);
                }
            }

            MemorySize => (),
            MemoryGrow(expr) => self.rename_expr(expr),

            I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr) | I32Load8U(expr)
            | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr) | I64Load16S(expr)
            | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => self.rename_expr(expr),

            GetLocal(var) => {
                if let Some(new_var) = self.name_map.get(var) {
                    *var = *new_var;
                }
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
            | F64ReinterpretI64(expr) => self.rename_expr(expr),

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
                self.rename_expr(left);
                self.rename_expr(right);
            }
        }
    }
}
