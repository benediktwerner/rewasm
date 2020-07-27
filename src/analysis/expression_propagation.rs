use std::collections::HashSet;

use crate::cfg::{Cfg, InstrPos};
use crate::ssa::{Expr, Stmt, Var};

use super::used_vars;
use super::DefUseMap;

#[derive(Debug, Clone, Copy)]
struct ExprProperties {
    contains_mem_ref: bool,
    contains_global: bool,
    contains_call: bool,
    complexity: u32,
}

impl ExprProperties {
    fn compute(expr: &Expr) -> Self {
        Self {
            contains_mem_ref: contains_memory_ref(expr),
            contains_global: contains_global(expr),
            contains_call: contains_call(expr),
            complexity: expr.complexity(),
        }
    }

    fn can_propagate_over_expr(self, expr: &Expr) -> bool {
        if (self.contains_call || self.contains_mem_ref || self.contains_global) && contains_call(expr) {
            return false;
        }
        if (self.contains_call || self.contains_mem_ref) && contains_memory_ref(expr) {
            return false;
        }
        if (self.contains_call || self.contains_mem_ref) && contains_global(expr) {
            return false;
        }
        true
    }

    fn can_propagate_over_stmt(self, stmt: &Stmt) -> bool {
        use Stmt::*;
        match stmt {
            SetGlobal(_, expr) => !self.contains_call && !self.contains_global && self.can_propagate_over_expr(expr),
            SetLocal(_, expr) | Expr(expr) | Return(expr) | Branch(expr) => self.can_propagate_over_expr(expr),
            I32Store(location, value)
            | I64Store(location, value)
            | F32Store(location, value)
            | F64Store(location, value)
            | I32Store8(location, value)
            | I32Store16(location, value)
            | I64Store8(location, value)
            | I64Store16(location, value)
            | I64Store32(location, value) => {
                !self.contains_call
                    && !self.contains_mem_ref
                    && self.can_propagate_over_expr(location)
                    && self.can_propagate_over_expr(value)
            }
            While(..) | ForLoop(..) | If(..) | IfElse(..) | SwitchCase(..) | Seq(..) => unreachable!(),
            Phi(..) | Nop | Break | ReturnVoid | Unreachable => true,
        }
    }
}

pub fn propagate_expressions(cfg: &mut Cfg, DefUseMap(def_map, use_map): &mut DefUseMap) {
    let mut done = false;
    let mut defs: Vec<_> = def_map.iter().collect();
    defs.sort_unstable_by_key(|(_, pos)| *pos);

    // TODO: does this need to be a loop?
    while !done {
        done = true;
        for (var, def_pos) in defs.iter().copied() {
            if !use_map.contains_key(var) {
                continue;
            }

            let def_stmt = cfg.stmt(*def_pos);
            match def_stmt {
                Stmt::SetLocal(_, def_expr) => {
                    let properties = ExprProperties::compute(def_expr);
                    if properties.contains_call && use_map[var].len() > 1 {
                        continue;
                    }

                    let def_expr = def_expr.clone();
                    let mut processed = HashSet::new();

                    for &use_pos in &use_map[var] {
                        if can_propagate(cfg, &def_expr, *def_pos, use_pos, *var, properties) {
                            replace_all(cfg.stmt_mut(use_pos), *var, &def_expr);
                            processed.insert(use_pos);
                            done = false;
                        }
                    }

                    use_map.get_mut(var).unwrap().retain(|pos| !processed.contains(pos));
                    for v in used_vars::find(&def_expr) {
                        use_map.get_mut(&v).unwrap().extend(processed.iter().copied());
                    }

                    if properties.contains_call && use_map[var].is_empty() {
                        // Call was propagated to it's single use.
                        // Remove it at the definition so that the variable gets removed by DCE.
                        *cfg.stmt_mut(*def_pos) = Stmt::SetLocal(*var, Expr::True);
                    }
                }
                Stmt::Phi(..) => (),
                other => unreachable!("Invalid defining stmt: {:?}", other),
            }
        }
    }
}

fn can_propagate(
    cfg: &Cfg,
    def_expr: &Expr,
    def_pos: InstrPos,
    use_pos: InstrPos,
    var: Var,
    properties: ExprProperties,
) -> bool {
    let use_stmt = cfg.stmt(use_pos);
    match use_stmt {
        Stmt::Phi(..) => return false,
        Stmt::Branch(..) => {
            if properties.contains_call {
                return false;
            }
        }
        _ => (),
    }
    if properties.contains_call && count_var_occ(use_stmt, var) > 1
        || properties.complexity > 3
        || properties.complexity > 1 && properties.complexity + use_stmt.complexity() > 7
        || !properties.can_propagate_over_stmt(use_stmt)
    {
        return false;
    }

    let used_vars = used_vars::find(def_expr).iter().map(|v| v.index).collect();
    if def_pos.node == use_pos.node {
        let code = &cfg.nodes[def_pos.node].code[def_pos.instr..use_pos.instr];
        return can_propagate_over(code, &used_vars, properties);
    } else {
        let code = &cfg.nodes[def_pos.node].code[def_pos.instr..];
        if !can_propagate_over(code, &used_vars, properties) {
            return false;
        }

        let code = &cfg.nodes[use_pos.node].code[..use_pos.instr];
        if !can_propagate_over(code, &used_vars, properties) {
            return false;
        }

        let mut targets = HashSet::new();
        targets.insert(use_pos.node);
        for node in cfg.graph_slice(def_pos.node, targets) {
            if node == def_pos.node || node == use_pos.node {
                continue;
            }
            if !can_propagate_over(code, &used_vars, properties) {
                return false;
            }
        }
    }

    true
}

fn can_propagate_over(code: &[Stmt], used_vars: &HashSet<u32>, properties: ExprProperties) -> bool {
    for stmt in code {
        use Stmt::*;
        match stmt {
            Phi(var, _) => {
                if used_vars.contains(&var.index) {
                    return false;
                }
            }
            SetLocal(var, expr) => {
                if used_vars.contains(&var.index) {
                    return false;
                }
                if !properties.can_propagate_over_expr(expr) {
                    return false;
                }
            }
            _ => {
                if !properties.can_propagate_over_stmt(stmt) {
                    return false;
                }
            }
        }
    }
    true
}

macro_rules! fn_contains {
    ($fn_name:ident, $($p:pat),+) => {
        #[allow(unreachable_patterns)]
        fn $fn_name(expr: &Expr) -> bool {
            use Expr::*;
            match expr {
                $($p => true),+,
                Select(cond, true_expr, false_expr) => {
                    $fn_name(cond)
                        || $fn_name(true_expr)
                        || $fn_name(false_expr)
                }
                MemoryGrow(expr)
                | I32Eqz(expr)
                | I64Eqz(expr)
                | I32Clz(expr)
                | I32Ctz(expr)
                | I32Popcnt(expr)
                | I64Clz(expr)
                | I64Ctz(expr)
                | I64Popcnt(expr)
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
                | F64ReinterpretI64(expr) => $fn_name(expr),

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
                | F64Copysign(left, right) => $fn_name(left) || $fn_name(right),
                _ => false,
            }
        }
    }
}

fn_contains!(
    contains_memory_ref,
    MemorySize,
    MemoryGrow(_),
    I32Load(_),
    I64Load(_),
    F32Load(_),
    F64Load(_),
    I32Load8S(_),
    I32Load8U(_),
    I32Load16S(_),
    I32Load16U(_),
    I64Load8S(_),
    I64Load8U(_),
    I64Load16S(_),
    I64Load16U(_),
    I64Load32S(_),
    I64Load32U(_)
);

fn_contains!(contains_call, Call(..), CallIndirect(..));
fn_contains!(contains_global, GetGlobal(..));

fn count_var_occ(stmt: &Stmt, var: Var) -> u32 {
    use Stmt::*;
    match stmt {
        Expr(expr) | Return(expr) | SetLocal(_, expr) | SetGlobal(_, expr) | Branch(expr) => {
            count_var_occ_expr(expr, var)
        }
        I32Store(location, value)
        | I64Store(location, value)
        | F32Store(location, value)
        | F64Store(location, value)
        | I32Store8(location, value)
        | I32Store16(location, value)
        | I64Store8(location, value)
        | I64Store16(location, value)
        | I64Store32(location, value) => count_var_occ_expr(location, var) + count_var_occ_expr(value, var),
        While(..) | ForLoop(..) | If(..) | IfElse(..) | SwitchCase(..) | Seq(..) => unreachable!(),
        Nop => 0,
        Break => 0,
        ReturnVoid => 0,
        Unreachable => 0,
        Phi(v, args) => {
            if v.index == var.index && args.iter().any(|s| *s == var.subscript) {
                1
            } else {
                0
            }
        }
    }
}

fn count_var_occ_expr(expr: &Expr, var: Var) -> u32 {
    use Expr::*;
    match expr {
        True => 0,
        Select(cond, true_expr, false_expr) => {
            count_var_occ_expr(cond, var) + count_var_occ_expr(true_expr, var) + count_var_occ_expr(false_expr, var)
        }

        Call(_, args) => args.iter().map(|arg| count_var_occ_expr(arg, var)).sum(),
        CallIndirect(index, args, _) => {
            count_var_occ_expr(index, var) + args.iter().map(|arg| count_var_occ_expr(arg, var)).sum::<u32>()
        }

        MemorySize => 0,
        MemoryGrow(expr) => count_var_occ_expr(expr, var),
        I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr) | I32Load8U(expr)
        | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr) | I64Load16S(expr)
        | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => count_var_occ_expr(expr, var),

        GetLocal(v) => {
            if *v == var {
                1
            } else {
                0
            }
        }
        GetGlobal(_) => 0,

        I32Const(_) | I64Const(_) | F32Const(_) | F64Const(_) => 0,

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
        | F64ReinterpretI64(expr) => count_var_occ_expr(expr, var),

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
        | F64Copysign(left, right) => count_var_occ_expr(left, var) + count_var_occ_expr(right, var),
    }
}

fn replace_all(use_stmt: &mut Stmt, var: Var, def_expr: &Expr) {
    use Stmt::*;
    match use_stmt {
        Expr(expr) | Return(expr) | Branch(expr) | SetLocal(_, expr) | SetGlobal(_, expr) => {
            replace_all_expr(expr, var, def_expr)
        }

        I32Store(location, value)
        | I64Store(location, value)
        | F32Store(location, value)
        | F64Store(location, value)
        | I32Store8(location, value)
        | I32Store16(location, value)
        | I64Store8(location, value)
        | I64Store16(location, value)
        | I64Store32(location, value) => {
            replace_all_expr(location, var, def_expr);
            replace_all_expr(value, var, def_expr);
        }
        Nop => unreachable!(),
        Phi(..) => unreachable!(),
        Unreachable => unreachable!(),
        ReturnVoid => unreachable!(),
        While(..) => unreachable!(),
        ForLoop(..) => unreachable!(),
        Break => unreachable!(),
        If(..) => unreachable!(),
        IfElse(..) => unreachable!(),
        SwitchCase(..) => unreachable!(),
        Seq(..) => unreachable!(),
    }
}

fn replace_all_expr(use_expr: &mut Expr, var: Var, def_expr: &Expr) {
    use Expr::*;
    match use_expr {
        True => (),
        Select(cond, true_expr, false_expr) => {
            replace_all_expr(cond, var, def_expr);
            replace_all_expr(true_expr, var, def_expr);
            replace_all_expr(false_expr, var, def_expr);
        }

        Call(_, args) => {
            for arg in args {
                replace_all_expr(arg, var, def_expr);
            }
        }
        CallIndirect(index, args, _) => {
            replace_all_expr(index, var, def_expr);
            for arg in args {
                replace_all_expr(arg, var, def_expr);
            }
        }

        MemorySize => (),
        MemoryGrow(expr) => replace_all_expr(expr, var, def_expr),
        I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr) | I32Load8U(expr)
        | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr) | I64Load16S(expr)
        | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => replace_all_expr(expr, var, def_expr),

        GetLocal(v) => {
            if *v == var {
                *use_expr = def_expr.clone();
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
        | F64ReinterpretI64(expr) => replace_all_expr(expr, var, def_expr),

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
            replace_all_expr(left, var, def_expr);
            replace_all_expr(right, var, def_expr);
        }
    }
}
