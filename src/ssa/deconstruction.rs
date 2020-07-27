use crate::cfg::Cfg;

use super::{Expr, Stmt};

pub fn transform_out_of_ssa(cfg: &mut Cfg) {
    for (_, node) in cfg.nodes.iter_mut() {
        node.code.retain(|stmt| match stmt {
            Stmt::Phi(..) => false,
            Stmt::Nop => false,
            _ => true,
        });

        for stmt in node.code.iter_mut() {
            remove_subscripts_in_stmt(stmt);
        }
    }
}

fn remove_subscripts_in_stmt(stmt: &mut Stmt) {
    use Stmt::*;
    match stmt {
        Expr(expr) | Return(expr) | Branch(expr) => remove_subscripts_in_expr(expr),
        SetLocal(ref mut var, expr) => {
            remove_subscripts_in_expr(expr);
            var.subscript = 0;
        }
        SetGlobal(_, expr) => remove_subscripts_in_expr(expr),
        I32Store(location, value)
        | I64Store(location, value)
        | F32Store(location, value)
        | F64Store(location, value)
        | I32Store8(location, value)
        | I32Store16(location, value)
        | I64Store8(location, value)
        | I64Store16(location, value)
        | I64Store32(location, value) => {
            remove_subscripts_in_expr(location);
            remove_subscripts_in_expr(value);
        }
        Unreachable => (),
        ReturnVoid => (),
        Nop => (),
        Phi(..) => unreachable!(),
        While(..) => unreachable!(),
        ForLoop(..) => unreachable!(),
        Break => unreachable!(),
        If(..) => unreachable!(),
        IfElse(..) => unreachable!(),
        SwitchCase(..) => unreachable!(),
        Seq(..) => unreachable!(),
    }
}

fn remove_subscripts_in_expr(expr: &mut Expr) {
    use Expr::*;
    match expr {
        True => (),
        Select(cond, true_expr, false_expr) => {
            remove_subscripts_in_expr(cond);
            remove_subscripts_in_expr(true_expr);
            remove_subscripts_in_expr(false_expr);
        }
        Call(_, args) => {
            for arg in args {
                remove_subscripts_in_expr(arg);
            }
        }
        CallIndirect(expr, args, _) => {
            remove_subscripts_in_expr(expr);
            for arg in args {
                remove_subscripts_in_expr(arg);
            }
        }
        MemorySize => (),
        MemoryGrow(expr) | I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr)
        | I32Load8U(expr) | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr)
        | I64Load16S(expr) | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => remove_subscripts_in_expr(expr),

        GetLocal(ref mut var) => var.subscript = 0,
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
        | F64ReinterpretI64(expr) => remove_subscripts_in_expr(expr),

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
            remove_subscripts_in_expr(left);
            remove_subscripts_in_expr(right);
        }
    }
}
