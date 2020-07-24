use std::{collections::HashSet, hash::BuildHasher};

use crate::ssa::{Expr, Var};

pub fn find(expr: &Expr) -> HashSet<Var> {
    let mut result = HashSet::new();
    find_and_add(expr, &mut result);
    result
}

pub fn find_and_add<S: BuildHasher>(expr: &Expr, result: &mut HashSet<Var, S>) {
    use Expr::*;
    match expr {
        True => (),
        Select(cond, true_expr, false_expr) => {
            find_and_add(cond, result);
            find_and_add(true_expr, result);
            find_and_add(false_expr, result);
        }

        Call(_, args) => {
            for arg in args {
                find_and_add(arg, result);
            }
        }
        CallIndirect(index, args, _) => {
            find_and_add(index, result);
            for arg in args {
                find_and_add(arg, result);
            }
        }

        MemorySize => (),
        MemoryGrow(expr) => find_and_add(expr, result),
        I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr) | I32Load8U(expr)
        | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr) | I64Load16S(expr)
        | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => find_and_add(expr, result),

        GetLocal(var) => {
            result.insert(*var);
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
        | F64ReinterpretI64(expr) => find_and_add(expr, result),

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
            find_and_add(left, result);
            find_and_add(right, result);
        }
    }
}
