use std::collections::HashMap;

use crate::fmt;
use crate::wasm::TableElement;
use bwasm::ValueType;

use super::Var;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    True,

    // cond, if, else
    Select(Box<Expr>, Box<Expr>, Box<Expr>),

    // func_index, args
    Call(u32, Vec<Expr>),
    // index, targets, args, type_ref
    CallIndirect(Box<Expr>, Vec<Expr>, u32),

    MemorySize,
    MemoryGrow(Box<Expr>),

    I32Load(Box<Expr>),
    I64Load(Box<Expr>),
    F32Load(Box<Expr>),
    F64Load(Box<Expr>),
    I32Load8S(Box<Expr>),
    I32Load8U(Box<Expr>),
    I32Load16S(Box<Expr>),
    I32Load16U(Box<Expr>),
    I64Load8S(Box<Expr>),
    I64Load8U(Box<Expr>),
    I64Load16S(Box<Expr>),
    I64Load16U(Box<Expr>),
    I64Load32S(Box<Expr>),
    I64Load32U(Box<Expr>),

    GetLocal(Var),
    GetGlobal(u32),
    I32Const(u32),
    I64Const(u64),
    F32Const(u32),
    F64Const(u64),

    I32Eqz(Box<Expr>),
    I32Eq(Box<Expr>, Box<Expr>),
    I32Ne(Box<Expr>, Box<Expr>),
    I32LtS(Box<Expr>, Box<Expr>),
    I32LtU(Box<Expr>, Box<Expr>),
    I32GtS(Box<Expr>, Box<Expr>),
    I32GtU(Box<Expr>, Box<Expr>),
    I32LeS(Box<Expr>, Box<Expr>),
    I32LeU(Box<Expr>, Box<Expr>),
    I32GeS(Box<Expr>, Box<Expr>),
    I32GeU(Box<Expr>, Box<Expr>),

    I64Eqz(Box<Expr>),
    I64Eq(Box<Expr>, Box<Expr>),
    I64Ne(Box<Expr>, Box<Expr>),
    I64LtS(Box<Expr>, Box<Expr>),
    I64LtU(Box<Expr>, Box<Expr>),
    I64GtS(Box<Expr>, Box<Expr>),
    I64GtU(Box<Expr>, Box<Expr>),
    I64LeS(Box<Expr>, Box<Expr>),
    I64LeU(Box<Expr>, Box<Expr>),
    I64GeS(Box<Expr>, Box<Expr>),
    I64GeU(Box<Expr>, Box<Expr>),

    F32Eq(Box<Expr>, Box<Expr>),
    F32Ne(Box<Expr>, Box<Expr>),
    F32Lt(Box<Expr>, Box<Expr>),
    F32Gt(Box<Expr>, Box<Expr>),
    F32Le(Box<Expr>, Box<Expr>),
    F32Ge(Box<Expr>, Box<Expr>),

    F64Eq(Box<Expr>, Box<Expr>),
    F64Ne(Box<Expr>, Box<Expr>),
    F64Lt(Box<Expr>, Box<Expr>),
    F64Gt(Box<Expr>, Box<Expr>),
    F64Le(Box<Expr>, Box<Expr>),
    F64Ge(Box<Expr>, Box<Expr>),

    I32Clz(Box<Expr>),
    I32Ctz(Box<Expr>),
    I32Popcnt(Box<Expr>),
    I32Neg(Box<Expr>),
    I32Add(Box<Expr>, Box<Expr>),
    I32Sub(Box<Expr>, Box<Expr>),
    I32Mul(Box<Expr>, Box<Expr>),
    I32DivS(Box<Expr>, Box<Expr>),
    I32DivU(Box<Expr>, Box<Expr>),
    I32RemS(Box<Expr>, Box<Expr>),
    I32RemU(Box<Expr>, Box<Expr>),
    I32And(Box<Expr>, Box<Expr>),
    I32Or(Box<Expr>, Box<Expr>),
    I32Xor(Box<Expr>, Box<Expr>),
    I32Shl(Box<Expr>, Box<Expr>),
    I32ShrS(Box<Expr>, Box<Expr>),
    I32ShrU(Box<Expr>, Box<Expr>),
    I32Rotl(Box<Expr>, Box<Expr>),
    I32Rotr(Box<Expr>, Box<Expr>),

    I64Clz(Box<Expr>),
    I64Ctz(Box<Expr>),
    I64Popcnt(Box<Expr>),
    I64Neg(Box<Expr>),
    I64Add(Box<Expr>, Box<Expr>),
    I64Sub(Box<Expr>, Box<Expr>),
    I64Mul(Box<Expr>, Box<Expr>),
    I64DivS(Box<Expr>, Box<Expr>),
    I64DivU(Box<Expr>, Box<Expr>),
    I64RemS(Box<Expr>, Box<Expr>),
    I64RemU(Box<Expr>, Box<Expr>),
    I64And(Box<Expr>, Box<Expr>),
    I64Or(Box<Expr>, Box<Expr>),
    I64Xor(Box<Expr>, Box<Expr>),
    I64Shl(Box<Expr>, Box<Expr>),
    I64ShrS(Box<Expr>, Box<Expr>),
    I64ShrU(Box<Expr>, Box<Expr>),
    I64Rotl(Box<Expr>, Box<Expr>),
    I64Rotr(Box<Expr>, Box<Expr>),

    F32Abs(Box<Expr>),
    F32Neg(Box<Expr>),
    F32Ceil(Box<Expr>),
    F32Floor(Box<Expr>),
    F32Trunc(Box<Expr>),
    F32Nearest(Box<Expr>),
    F32Sqrt(Box<Expr>),
    F32Add(Box<Expr>, Box<Expr>),
    F32Sub(Box<Expr>, Box<Expr>),
    F32Mul(Box<Expr>, Box<Expr>),
    F32Div(Box<Expr>, Box<Expr>),
    F32Min(Box<Expr>, Box<Expr>),
    F32Max(Box<Expr>, Box<Expr>),
    F32Copysign(Box<Expr>, Box<Expr>),

    F64Abs(Box<Expr>),
    F64Neg(Box<Expr>),
    F64Ceil(Box<Expr>),
    F64Floor(Box<Expr>),
    F64Trunc(Box<Expr>),
    F64Nearest(Box<Expr>),
    F64Sqrt(Box<Expr>),
    F64Add(Box<Expr>, Box<Expr>),
    F64Sub(Box<Expr>, Box<Expr>),
    F64Mul(Box<Expr>, Box<Expr>),
    F64Div(Box<Expr>, Box<Expr>),
    F64Min(Box<Expr>, Box<Expr>),
    F64Max(Box<Expr>, Box<Expr>),
    F64Copysign(Box<Expr>, Box<Expr>),

    I32WrapI64(Box<Expr>),
    I32TruncSF32(Box<Expr>),
    I32TruncUF32(Box<Expr>),
    I32TruncSF64(Box<Expr>),
    I32TruncUF64(Box<Expr>),
    I64ExtendSI32(Box<Expr>),
    I64ExtendUI32(Box<Expr>),
    I64TruncSF32(Box<Expr>),
    I64TruncUF32(Box<Expr>),
    I64TruncSF64(Box<Expr>),
    I64TruncUF64(Box<Expr>),
    F32ConvertSI32(Box<Expr>),
    F32ConvertUI32(Box<Expr>),
    F32ConvertSI64(Box<Expr>),
    F32ConvertUI64(Box<Expr>),
    F32DemoteF64(Box<Expr>),
    F64ConvertSI32(Box<Expr>),
    F64ConvertUI32(Box<Expr>),
    F64ConvertSI64(Box<Expr>),
    F64ConvertUI64(Box<Expr>),
    F64PromoteF32(Box<Expr>),

    I32ReinterpretF32(Box<Expr>),
    I64ReinterpretF64(Box<Expr>),
    F32ReinterpretI32(Box<Expr>),
    F64ReinterpretI64(Box<Expr>),
}

impl Expr {
    pub fn complexity(&self) -> u32 {
        use Expr::*;
        match self {
            True => 1,
            Select(cond, true_expr, false_expr) => {
                1 + cond.complexity() + true_expr.complexity() + false_expr.complexity()
            }

            Call(_, args) => args.iter().map(|arg| arg.complexity()).sum::<u32>() + 1,
            CallIndirect(..) => 100,

            MemorySize => 1,
            MemoryGrow(expr) => 1 + expr.complexity(),
            I32Load(expr) | I64Load(expr) | F32Load(expr) | F64Load(expr) | I32Load8S(expr) | I32Load8U(expr)
            | I32Load16S(expr) | I32Load16U(expr) | I64Load8S(expr) | I64Load8U(expr) | I64Load16S(expr)
            | I64Load16U(expr) | I64Load32S(expr) | I64Load32U(expr) => 1 + expr.complexity(),

            GetLocal(_) | GetGlobal(_) => 1,

            I32Const(_) | I64Const(_) | F32Const(_) | F64Const(_) => 1,

            I32Eqz(expr) | I64Eqz(expr) | I32Clz(expr) | I32Ctz(expr) | I32Popcnt(expr) | I32Neg(expr)
            | I64Clz(expr) | I64Ctz(expr) | I64Popcnt(expr) | I64Neg(expr) | F32Abs(expr) | F32Neg(expr)
            | F32Ceil(expr) | F32Floor(expr) | F32Trunc(expr) | F32Nearest(expr) | F32Sqrt(expr) | F64Abs(expr)
            | F64Neg(expr) | F64Ceil(expr) | F64Floor(expr) | F64Trunc(expr) | F64Nearest(expr) | F64Sqrt(expr)
            | I32WrapI64(expr) => 1 + expr.complexity(),

            I32TruncSF32(expr)
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
            | F64ReinterpretI64(expr) => 1 + expr.complexity(),

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
            | F64Copysign(left, right) => left.complexity() + 1 + right.complexity(),
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            Expr::True => 0,

            Expr::Select(..) => 13,

            Expr::Call(..) => 0,
            Expr::CallIndirect(..) => 0,
            Expr::MemorySize => 0,
            Expr::MemoryGrow(..) => 0,

            Expr::I32Load(..) => 0,
            Expr::I64Load(..) => 0,
            Expr::F32Load(..) => 0,
            Expr::F64Load(..) => 0,
            Expr::I32Load8S(..) => 0,
            Expr::I32Load8U(..) => 0,
            Expr::I32Load16S(..) => 0,
            Expr::I32Load16U(..) => 0,
            Expr::I64Load8S(..) => 0,
            Expr::I64Load8U(..) => 0,
            Expr::I64Load16S(..) => 0,
            Expr::I64Load16U(..) => 0,
            Expr::I64Load32S(..) => 0,
            Expr::I64Load32U(..) => 0,

            Expr::GetLocal(..) => 0,
            Expr::GetGlobal(..) => 0,
            Expr::I32Const(..) => 0,
            Expr::I64Const(..) => 0,
            Expr::F32Const(..) => 0,
            Expr::F64Const(..) => 0,

            Expr::I32Eqz(..) => 7,
            Expr::I32Eq(..) => 7,
            Expr::I32Ne(..) => 7,
            Expr::I32LtS(..) => 6,
            Expr::I32LtU(..) => 6,
            Expr::I32GtS(..) => 6,
            Expr::I32GtU(..) => 6,
            Expr::I32LeS(..) => 6,
            Expr::I32LeU(..) => 6,
            Expr::I32GeS(..) => 6,
            Expr::I32GeU(..) => 6,

            Expr::I64Eqz(..) => 7,
            Expr::I64Eq(..) => 7,
            Expr::I64Ne(..) => 7,
            Expr::I64LtS(..) => 6,
            Expr::I64LtU(..) => 6,
            Expr::I64GtS(..) => 6,
            Expr::I64GtU(..) => 6,
            Expr::I64LeS(..) => 6,
            Expr::I64LeU(..) => 6,
            Expr::I64GeS(..) => 6,
            Expr::I64GeU(..) => 6,

            Expr::F32Eq(..) => 7,
            Expr::F32Ne(..) => 7,
            Expr::F32Lt(..) => 6,
            Expr::F32Gt(..) => 6,
            Expr::F32Le(..) => 6,
            Expr::F32Ge(..) => 6,

            Expr::F64Eq(..) => 7,
            Expr::F64Ne(..) => 7,
            Expr::F64Lt(..) => 6,
            Expr::F64Gt(..) => 6,
            Expr::F64Le(..) => 6,
            Expr::F64Ge(..) => 6,

            Expr::I32Clz(..) => 0,
            Expr::I32Ctz(..) => 0,
            Expr::I32Popcnt(..) => 0,
            Expr::I32Neg(..) => 1,
            Expr::I32Add(..) => 4,
            Expr::I32Sub(..) => 4,
            Expr::I32Mul(..) => 3,
            Expr::I32DivS(..) => 3,
            Expr::I32DivU(..) => 3,
            Expr::I32RemS(..) => 0,
            Expr::I32RemU(..) => 0,
            Expr::I32And(..) => 8,
            Expr::I32Or(..) => 0,
            Expr::I32Xor(..) => 9,
            Expr::I32Shl(..) => 5,
            Expr::I32ShrS(..) => 5,
            Expr::I32ShrU(..) => 5,
            Expr::I32Rotl(..) => 0,
            Expr::I32Rotr(..) => 0,

            Expr::I64Clz(..) => 0,
            Expr::I64Ctz(..) => 0,
            Expr::I64Popcnt(..) => 0,
            Expr::I64Neg(..) => 1,
            Expr::I64Add(..) => 4,
            Expr::I64Sub(..) => 4,
            Expr::I64Mul(..) => 3,
            Expr::I64DivS(..) => 3,
            Expr::I64DivU(..) => 3,
            Expr::I64RemS(..) => 0,
            Expr::I64RemU(..) => 0,
            Expr::I64And(..) => 8,
            Expr::I64Or(..) => 10,
            Expr::I64Xor(..) => 9,
            Expr::I64Shl(..) => 5,
            Expr::I64ShrS(..) => 5,
            Expr::I64ShrU(..) => 5,
            Expr::I64Rotl(..) => 0,
            Expr::I64Rotr(..) => 0,

            Expr::F32Abs(..) => 0,
            Expr::F32Neg(..) => 1,
            Expr::F32Ceil(..) => 0,
            Expr::F32Floor(..) => 0,
            Expr::F32Trunc(..) => 0,
            Expr::F32Nearest(..) => 0,
            Expr::F32Sqrt(..) => 0,
            Expr::F32Add(..) => 4,
            Expr::F32Sub(..) => 4,
            Expr::F32Mul(..) => 3,
            Expr::F32Div(..) => 3,
            Expr::F32Min(..) => 0,
            Expr::F32Max(..) => 0,
            Expr::F32Copysign(..) => 0,

            Expr::F64Abs(..) => 0,
            Expr::F64Neg(..) => 1,
            Expr::F64Ceil(..) => 0,
            Expr::F64Floor(..) => 0,
            Expr::F64Trunc(..) => 0,
            Expr::F64Nearest(..) => 0,
            Expr::F64Sqrt(..) => 0,
            Expr::F64Add(..) => 4,
            Expr::F64Sub(..) => 4,
            Expr::F64Mul(..) => 3,
            Expr::F64Div(..) => 3,
            Expr::F64Min(..) => 0,
            Expr::F64Max(..) => 0,
            Expr::F64Copysign(..) => 0,

            Expr::I32WrapI64(..) => 0,
            Expr::I32TruncSF32(..) => 0,
            Expr::I32TruncUF32(..) => 0,
            Expr::I32TruncSF64(..) => 0,
            Expr::I32TruncUF64(..) => 0,
            Expr::I64ExtendSI32(..) => 0,
            Expr::I64ExtendUI32(..) => 0,
            Expr::I64TruncSF32(..) => 0,
            Expr::I64TruncUF32(..) => 0,
            Expr::I64TruncSF64(..) => 0,
            Expr::I64TruncUF64(..) => 0,
            Expr::F32ConvertSI32(..) => 0,
            Expr::F32ConvertUI32(..) => 0,
            Expr::F32ConvertSI64(..) => 0,
            Expr::F32ConvertUI64(..) => 0,
            Expr::F32DemoteF64(..) => 0,
            Expr::F64ConvertSI32(..) => 0,
            Expr::F64ConvertUI32(..) => 0,
            Expr::F64ConvertSI64(..) => 0,
            Expr::F64ConvertUI64(..) => 0,
            Expr::F64PromoteF32(..) => 0,

            Expr::I32ReinterpretF32(..) => 0,
            Expr::I64ReinterpretF64(..) => 0,
            Expr::F32ReinterpretI32(..) => 0,
            Expr::F64ReinterpretI64(..) => 0,
        }
    }

    pub fn result_type(&self, module: &bwasm::Module, var_types: &HashMap<Var, ValueType>) -> ValueType {
        use Expr::*;
        use ValueType::*;
        match self {
            True => I32,
            Select(_, expr, _) => expr.result_type(module, var_types),

            Call(idx, ..) => module.func(*idx).return_type().unwrap(),
            CallIndirect(_, _, type_ref) => module.types()[*type_ref as usize].return_type().unwrap(),
            MemorySize => I32,
            MemoryGrow(..) => I32,

            I32Load(..) | F32Load(..) | I32Load8S(..) | I32Load8U(..) | I32Load16S(..) | I32Load16U(..)
            | I32Clz(..) | I32Ctz(..) | I32Popcnt(..) | I32Neg(..) | I32Add(..) | I32Sub(..) | I32Mul(..)
            | I32DivS(..) | I32DivU(..) | I32RemS(..) | I32RemU(..) | I32And(..) | I32Or(..) | I32Xor(..)
            | I32Shl(..) | I32ShrS(..) | I32ShrU(..) | I32Rotl(..) | I32Rotr(..) | I32Const(..) => I32,

            I64Load(..) | F64Load(..) | I64Load8S(..) | I64Load8U(..) | I64Load16S(..) | I64Load16U(..)
            | I64Load32S(..) | I64Load32U(..) | I64Clz(..) | I64Ctz(..) | I64Popcnt(..) | I64Neg(..) | I64Add(..)
            | I64Sub(..) | I64Mul(..) | I64DivS(..) | I64DivU(..) | I64RemS(..) | I64RemU(..) | I64And(..)
            | I64Or(..) | I64Xor(..) | I64Shl(..) | I64ShrS(..) | I64ShrU(..) | I64Rotl(..) | I64Rotr(..)
            | I64Const(..) => I64,

            GetLocal(var) => var_types[var],
            GetGlobal(idx) => module.globals()[*idx as usize].value_type(),

            I32Eqz(..) | I32Eq(..) | I32Ne(..) | I32LtS(..) | I32LtU(..) | I32GtS(..) | I32GtU(..) | I32LeS(..)
            | I32LeU(..) | I32GeS(..) | I32GeU(..) | I64Eqz(..) | I64Eq(..) | I64Ne(..) | I64LtS(..) | I64LtU(..)
            | I64GtS(..) | I64GtU(..) | I64LeS(..) | I64LeU(..) | I64GeS(..) | I64GeU(..) | F32Eq(..) | F32Ne(..)
            | F32Lt(..) | F32Gt(..) | F32Le(..) | F32Ge(..) | F64Eq(..) | F64Ne(..) | F64Lt(..) | F64Gt(..)
            | F64Le(..) | F64Ge(..) => I32,

            F32Const(..) | F32Abs(..) | F32Neg(..) | F32Ceil(..) | F32Floor(..) | F32Trunc(..) | F32Nearest(..)
            | F32Sqrt(..) | F32Add(..) | F32Sub(..) | F32Mul(..) | F32Div(..) | F32Min(..) | F32Max(..)
            | F32Copysign(..) => F32,

            F64Const(..) | F64Abs(..) | F64Neg(..) | F64Ceil(..) | F64Floor(..) | F64Trunc(..) | F64Nearest(..)
            | F64Sqrt(..) | F64Add(..) | F64Sub(..) | F64Mul(..) | F64Div(..) | F64Min(..) | F64Max(..)
            | F64Copysign(..) => F64,

            I32WrapI64(..) | I32TruncSF32(..) | I32TruncUF32(..) | I32TruncSF64(..) | I32TruncUF64(..) => I32,

            I64ExtendSI32(..) | I64ExtendUI32(..) | I64TruncSF32(..) | I64TruncUF32(..) | I64TruncSF64(..)
            | I64TruncUF64(..) => I64,

            F32ConvertSI32(..) | F32ConvertUI32(..) | F32ConvertSI64(..) | F32ConvertUI64(..) | F32DemoteF64(..) => F32,

            F64ConvertSI32(..) | F64ConvertUI32(..) | F64ConvertSI64(..) | F64ConvertUI64(..) | F64PromoteF32(..) => {
                F64
            }

            I32ReinterpretF32(..) => I32,
            I64ReinterpretF64(..) => I64,
            F32ReinterpretI32(..) => F32,
            F64ReinterpretI64(..) => F64,
        }
    }

    pub fn can_invert(&self) -> bool {
        use Expr::*;
        match self {
            I32Eqz(..) | I32Eq(..) | I32Ne(..) | I32LtS(..) | I32LtU(..) | I32GtS(..) | I32GtU(..) | I32LeS(..)
            | I32LeU(..) | I32GeS(..) | I32GeU(..) | I64Eqz(..) | I64Eq(..) | I64Ne(..) | I64LtS(..) | I64LtU(..)
            | I64GtS(..) | I64GtU(..) | I64LeS(..) | I64LeU(..) | I64GeS(..) | I64GeU(..) | F32Eq(..) | F32Ne(..)
            | F32Lt(..) | F32Gt(..) | F32Le(..) | F32Ge(..) | F64Eq(..) | F64Ne(..) | F64Lt(..) | F64Gt(..)
            | F64Le(..) | F64Ge(..) => true,
            _ => false,
        }
    }

    pub fn invert(self) -> Self {
        use Expr::*;
        match self {
            I32Eqz(expr) => I32Ne(expr, Box::new(I32Const(0))),
            I32Eq(a, b) => I32Ne(a, b),
            I32Ne(a, b) => I32Eq(a, b),
            I32LtS(a, b) => I32GeS(a, b),
            I32LtU(a, b) => I32GeU(a, b),
            I32GtS(a, b) => I32LeS(a, b),
            I32GtU(a, b) => I32LeU(a, b),
            I32LeS(a, b) => I32GtS(a, b),
            I32LeU(a, b) => I32GtU(a, b),
            I32GeS(a, b) => I32LtS(a, b),
            I32GeU(a, b) => I32LtU(a, b),
            I64Eqz(expr) => I64Ne(expr, Box::new(I64Const(0))),
            I64Eq(a, b) => I64Ne(a, b),
            I64Ne(a, b) => I64Eq(a, b),
            I64LtS(a, b) => I64GeS(a, b),
            I64LtU(a, b) => I64GeU(a, b),
            I64GtS(a, b) => I64LeS(a, b),
            I64GtU(a, b) => I64LeU(a, b),
            I64LeS(a, b) => I64GtS(a, b),
            I64LeU(a, b) => I64GtU(a, b),
            I64GeS(a, b) => I64LtS(a, b),
            I64GeU(a, b) => I64LtU(a, b),
            F32Eq(a, b) => F32Ne(a, b),
            F32Ne(a, b) => F32Eq(a, b),
            F32Lt(a, b) => F32Ge(a, b),
            F32Gt(a, b) => F32Le(a, b),
            F32Le(a, b) => F32Gt(a, b),
            F32Ge(a, b) => F32Lt(a, b),
            F64Eq(a, b) => F64Ne(a, b),
            F64Ne(a, b) => F64Eq(a, b),
            F64Lt(a, b) => F64Ge(a, b),
            F64Gt(a, b) => F64Le(a, b),
            F64Le(a, b) => F64Gt(a, b),
            F64Ge(a, b) => F64Lt(a, b),
            _ => panic!("Can not invert {:?}", self),
        }
    }
}

fn write_paren(f: &mut fmt::CodeWriter, curr: &Expr, other: &Expr) {
    if curr.precedence() < other.precedence() {
        f.write("(");
        f.write(other);
        f.write(")");
    } else {
        f.write(other);
    }
}

fn write_paren_low(f: &mut fmt::CodeWriter, curr: &Expr, other: &Expr) {
    if curr.precedence() <= other.precedence() {
        f.write("(");
        f.write(other);
        f.write(")");
    } else {
        f.write(other);
    }
}

fn write_unop_func(f: &mut fmt::CodeWriter, name: &'static str, arg: &Expr) {
    f.write(name);
    f.write("(");
    f.write(arg);
    f.write(")");
}

fn write_binop(f: &mut fmt::CodeWriter, symbol: &'static str, expr: &Expr, a: &Expr, b: &Expr) {
    write_paren(f, expr, a);
    f.write(symbol);
    write_paren(f, expr, b);
}

fn write_binop_low(f: &mut fmt::CodeWriter, symbol: &'static str, expr: &Expr, a: &Expr, b: &Expr) {
    write_paren(f, expr, a);
    f.write(symbol);
    write_paren_low(f, expr, b);
}

fn write_binop_func(f: &mut fmt::CodeWriter, name: &'static str, a: &Expr, b: &Expr) {
    f.write(name);
    f.write("(");
    f.write(a);
    f.write(", ");
    f.write(b);
    f.write(")");
}

pub fn write_call(f: &mut fmt::CodeWriter, index: u32, args: &[Expr]) {
    let func_name = f.module().func(index).name().to_string();
    write!(f, "{}(", func_name);
    if !args.is_empty() {
        f.write(&args[0]);
        for arg in &args[1..] {
            f.write(", ");
            f.write(arg);
        }
    }
    f.write(")");
}

impl fmt::CodeDisplay for Expr {
    fn fmt_code(&self, f: &mut fmt::CodeWriter) {
        match self {
            Expr::True => f.write("true"),
            Expr::Select(cond, a, b) => {
                f.write(cond);
                f.write(" ? ");
                write_paren(f, self, a);
                f.write(" : ");
                write_paren(f, self, b);
            }
            Expr::Call(index, args) => write_call(f, *index, args),
            Expr::CallIndirect(index, args, sig) => {
                let mut targets = HashMap::new();
                let wasm = f.wasm();
                let module = wasm.module();
                if !wasm.tables().is_empty() {
                    for (i, ele) in wasm.tables()[0].elements.iter().enumerate() {
                        if let TableElement::Func(index) = ele {
                            if module.func(*index).type_ref() == *sig {
                                targets.insert(i as u32, *index);
                            }
                        }
                    }
                }
                if targets.is_empty() || targets.len() > 10 {
                    f.write("indirect_call(");
                    f.write(index);
                    f.write(")(");
                    if !args.is_empty() {
                        f.write(&args[0]);
                        for arg in &args[1..] {
                            f.write(", ");
                            f.write(arg);
                        }
                    }
                    f.write(")");
                } else {
                    f.write("match ");
                    f.write(index);
                    f.write(" {");
                    f.indent();
                    for (i, target) in targets {
                        f.newline();
                        write!(f, "{} => ", i);
                        write_call(f, target, args);
                        f.write(",");
                    }
                    f.dedent();
                    f.newline();
                    f.write("}");
                }
            }

            Expr::MemorySize => f.write("memory_size()"),
            Expr::MemoryGrow(arg) => write_unop_func(f, "grow_memory", arg),

            Expr::I32Load(target) => write_unop_func(f, "load<i32>", target),
            Expr::I64Load(target) => write_unop_func(f, "load<i64>", target),
            Expr::F32Load(target) => write_unop_func(f, "load<f32>", target),
            Expr::F64Load(target) => write_unop_func(f, "load<f64>", target),
            Expr::I32Load8S(target) => write_unop_func(f, "load_8s<i32>", target),
            Expr::I32Load8U(target) => write_unop_func(f, "load_8u<i32>", target),
            Expr::I32Load16S(target) => write_unop_func(f, "load_16s<i32>", target),
            Expr::I32Load16U(target) => write_unop_func(f, "load_16u<i32>", target),
            Expr::I64Load8S(target) => write_unop_func(f, "load_8s<i64>", target),
            Expr::I64Load8U(target) => write_unop_func(f, "load_8u<i64>", target),
            Expr::I64Load16S(target) => write_unop_func(f, "load_16s<i64>", target),
            Expr::I64Load16U(target) => write_unop_func(f, "load_16u<i64>", target),
            Expr::I64Load32S(target) => write_unop_func(f, "load_32s<i64>", target),
            Expr::I64Load32U(target) => write_unop_func(f, "load_32u<i64>", target),

            Expr::GetLocal(var) => {
                if var.index < f.func().param_count() {
                    write!(f, "arg_{}", var);
                } else {
                    write!(f, "var_{}", var);
                }
            }
            Expr::GetGlobal(index) => write!(f, "global_{}", *index),
            Expr::I32Const(val) => write!(f, "{}", *val as i32),
            Expr::I64Const(val) => write!(f, "{}", *val as i64),
            Expr::F32Const(val) => write!(f, "{}", f32::from_bits(*val)),
            Expr::F64Const(val) => write!(f, "{}", f64::from_bits(*val)),

            Expr::I32Eqz(arg) => {
                write_paren(f, self, arg);
                f.write(" == 0");
            }
            Expr::I32Eq(a, b) => write_binop(f, " == ", self, a, b),
            Expr::I32Ne(a, b) => write_binop(f, " != ", self, a, b),
            Expr::I32LtS(a, b) => write_binop(f, " <s ", self, a, b),
            Expr::I32LtU(a, b) => write_binop(f, " <u ", self, a, b),
            Expr::I32GtS(a, b) => write_binop(f, " >s ", self, a, b),
            Expr::I32GtU(a, b) => write_binop(f, " >u ", self, a, b),
            Expr::I32LeS(a, b) => write_binop(f, " <=s ", self, a, b),
            Expr::I32LeU(a, b) => write_binop(f, " <=u ", self, a, b),
            Expr::I32GeS(a, b) => write_binop(f, " >=s ", self, a, b),
            Expr::I32GeU(a, b) => write_binop(f, " >=u ", self, a, b),

            Expr::I64Eqz(arg) => {
                write_paren(f, self, arg);
                f.write(" == 0");
            }
            Expr::I64Eq(a, b) => write_binop(f, " == ", self, a, b),
            Expr::I64Ne(a, b) => write_binop(f, " != ", self, a, b),
            Expr::I64LtS(a, b) => write_binop(f, " <s ", self, a, b),
            Expr::I64LtU(a, b) => write_binop(f, " <u ", self, a, b),
            Expr::I64GtS(a, b) => write_binop(f, " >s ", self, a, b),
            Expr::I64GtU(a, b) => write_binop(f, " >u ", self, a, b),
            Expr::I64LeS(a, b) => write_binop(f, " <=s ", self, a, b),
            Expr::I64LeU(a, b) => write_binop(f, " <=u ", self, a, b),
            Expr::I64GeS(a, b) => write_binop(f, " >=s ", self, a, b),
            Expr::I64GeU(a, b) => write_binop(f, " >=u ", self, a, b),

            Expr::F32Eq(a, b) => write_binop(f, " == ", self, a, b),
            Expr::F32Ne(a, b) => write_binop(f, " != ", self, a, b),
            Expr::F32Lt(a, b) => write_binop(f, " < ", self, a, b),
            Expr::F32Gt(a, b) => write_binop(f, " > ", self, a, b),
            Expr::F32Le(a, b) => write_binop(f, " <= ", self, a, b),
            Expr::F32Ge(a, b) => write_binop(f, " >= ", self, a, b),

            Expr::F64Eq(a, b) => write_binop(f, " == ", self, a, b),
            Expr::F64Ne(a, b) => write_binop(f, " != ", self, a, b),
            Expr::F64Lt(a, b) => write_binop(f, " < ", self, a, b),
            Expr::F64Gt(a, b) => write_binop(f, " > ", self, a, b),
            Expr::F64Le(a, b) => write_binop(f, " <= ", self, a, b),
            Expr::F64Ge(a, b) => write_binop(f, " >= ", self, a, b),

            Expr::I32Clz(arg) => write_unop_func(f, "clz", arg),
            Expr::I32Ctz(arg) => write_unop_func(f, "ctz", arg),
            Expr::I32Popcnt(arg) => write_unop_func(f, "popcnt", arg),
            Expr::I32Neg(arg) => {
                f.write("-");
                write_paren(f, self, arg);
            }
            Expr::I32Add(a, b) => {
                if let Expr::I32Const(val) = **b {
                    let signed_val = val as i32;
                    if signed_val < 0 {
                        write_binop(f, " - ", self, a, &Expr::I32Const((-signed_val) as u32));
                        return;
                    }
                }
                write_binop(f, " + ", self, a, b);
            }
            Expr::I32Sub(a, b) => write_binop_low(f, " - ", self, a, b),
            Expr::I32Mul(a, b) => write_binop(f, " * ", self, a, b),
            Expr::I32DivS(a, b) => write_binop_low(f, " /s ", self, a, b),
            Expr::I32DivU(a, b) => write_binop_low(f, " /u ", self, a, b),
            Expr::I32RemS(a, b) => write_binop_func(f, "rem_s", a, b),
            Expr::I32RemU(a, b) => write_binop_func(f, "rem_u", a, b),
            Expr::I32And(a, b) => write_binop(f, " & ", self, a, b),
            Expr::I32Or(a, b) => write_binop(f, " | ", self, a, b),
            Expr::I32Xor(a, b) => write_binop(f, " ^ ", self, a, b),
            Expr::I32Shl(a, b) => write_binop(f, " << ", self, a, b),
            Expr::I32ShrS(a, b) => write_binop(f, " >>s ", self, a, b),
            Expr::I32ShrU(a, b) => write_binop(f, " >>u ", self, a, b),
            Expr::I32Rotl(a, b) => write_binop_func(f, "rotl", a, b),
            Expr::I32Rotr(a, b) => write_binop_func(f, "rotr", a, b),

            Expr::I64Clz(arg) => write_unop_func(f, "clz", arg),
            Expr::I64Ctz(arg) => write_unop_func(f, "ctz", arg),
            Expr::I64Popcnt(arg) => write_unop_func(f, "popcnt", arg),
            Expr::I64Neg(arg) => {
                f.write("-");
                write_paren(f, self, arg);
            }
            Expr::I64Add(a, b) => {
                if let Expr::I64Const(val) = **b {
                    let signed_val = val as i64;
                    if signed_val < 0 {
                        write_binop(f, " - ", self, a, &Expr::I64Const((-signed_val) as u64));
                        return;
                    }
                }
                write_binop(f, " + ", self, a, b);
            }
            Expr::I64Sub(a, b) => write_binop_low(f, " - ", self, a, b),
            Expr::I64Mul(a, b) => write_binop(f, " * ", self, a, b),
            Expr::I64DivS(a, b) => write_binop_low(f, " /s ", self, a, b),
            Expr::I64DivU(a, b) => write_binop_low(f, " /u ", self, a, b),
            Expr::I64RemS(a, b) => write_binop_func(f, "rem_s", a, b),
            Expr::I64RemU(a, b) => write_binop_func(f, "rem_u", a, b),
            Expr::I64And(a, b) => write_binop(f, " & ", self, a, b),
            Expr::I64Or(a, b) => write_binop(f, " | ", self, a, b),
            Expr::I64Xor(a, b) => write_binop(f, " ^ ", self, a, b),
            Expr::I64Shl(a, b) => write_binop(f, " << ", self, a, b),
            Expr::I64ShrS(a, b) => write_binop(f, " >>s ", self, a, b),
            Expr::I64ShrU(a, b) => write_binop(f, " >>u ", self, a, b),
            Expr::I64Rotl(a, b) => write_binop_func(f, "rotl", a, b),
            Expr::I64Rotr(a, b) => write_binop_func(f, "rotr", a, b),

            Expr::F32Abs(arg) => write_unop_func(f, "abs", arg),
            Expr::F32Neg(arg) => {
                f.write("-");
                write_paren(f, self, arg);
            }
            Expr::F32Ceil(arg) => write_unop_func(f, "ceil", arg),
            Expr::F32Floor(arg) => write_unop_func(f, "floor", arg),
            Expr::F32Trunc(arg) => write_unop_func(f, "trunc", arg),
            Expr::F32Nearest(arg) => write_unop_func(f, "nearest", arg),
            Expr::F32Sqrt(arg) => write_unop_func(f, "sqrt", arg),
            Expr::F32Add(a, b) => write_binop(f, " + ", self, a, b),
            Expr::F32Sub(a, b) => write_binop_low(f, " - ", self, a, b),
            Expr::F32Mul(a, b) => write_binop(f, " * ", self, a, b),
            Expr::F32Div(a, b) => write_binop_low(f, " / ", self, a, b),
            Expr::F32Min(a, b) => write_binop_func(f, "min", a, b),
            Expr::F32Max(a, b) => write_binop_func(f, "max", a, b),
            Expr::F32Copysign(a, b) => write_binop_func(f, "copysign", a, b),

            Expr::F64Abs(arg) => write_unop_func(f, "abs", arg),
            Expr::F64Neg(arg) => {
                f.write("-");
                write_paren(f, self, arg);
            }
            Expr::F64Ceil(arg) => write_unop_func(f, "ceil", arg),
            Expr::F64Floor(arg) => write_unop_func(f, "floor", arg),
            Expr::F64Trunc(arg) => write_unop_func(f, "trunc", arg),
            Expr::F64Nearest(arg) => write_unop_func(f, "nearest", arg),
            Expr::F64Sqrt(arg) => write_unop_func(f, "sqrt", arg),
            Expr::F64Add(a, b) => write_binop(f, " + ", self, a, b),
            Expr::F64Sub(a, b) => write_binop_low(f, " - ", self, a, b),
            Expr::F64Mul(a, b) => write_binop(f, " * ", self, a, b),
            Expr::F64Div(a, b) => write_binop_low(f, " / ", self, a, b),
            Expr::F64Min(a, b) => write_binop_func(f, "min", a, b),
            Expr::F64Max(a, b) => write_binop_func(f, "max", a, b),
            Expr::F64Copysign(a, b) => write_binop_func(f, "copysign", a, b),

            Expr::I32WrapI64(arg) => write_unop_func(f, "wrap<i32>", arg),
            Expr::I32TruncSF32(arg) => write_unop_func(f, "trunc_s<i32>", arg),
            Expr::I32TruncUF32(arg) => write_unop_func(f, "trunc_u<i32>", arg),
            Expr::I32TruncSF64(arg) => write_unop_func(f, "trunc_s<i32>", arg),
            Expr::I32TruncUF64(arg) => write_unop_func(f, "trunc_u<i32>", arg),
            Expr::I64ExtendSI32(arg) => write_unop_func(f, "extend_s<i64>", arg),
            Expr::I64ExtendUI32(arg) => write_unop_func(f, "extend_u<i64>", arg),
            Expr::I64TruncSF32(arg) => write_unop_func(f, "trunc_s<i64>", arg),
            Expr::I64TruncUF32(arg) => write_unop_func(f, "trunc_u<i64>", arg),
            Expr::I64TruncSF64(arg) => write_unop_func(f, "trunc_s<i64>", arg),
            Expr::I64TruncUF64(arg) => write_unop_func(f, "trunc_u<i64>", arg),
            Expr::F32ConvertSI32(arg) => write_unop_func(f, "convert_s<f32>", arg),
            Expr::F32ConvertUI32(arg) => write_unop_func(f, "convert_u<f32>", arg),
            Expr::F32ConvertSI64(arg) => write_unop_func(f, "convert_s<f32>", arg),
            Expr::F32ConvertUI64(arg) => write_unop_func(f, "convert_u<f32>", arg),
            Expr::F32DemoteF64(arg) => write_unop_func(f, "demote<f32>", arg),
            Expr::F64ConvertSI32(arg) => write_unop_func(f, "convert_s<f64>", arg),
            Expr::F64ConvertUI32(arg) => write_unop_func(f, "convert_u<f64>", arg),
            Expr::F64ConvertSI64(arg) => write_unop_func(f, "convert_s<f64>", arg),
            Expr::F64ConvertUI64(arg) => write_unop_func(f, "convert_u<f64>", arg),
            Expr::F64PromoteF32(arg) => write_unop_func(f, "promote<f64>", arg),

            Expr::I32ReinterpretF32(arg) => write_unop_func(f, "reinterpret<f32>", arg),
            Expr::I64ReinterpretF64(arg) => write_unop_func(f, "reinterpret<f64>", arg),
            Expr::F32ReinterpretI32(arg) => write_unop_func(f, "reinterpret<f32>", arg),
            Expr::F64ReinterpretI64(arg) => write_unop_func(f, "reinterpret<f64>", arg),
        }
    }
}
