use crate::fmt;

use super::{Cond, Expr, Var};

#[derive(Debug, Clone, Copy)]
pub enum LoopKind {
    While,
    DoWhile,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Unreachable,
    Expr(Expr),
    Return(Expr),
    ReturnVoid,
    While(Cond, Vec<Stmt>, LoopKind),
    Break,
    Seq(Vec<Stmt>),
    Nop,

    Branch(Expr),
    Phi(Var, Vec<u32>),

    If(Cond, Vec<Stmt>),
    IfElse(Cond, Vec<Stmt>, Vec<Stmt>),

    SetLocal(Var, Expr),
    SetGlobal(u32, Expr),

    // target, offset, value
    I32Store(Expr, Expr),
    I64Store(Expr, Expr),
    F32Store(Expr, Expr),
    F64Store(Expr, Expr),
    I32Store8(Expr, Expr),
    I32Store16(Expr, Expr),
    I64Store8(Expr, Expr),
    I64Store16(Expr, Expr),
    I64Store32(Expr, Expr),
}

impl Stmt {
    pub fn is_jump(&self) -> bool {
        match self {
            Stmt::Break => true,
            Stmt::Return(..) => true,
            Stmt::ReturnVoid => true,
            Stmt::Unreachable => true,
            _ => false,
        }
    }

    pub fn phi(index: u32) -> Stmt {
        Stmt::Phi(Var::no_sub(index), Vec::new())
    }

    pub fn length(&self) -> usize {
        use Stmt::*;
        match self {
            Expr(expr) => expr.length(),
            Return(expr) => 5 + expr.length(),
            ReturnVoid => 5,
            Branch(expr) => 5 + expr.length(),
            Phi(..) => 0,
            SetLocal(_, expr) | SetGlobal(_, expr) => 5 + expr.length(),
            I32Store(location, value)
            | I64Store(location, value)
            | F32Store(location, value)
            | F64Store(location, value)
            | I32Store8(location, value)
            | I32Store16(location, value)
            | I64Store8(location, value)
            | I64Store16(location, value)
            | I64Store32(location, value) => location.length() + 6 + value.length(),
            Unreachable => 10,
            While(..) => unreachable!(),
            Break => unreachable!(),
            If(..) => unreachable!(),
            IfElse(..) => unreachable!(),
            Seq(..) => unreachable!(),
            Nop => unreachable!(),
        }
    }
}

fn write_store(f: &mut fmt::CodeWriter, fn_name: &'static str, target: &Expr, expr: &Expr) {
    f.write(fn_name);
    f.write("(");
    f.write(target);
    f.write(", ");
    f.write(expr);
    f.write(")");
}

fn write_assign_local(f: &mut fmt::CodeWriter, var: Var, expr: &Expr) {
    match expr {
        Expr::I32Add(v, b) | Expr::I64Add(v, b) | Expr::F32Add(v, b) | Expr::F64Add(v, b) => {
            if let Expr::GetLocal(v) = **v {
                if v == var {
                    write!(f, " += ");
                    f.write(b);
                    return;
                }
            }
        }
        Expr::I32Mul(v, b) | Expr::I64Mul(v, b) | Expr::F32Mul(v, b) | Expr::F64Mul(v, b) => {
            if let Expr::GetLocal(v) = **v {
                if v == var {
                    write!(f, " *= ");
                    f.write(b);
                    return;
                }
            }
        }
        Expr::I32Sub(v, b) | Expr::I64Sub(v, b) | Expr::F32Sub(v, b) | Expr::F64Sub(v, b) => {
            if let Expr::GetLocal(v) = **v {
                if v == var {
                    write!(f, " -= ");
                    f.write(b);
                    return;
                }
            }
        }
        _ => (),
    }
    write!(f, " = ");
    f.write(expr);
}

fn write_assign_global(f: &mut fmt::CodeWriter, var: u32, expr: &Expr) {
    match expr {
        Expr::I32Add(v, b) | Expr::I64Add(v, b) | Expr::F32Add(v, b) | Expr::F64Add(v, b) => {
            if let Expr::GetGlobal(v) = **v {
                if v == var {
                    write!(f, " += ");
                    f.write(b);
                    return;
                }
            }
        }
        Expr::I32Mul(v, b) | Expr::I64Mul(v, b) | Expr::F32Mul(v, b) | Expr::F64Mul(v, b) => {
            if let Expr::GetGlobal(v) = **v {
                if v == var {
                    write!(f, " *= ");
                    f.write(b);
                    return;
                }
            }
        }
        Expr::I32Sub(v, b) | Expr::I64Sub(v, b) | Expr::F32Sub(v, b) | Expr::F64Sub(v, b) => {
            if let Expr::GetGlobal(v) = **v {
                if v == var {
                    write!(f, " -= ");
                    f.write(b);
                    return;
                }
            }
        }
        _ => (),
    }
    write!(f, " = ");
    f.write(expr);
}

impl fmt::CodeDisplay for Stmt {
    fn fmt_code(&self, f: &mut fmt::CodeWriter) {
        match self {
            Stmt::Nop => return,
            Stmt::Seq(code) => {
                f.write(&code[..]);
                return;
            }
            _ => (),
        }

        f.newline();
        match self {
            Stmt::Unreachable => write!(f, "unreachable!();"),
            Stmt::Expr(expr) => {
                f.write(expr);
                f.write(";");
            }
            Stmt::Return(result) => {
                f.write("return ");
                f.write(result);
                f.write(";");
            }
            Stmt::ReturnVoid => {
                f.write("return;");
            }
            Stmt::While(cond, body, kind) => {
                match kind {
                    LoopKind::While => {
                        f.write("while ");
                        f.write(cond);
                    }
                    LoopKind::DoWhile => f.write("do"),
                }
                f.write(" {");
                f.indent();
                f.write(&body[..]);
                f.dedent();
                f.newline();
                f.write("}");
                if let LoopKind::DoWhile = kind {
                    f.write(" while ");
                    f.write(cond);
                    f.write(";");
                }
            }
            Stmt::Break => {
                f.write("break;");
            }
            Stmt::Seq(..) => unreachable!(),
            Stmt::Nop => unreachable!(),
            Stmt::Branch(cond) => f.write(cond),
            Stmt::Phi(var, args) => {
                let arg_name = if var.index < f.func().param_count() {
                    "arg"
                } else {
                    "var"
                };
                write!(f, "{}{} = phi(", arg_name, var);
                if !args.is_empty() {
                    write!(f, "{}{}_{}", arg_name, var.index, &args[0]);
                    for arg in &args[1..] {
                        f.write(", ");
                        write!(f, "{}{}_{}", arg_name, var.index, arg);
                    }
                }
                f.write(");");
            }
            Stmt::If(cond, then_branch) => {
                f.write("if ");
                f.write(cond);
                f.write(" {");
                f.indent();
                f.write(&then_branch[..]);
                f.dedent();
                f.newline();
                f.write("}");
            }
            Stmt::IfElse(cond, if_branch, else_branch) => {
                f.write("if ");
                f.write(cond);
                f.write(" {");
                f.indent();
                f.write(&if_branch[..]);
                f.dedent();
                f.newline();
                f.write("}");
                f.newline();
                if else_branch.len() == 1 {
                    match &else_branch[0] {
                        stmt @ Stmt::If(..) | stmt @ Stmt::IfElse(..) => {
                            f.write("else ");
                            f.suppress_newline();
                            f.write(stmt);
                            return;
                        }
                        _ => (),
                    }
                }
                f.write("else {");
                f.indent();
                f.write(&else_branch[..]);
                f.dedent();
                f.newline();
                f.write("}");
            }
            Stmt::SetLocal(var, expr) => {
                if var.index < f.func().param_count() {
                    write!(f, "arg_{}", var);
                } else {
                    write!(f, "var_{}", var);
                }
                write_assign_local(f, *var, expr);
                f.write(";");
            }
            Stmt::SetGlobal(index, expr) => {
                write!(f, "global_{}", index);
                write_assign_global(f, *index, expr);
                f.write(";");
            }
            Stmt::I32Store(target, expr) => write_store(f, "store_32", target, expr),
            Stmt::I64Store(target, expr) => write_store(f, "store_64", target, expr),
            Stmt::F32Store(target, expr) => write_store(f, "store_32", target, expr),
            Stmt::F64Store(target, expr) => write_store(f, "store_64", target, expr),
            Stmt::I32Store8(target, expr) => write_store(f, "store_8", target, expr),
            Stmt::I32Store16(target, expr) => write_store(f, "store_16", target, expr),
            Stmt::I64Store8(target, expr) => write_store(f, "store_8", target, expr),
            Stmt::I64Store16(target, expr) => write_store(f, "store_16", target, expr),
            Stmt::I64Store32(target, expr) => write_store(f, "store_32", target, expr),
        }
    }
}
