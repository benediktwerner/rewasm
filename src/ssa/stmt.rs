use crate::fmt;

use super::{Cond, Expr, Var, cond::MappedExpr, ValueSpace};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    ForLoop(Var, Option<Expr>, Cond, Expr, Vec<Stmt>),
    Break,
    Seq(Vec<Stmt>),
    Nop,

    Branch(Expr),
    Phi(Var, Vec<u32>),

    If(Cond, Vec<Stmt>),
    IfElse(Cond, Vec<Stmt>, Vec<Stmt>),
    SwitchCase(MappedExpr, Vec<(ValueSpace, Stmt)>, Option<Box<Stmt>>),

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

    pub fn complexity(&self) -> u32 {
        use Stmt::*;
        match self {
            Expr(expr) => expr.complexity(),
            Return(expr) => 1 + expr.complexity(),
            ReturnVoid => 1,
            Branch(expr) => 1 + expr.complexity(),
            Phi(..) => 0,
            SetLocal(_, expr) | SetGlobal(_, expr) => 1 + expr.complexity(),
            I32Store(location, value)
            | I64Store(location, value)
            | F32Store(location, value)
            | F64Store(location, value)
            | I32Store8(location, value)
            | I32Store16(location, value)
            | I64Store8(location, value)
            | I64Store16(location, value)
            | I64Store32(location, value) => 1 + location.complexity() + value.complexity(),
            Unreachable => 1,
            While(..) => unreachable!(),
            ForLoop(..) => unreachable!(),
            Break => unreachable!(),
            If(..) => unreachable!(),
            IfElse(..) => unreachable!(),
            SwitchCase(..) => unreachable!(),
            Seq(..) => unreachable!(),
            Nop => unreachable!(),
        }
    }
}

impl Default for Stmt {
    fn default() -> Self {
        Stmt::Nop
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
    if var.index < f.func().param_count() {
        write!(f, "arg_{}", var);
    } else {
        write!(f, "var_{}", var);
    }

    match expr {
        Expr::I32Add(v, b) | Expr::I64Add(v, b) | Expr::F32Add(v, b) | Expr::F64Add(v, b) => {
            if let Expr::GetLocal(v) = **v {
                if v == var {
                    if let Expr::I32Const(val) = **b {
                        let val = val as i32;
                        if val == 1 {
                            f.write("++");
                        } else if val == -1 {
                            f.write("--");
                        } else if val < 0 {
                            f.write(" -= ");
                            write!(f, "{}", -val);
                        } else {
                            f.write(" += ");
                            write!(f, "{}", val);
                        }
                    } else if let Expr::I64Const(val) = **b {
                        let val = val as i64;
                        if val == 1 {
                            f.write("++");
                        } else if val == -1 {
                            f.write("--");
                        } else if val < 0 {
                            f.write(" -= ");
                            write!(f, "{}", -val);
                        } else {
                            f.write(" += ");
                            write!(f, "{}", val);
                        }
                    } else {
                        f.write(" += ");
                        f.write(b);
                    }
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
                    if let Expr::I32Const(val) = **b {
                        if (val as i32) < 0 {
                            f.write(" -= ");
                            write!(f, "{}", -(val as i32));
                            return;
                        }
                    } else if let Expr::I64Const(val) = **b {
                        if (val as i64) < 0 {
                            f.write(" -= ");
                            write!(f, "{}", -(val as i64));
                            return;
                        }
                    }
                    f.write(" += ");
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
            Stmt::ForLoop(var, init, cond, post, body) => {
                f.write("for ");
                if let Some(init) = init {
                    write_assign_local(f, *var, init);
                }
                f.write("; ");
                f.write(cond);
                f.write("; ");
                write_assign_local(f, *var, post);
                f.write(" {");
                f.indent();
                f.write(&body[..]);
                f.dedent();
                f.newline();
                f.write("}");
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
            Stmt::SwitchCase(expr, cases, default) => {
                f.write("match ");
                f.write(expr);
                f.write(" {");
                f.indent();
                for (values, stmt) in cases {
                    f.newline();
                    f.write(values);
                    f.write(" => {");
                    f.indent();
                    f.write(stmt);
                    f.dedent();
                    f.newline();
                    f.write("}");
                }
                if let Some(default_stmt) = default {
                    f.newline();
                    f.write("default => {");
                    f.indent();
                    f.write(default_stmt);
                    f.dedent();
                    f.newline();
                    f.write("}");
                }
                f.dedent();
                f.newline();
                f.write("}");
            }
            Stmt::SetLocal(var, expr) => {
                write_assign_local(f, *var, expr);
                f.write(";");
            }
            Stmt::SetGlobal(index, expr) => {
                write!(f, "global_{}", index);
                write_assign_global(f, *index, expr);
                f.write(";");
            }
            Stmt::I32Store(target, expr) => write_store(f, "store<i32>", target, expr),
            Stmt::I64Store(target, expr) => write_store(f, "store<i64>", target, expr),
            Stmt::F32Store(target, expr) => write_store(f, "store<f32>", target, expr),
            Stmt::F64Store(target, expr) => write_store(f, "store<f64>", target, expr),
            Stmt::I32Store8(target, expr) => write_store(f, "store_8<i32>", target, expr),
            Stmt::I32Store16(target, expr) => write_store(f, "store_16<i32>", target, expr),
            Stmt::I64Store8(target, expr) => write_store(f, "store_8<i64>", target, expr),
            Stmt::I64Store16(target, expr) => write_store(f, "store_16<i64>", target, expr),
            Stmt::I64Store32(target, expr) => write_store(f, "store_32<i32>", target, expr),
        }
    }
}
