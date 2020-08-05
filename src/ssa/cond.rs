use std::collections::{HashMap, HashSet};
use std::ops::Not;

use z3::ast::Ast;

use crate::analysis::used_vars;
use crate::cfg::{EdgeCond, EdgeType};
use crate::fmt;

use super::{Expr, Var};

thread_local! {
    static Z3_CTX: z3::Context = z3::Context::new(&z3::Config::new());
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MappedExpr {
    Expr(Box<Expr>),
    Const(u32),
    Mapped(u32),
}

impl MappedExpr {
    pub fn insert_exprs(&mut self, expr_map: &HashMap<u32, Expr>) {
        if let Self::Mapped(index) = self {
            *self = Self::Expr(Box::new(expr_map[index].clone()));
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            Self::Expr(expr) => expr.precedence(),
            Self::Mapped(_) | Self::Const(_) => 0,
        }
    }

    fn find_vars_internal(&self, result: &mut HashSet<Var>) {
        match self {
            Self::Expr(expr) => used_vars::find_and_add(expr, result),
            Self::Mapped(_) | Self::Const(_) => (),
        }
    }

    fn to_z3_int_expr<'ctx>(&self, ctx: &'ctx z3::Context) -> Option<z3::ast::BV<'ctx>> {
        match self {
            Self::Expr(_) => None,
            Self::Mapped(idx) => Some(z3::ast::BV::new_const(ctx, *idx, 32)),
            Self::Const(val) => Some(z3::ast::BV::from_u64(ctx, (*val).into(), 32)),
        }
    }

    fn to_z3_bool_expr<'ctx>(&self, ctx: &'ctx z3::Context) -> Option<z3::ast::Bool<'ctx>> {
        match self {
            Self::Expr(_) => None,
            Self::Mapped(idx) => Some(z3::ast::Bool::new_const(ctx, *idx)),
            Self::Const(0) => Some(z3::ast::Bool::from_bool(ctx, false)),
            Self::Const(_) => Some(z3::ast::Bool::from_bool(ctx, true)),
        }
    }
}

impl<'ctx> From<z3::ast::Dynamic<'ctx>> for MappedExpr {
    fn from(z3_expr: z3::ast::Dynamic<'ctx>) -> Self {
        assert!(z3_expr.is_app(), "z3_expr is not an application");

        use z3_sys::DeclKind::*;
        match z3_expr.decl().kind() {
            BNUM => {
                let bv_expr = z3_expr.as_bv().unwrap();
                MappedExpr::Const(bv_expr.as_u64().unwrap() as u32)
            }
            UNINTERPRETED if z3_expr.num_args() == 0 => {
                if let z3::Symbol::Int(idx) = z3_expr.decl().symbol() {
                    MappedExpr::Mapped(idx as u32)
                } else {
                    panic!("z3 var has a string name")
                }
            }
            other => panic!("unknown z3_expr decl kind: {:?}", other),
        }
    }
}

impl fmt::CodeDisplay for MappedExpr {
    fn fmt_code(&self, f: &mut fmt::CodeWriter) {
        match self {
            Self::Expr(expr) => f.write(expr),
            Self::Mapped(index) => write!(f, "expr_{}", index),
            Self::Const(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Neq,
    Geq,
    Gt,
    Leq,
    Lt,
}

impl CmpOp {
    pub fn invert(self) -> Self {
        match self {
            Self::Eq => Self::Neq,
            Self::Neq => Self::Eq,
            Self::Geq => Self::Lt,
            Self::Gt => Self::Leq,
            Self::Leq => Self::Gt,
            Self::Lt => Self::Geq,
        }
    }

    pub fn mirror(self) -> Self {
        match self {
            Self::Eq => Self::Eq,
            Self::Neq => Self::Neq,
            Self::Geq => Self::Leq,
            Self::Gt => Self::Lt,
            Self::Leq => Self::Geq,
            Self::Lt => Self::Gt,
        }
    }
}

impl std::fmt::Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Geq => write!(f, ">=u"),
            Self::Gt => write!(f, ">u"),
            Self::Leq => write!(f, "<=u"),
            Self::Lt => write!(f, "<u"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cond {
    True,
    False,
    Not(Box<Cond>),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),

    Cmp(MappedExpr, CmpOp, MappedExpr),
    Expr(MappedExpr),
}

impl From<EdgeCond> for Cond {
    fn from(cond: EdgeCond) -> Self {
        match cond.edge_type {
            EdgeType::Unconditional => Self::True,
            EdgeType::Conditional(true) => Self::from_index(cond.expr_index),
            EdgeType::Conditional(false) => Self::from_index(cond.expr_index).not(),
            EdgeType::CaseRange(start, end) => {
                let idx = MappedExpr::Mapped(cond.expr_index);
                if start + 1 == end {
                    Self::Cmp(MappedExpr::Mapped(cond.expr_index), CmpOp::Eq, MappedExpr::Const(start))
                } else {
                    let start = MappedExpr::Const(start);
                    let end = MappedExpr::Const(end);
                    Self::Cmp(start, CmpOp::Leq, idx.clone()).and(Self::Cmp(idx, CmpOp::Lt, end))
                }
            }
            EdgeType::Default(min_val) => Self::Cmp(
                MappedExpr::Mapped(cond.expr_index),
                CmpOp::Geq,
                MappedExpr::Const(min_val),
            ),
        }
    }
}

impl Cond {
    pub fn from_index(index: u32) -> Self {
        Self::Expr(MappedExpr::Mapped(index))
    }

    pub fn and(self, b: Self) -> Self {
        match (self.is_const_true(), b.is_const_true()) {
            (true, true) => Self::True,
            (true, false) => b,
            (false, true) => self,
            (false, false) => {
                if self.is_const_false() || b.is_const_false() {
                    Self::False
                } else {
                    Self::And(Box::new(self), Box::new(b))
                }
            }
        }
    }

    pub fn and_inplace(&mut self, b: Self) {
        let curr_self = std::mem::replace(self, Cond::True);
        *self = curr_self.and(b);
    }

    pub fn or(self, b: Self) -> Self {
        match (self.is_const_false(), b.is_const_false()) {
            (true, true) => Self::False,
            (true, false) => b,
            (false, true) => self,
            (false, false) => {
                if self.is_const_true() || b.is_const_true() {
                    Self::True
                } else {
                    Self::Or(Box::new(self), Box::new(b))
                }
            }
        }
    }

    pub fn is_const_true(&self) -> bool {
        match self {
            Self::True => true,
            Self::And(a, b) => a.is_const_true() && b.is_const_true(),
            Self::Or(a, b) => a.is_const_true() || b.is_const_true(),
            _ => false,
        }
    }

    pub fn is_const_false(&self) -> bool {
        match self {
            Self::False => true,
            Self::And(a, b) => a.is_const_false() || b.is_const_false(),
            Self::Or(a, b) => a.is_const_false() && b.is_const_false(),
            _ => false,
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            Self::True => 0,
            Self::False => 0,
            Self::Not(..) => 2,
            Self::And(..) => 11,
            Self::Or(..) => 12,
            Self::Cmp(_, CmpOp::Eq, _) => 7,
            Self::Cmp(..) => 6,
            Self::Expr(expr) => expr.precedence(),
        }
    }

    pub fn insert_exprs(&mut self, expr_map: &HashMap<u32, Expr>) {
        match self {
            Self::True | Self::False => (),
            Self::Not(cond) => cond.insert_exprs(expr_map),
            Self::And(a, b) => {
                a.insert_exprs(expr_map);
                b.insert_exprs(expr_map);
            }
            Self::Or(a, b) => {
                a.insert_exprs(expr_map);
                b.insert_exprs(expr_map);
            }
            Self::Cmp(a, _, b) => {
                a.insert_exprs(expr_map);
                b.insert_exprs(expr_map);
            }
            Self::Expr(expr) => expr.insert_exprs(expr_map),
        }
    }

    pub fn find_vars(&self) -> HashSet<Var> {
        let mut result = HashSet::new();
        self.find_vars_internal(&mut result);
        result
    }

    fn find_vars_internal(&self, result: &mut HashSet<Var>) {
        match self {
            Self::True | Self::False => (),
            Self::Not(cond) => cond.find_vars_internal(result),
            Self::And(a, b) => {
                a.find_vars_internal(result);
                b.find_vars_internal(result);
            }
            Self::Or(a, b) => {
                a.find_vars_internal(result);
                b.find_vars_internal(result);
            }
            Self::Cmp(a, _, b) => {
                a.find_vars_internal(result);
                b.find_vars_internal(result);
            }
            Self::Expr(expr) => expr.find_vars_internal(result),
        }
    }

    pub fn get_mapped_vars(&self) -> HashSet<u32> {
        let mut result = HashSet::new();
        self.get_mapped_vars_internal(&mut result);
        result
    }

    fn get_mapped_vars_internal(&self, result: &mut HashSet<u32>) {
        match self {
            Self::True | Self::False => (),
            Self::Not(cond) => cond.get_mapped_vars_internal(result),
            Self::And(a, b) => {
                a.get_mapped_vars_internal(result);
                b.get_mapped_vars_internal(result);
            }
            Self::Or(a, b) => {
                a.get_mapped_vars_internal(result);
                b.get_mapped_vars_internal(result);
            }
            Self::Cmp(a, _, b) => {
                if let MappedExpr::Mapped(a) = a {
                    result.insert(*a);
                }
                if let MappedExpr::Mapped(b) = b {
                    result.insert(*b);
                }
            }
            Self::Expr(expr) => {
                if let MappedExpr::Mapped(expr) = expr {
                    result.insert(*expr);
                }
            }
        }
    }

    pub fn get_subexprs(&self) -> Vec<&Cond> {
        let mut result = Vec::new();
        self.get_subexprs_internal(&mut result);
        result
    }

    fn get_subexprs_internal<'a>(&'a self, result: &mut Vec<&'a Cond>) {
        // TODO: Fix this. Convert to KNF or otherwise properly compute common terms in Cond::Or
        match self {
            Cond::True | Cond::False | Cond::Not(_) | Cond::Cmp(_, _, _) | Cond::Expr(_) | Cond::Or(..) => result.push(self),
            Cond::And(left, right) => {
                left.get_subexprs_internal(result);
                right.get_subexprs_internal(result);
            }
            // Cond::Or(left, right) => {
            //     let left = left.get_subexprs().into_iter().collect::<HashSet<_>>();
            //     let right = right.get_subexprs().into_iter().collect::<HashSet<_>>();
            //     for expr in left.intersection(&right) {
            //         result.push(expr);
            //     }
            // }
        }
    }

    pub fn simplify(&mut self) {
        Z3_CTX.with(|ctx| {
            if let Some(z3_expr) = self.to_z3_expr(ctx) {
                *self = simplify_z3_expr(z3_expr).into();
            };
        });
    }

    /// - Some(true) => self == !other
    /// - Some(false) => self == other
    /// - None => self is not compareable to other
    pub fn is_opposite(&self, other: &Self) -> Option<bool> {
        Z3_CTX.with(|ctx| {
            let a = self.to_z3_expr(ctx)?;
            let b = other.to_z3_expr(ctx)?;
            let cond = simplify_z3_expr(a._eq(&b.not()));
            cond.as_bool()
        })
    }

    fn to_z3_expr<'ctx>(&self, ctx: &'ctx z3::Context) -> Option<z3::ast::Bool<'ctx>> {
        Some(match self {
            Self::True => z3::ast::Bool::from_bool(ctx, true),
            Self::False => z3::ast::Bool::from_bool(ctx, false),
            Self::Not(cond) => cond.to_z3_expr(ctx)?.not(),
            Self::And(a, b) => a.to_z3_expr(ctx)?.and(&[&b.to_z3_expr(ctx)?]),
            Self::Or(a, b) => a.to_z3_expr(ctx)?.or(&[&b.to_z3_expr(ctx)?]),
            Self::Cmp(a, cmp, b) => {
                let a = a.to_z3_int_expr(ctx)?;
                let b = &b.to_z3_int_expr(ctx)?;
                match cmp {
                    CmpOp::Eq => a._eq(b),
                    CmpOp::Neq => a.distinct(&[b]),
                    CmpOp::Geq => a.bvuge(b),
                    CmpOp::Gt => a.bvugt(b),
                    CmpOp::Leq => a.bvule(b),
                    CmpOp::Lt => a.bvult(b),
                }
            }
            Self::Expr(expr) => expr.to_z3_bool_expr(ctx)?,
        })
    }
}

impl Not for Cond {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Not(expr) => *expr,
            Self::And(a, b) => Self::Or(Box::new(a.not()), Box::new(b.not())),
            Self::Or(a, b) => Self::And(Box::new(a.not()), Box::new(b.not())),
            Self::Cmp(a, cmp, b) => Self::Cmp(a, cmp.invert(), b),
            Self::Expr(MappedExpr::Const(0)) => Self::True,
            Self::Expr(MappedExpr::Const(_)) => Self::False,
            Self::Expr(MappedExpr::Expr(expr)) => {
                if expr.can_invert() {
                    Self::Expr(MappedExpr::Expr(Box::new(expr.invert())))
                } else {
                    Self::Not(Box::new(Self::Expr(MappedExpr::Expr(expr))))
                }
            }
            _ => Self::Not(Box::new(self)),
        }
    }
}

fn simplify_z3_expr(expr: z3::ast::Bool) -> z3::ast::Bool {
    let ctx = expr.get_ctx();
    let tactic_simplify = z3::Tactic::from_name(ctx, "simplify").unwrap();
    let tactic_propagate_values = z3::Tactic::from_name(ctx, "propagate-values").unwrap();
    let tactic_ctx_solver_simplify = z3::Tactic::from_name(ctx, "ctx-solver-simplify").unwrap();
    let expr = tactic_simplify.apply(&(&expr).into()).as_expr();
    let expr = tactic_propagate_values.apply(&(&expr).into()).as_expr();
    let expr = tactic_ctx_solver_simplify.apply(&(&expr).into()).as_expr();
    tactic_simplify.apply(&(&expr).into()).as_expr()
}

impl<'ctx, AST: z3::ast::Ast<'ctx>> From<AST> for Cond {
    fn from(z3_expr: AST) -> Self {
        assert!(z3_expr.is_app(), "z3_expr is not an application");

        use z3_sys::DeclKind::*;
        match z3_expr.decl().kind() {
            TRUE => Self::True,
            FALSE => Self::False,
            NOT => Self::not(z3_expr.arg(0).into()),
            AND => {
                let mut cond = z3_expr.arg(0).into();
                for i in 1..z3_expr.num_args() {
                    cond = Self::and(cond, z3_expr.arg(i).into());
                }
                cond
            }
            OR => {
                let mut cond = z3_expr.arg(0).into();
                for i in 1..z3_expr.num_args() {
                    cond = Self::or(cond, z3_expr.arg(i).into());
                }
                cond
            }
            EQ => Self::Cmp(z3_expr.arg(0).into(), CmpOp::Eq, z3_expr.arg(1).into()),
            UGEQ => Self::Cmp(z3_expr.arg(0).into(), CmpOp::Geq, z3_expr.arg(1).into()),
            UGT => Self::Cmp(z3_expr.arg(0).into(), CmpOp::Gt, z3_expr.arg(1).into()),
            ULEQ => Self::Cmp(z3_expr.arg(0).into(), CmpOp::Leq, z3_expr.arg(1).into()),
            ULT => Self::Cmp(z3_expr.arg(0).into(), CmpOp::Lt, z3_expr.arg(1).into()),
            UNINTERPRETED if z3_expr.num_args() == 0 => {
                if let z3::Symbol::Int(idx) = z3_expr.decl().symbol() {
                    Self::Expr(MappedExpr::Mapped(idx as u32))
                } else {
                    panic!("z3 var has a string name")
                }
            }
            other => panic!("unknown z3_expr decl kind: {:?}", other),
        }
    }
}

fn write_paren(f: &mut fmt::CodeWriter, curr: &Cond, other: &Cond) {
    if curr.precedence() < other.precedence() {
        f.write("(");
        f.write(other);
        f.write(")");
    } else {
        f.write(other);
    }
}

impl fmt::CodeDisplay for Cond {
    fn fmt_code(&self, f: &mut fmt::CodeWriter) {
        match self {
            Self::True => f.write("true"),
            Self::False => f.write("false"),
            Self::Not(expr) => match &**expr {
                Self::Expr(MappedExpr::Expr(expr)) if expr.can_invert() => f.write(expr.clone().invert()),
                _ => {
                    f.write("!");
                    write_paren(f, self, expr);
                }
            },
            Self::And(a, b) => {
                write_paren(f, self, a);
                f.write(" && ");
                write_paren(f, self, b);
            }
            Self::Or(a, b) => {
                write_paren(f, self, a);
                f.write(" || ");
                write_paren(f, self, b);
            }
            Self::Cmp(a, cmp, b) => {
                if self.precedence() < a.precedence() {
                    f.write("(");
                    f.write(a);
                    f.write(")");
                } else {
                    f.write(a);
                }
                write!(f, " {} ", cmp);
                if self.precedence() < b.precedence() {
                    f.write("(");
                    f.write(b);
                    f.write(")");
                } else {
                    f.write(b);
                }
            }
            Self::Expr(expr) => f.write(expr),
        }
    }
}
