use std::collections::HashSet;

use crate::ssa::{cond::MappedExpr, Cond, Stmt, ValueSpace};

pub fn apply(code: &mut Vec<Stmt>) {
    if code.is_empty() {
        return;
    }

    let mut i = 0;
    while i < code.len() - 1 {
        if let Stmt::If(cond, _) = &code[i] {
            for expr in cond.get_subexprs() {
                let end = find_nodes_with_common_subexpr(code, i, expr);
                if end > i + 1 {
                    let expr = expr.clone();
                    combine_nodes_with_common_subexpr(code, expr, i, end);
                    break;
                }
            }
        }

        i += 1;
    }

    let mut i = 0;
    while i < code.len() - 1 {
        if let Stmt::If(cond, _) = &code[i] {
            match &code[i + 1] {
                Stmt::If(other, _) | Stmt::IfElse(other, ..) => {
                    if cond.is_opposite(other).is_some() {
                        let cond = if let Cond::Not(_) = cond {
                            other.clone()
                        } else {
                            cond.clone()
                        };
                        combine_conditions(code, i, cond);
                    }
                }
                _ => (),
            }
        }
        i += 1;
    }

    i = 0;
    while i < code.len() - 1 {
        if let Stmt::If(cond, _) = &code[i] {
            if let Some(expr) = is_switch_case_cond(cond, None) {
                create_switch_case(code, i, expr);
            }
        }
        i += 1;
    }
}

fn find_nodes_with_common_subexpr<'a>(code: &'a [Stmt], start: usize, expr: &Cond) -> usize {
    for (i, stmt) in code[start + 1..].iter().enumerate() {
        if let Stmt::If(cond, _) = stmt {
            if cond.get_subexprs().iter().any(|e| *e == expr) {
                continue;
            }
        }
        return start + 1 + i;
    }
    code.len()
}

fn combine_nodes_with_common_subexpr(code: &mut Vec<Stmt>, expr: Cond, start: usize, end: usize) {
    let mut new_code = Vec::new();

    for stmt in code.drain(start..end) {
        if let Stmt::If(cond, body) = stmt {
            let cond = cond
                .get_subexprs()
                .into_iter()
                .cloned()
                .filter(|e| *e != expr)
                .fold(Cond::True, |a, b| a.and(b));

            if cond.is_const_true() {
                new_code.push(Stmt::Seq(body));
            } else {
                new_code.push(Stmt::If(cond, body));
            }
        } else {
            unreachable!();
        }
    }

    apply(&mut new_code);
    code.insert(start, Stmt::If(expr, new_code));
}

fn combine_conditions(code: &mut Vec<Stmt>, start: usize, cond: Cond) {
    let mut true_branch = HashSet::new();
    let mut false_branch = HashSet::new();

    for (i, stmt) in code.iter().enumerate().skip(start) {
        match stmt {
            Stmt::If(other_cond, _) | Stmt::IfElse(other_cond, ..) => match cond.is_opposite(other_cond) {
                Some(true) => {
                    false_branch.insert(i);
                }
                Some(false) => {
                    true_branch.insert(i);
                }
                None => break,
            },
            _ => break,
        }
    }

    let mut true_code = Vec::new();
    let mut false_code = Vec::new();

    for (i, stmt) in code
        .drain(start..(start + true_branch.len() + false_branch.len()))
        .enumerate()
    {
        if true_branch.contains(&(start + i)) {
            match stmt {
                Stmt::If(_, body) => true_code.extend(body),
                Stmt::IfElse(_, if_body, else_body) => {
                    true_code.extend(if_body);
                    false_code.extend(else_body);
                }
                _ => unreachable!(),
            }
        } else {
            match stmt {
                Stmt::If(_, body) => false_code.extend(body),
                Stmt::IfElse(_, if_body, else_body) => {
                    false_code.extend(if_body);
                    true_code.extend(else_body);
                }
                _ => unreachable!(),
            }
        }
    }

    if false_code.is_empty() {
        code.insert(start, Stmt::If(cond, true_code));
    } else if true_code.last().unwrap().is_jump() {
        code.insert(start, Stmt::If(cond, true_code));
        code.insert(start + 1, Stmt::Seq(false_code));
    } else if false_code.last().unwrap().is_jump() {
        code.insert(start, Stmt::If(cond, false_code));
        code.insert(start + 1, Stmt::Seq(true_code));
    } else {
        code.insert(start, Stmt::IfElse(cond, true_code, false_code));
    }
}

fn create_switch_case(code: &mut Vec<Stmt>, start: usize, expr: u32) {
    let mut cases = Vec::new();
    let mut total_values = ValueSpace::empty();

    for (i, stmt) in code.iter().enumerate().skip(start) {
        match stmt {
            Stmt::If(cond, _) => {
                if let Some(values) = find_case_values(cond, expr) {
                    if values.is_empty() || !total_values.intersect(&values).is_empty() {
                        break;
                    }
                    total_values = total_values.union(&values);
                    cases.push((values, i));
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    if cases.len() < 3 {
        return;
    }

    let default_case = if total_values.is_full() {
        let default_case_index = cases
            .iter()
            .enumerate()
            .max_by_key(|(_, (range, _))| range.size())
            .unwrap()
            .0;
        let default_case_index = cases.swap_remove(default_case_index).1;

        if let Stmt::If(_, stmt) = std::mem::take(&mut code[default_case_index]) {
            Some(Stmt::Seq(stmt))
        } else {
            unreachable!()
        }
    } else {
        None
    };

    cases.sort_by_key(|(range, _)| range.start());

    let mut cases_with_stmt = Vec::new();
    for (range, i) in cases {
        if let Stmt::If(_, stmt) = std::mem::take(&mut code[i]) {
            cases_with_stmt.push((range, Stmt::Seq(stmt)));
        } else {
            unreachable!();
        }
    }

    code.drain(start..(start + cases_with_stmt.len() + default_case.is_some() as usize));

    code.insert(
        start,
        Stmt::SwitchCase(MappedExpr::Mapped(expr), cases_with_stmt, default_case.map(Box::new)),
    );
}

fn is_switch_case_cond(cond: &Cond, switch_expr: Option<u32>) -> Option<u32> {
    match cond {
        Cond::True | Cond::False | Cond::Not(_) => None,
        Cond::And(left, right) | Cond::Or(left, right) => {
            let expr = is_switch_case_cond(left, switch_expr);
            if expr.is_some() && is_switch_case_cond(right, expr).is_some() {
                expr
            } else {
                None
            }
        }
        Cond::Cmp(left, _, right) => {
            if let Some(switch_expr) = switch_expr {
                match (left, right) {
                    (MappedExpr::Mapped(expr), MappedExpr::Const(_)) if *expr == switch_expr => Some(*expr),
                    (MappedExpr::Const(_), MappedExpr::Mapped(expr)) if *expr == switch_expr => Some(*expr),
                    _ => None,
                }
            } else {
                match (left, right) {
                    (MappedExpr::Mapped(expr), MappedExpr::Const(_)) => Some(*expr),
                    (MappedExpr::Const(_), MappedExpr::Mapped(expr)) => Some(*expr),
                    _ => None,
                }
            }
        }
        Cond::Expr(_) => None,
    }
}

fn find_case_values(cond: &Cond, switch_expr: u32) -> Option<ValueSpace> {
    match cond {
        Cond::True | Cond::False | Cond::Not(_) => None,
        Cond::And(left, right) => {
            Some(find_case_values(left, switch_expr)?.intersect(&find_case_values(right, switch_expr)?))
        }
        Cond::Or(left, right) => {
            Some(find_case_values(left, switch_expr)?.union(&find_case_values(right, switch_expr)?))
        }
        Cond::Cmp(left, op, right) => match (left, right) {
            (MappedExpr::Mapped(expr), MappedExpr::Const(val)) if *expr == switch_expr => {
                Some(ValueSpace::constrained_by(*op, *val))
            }
            (MappedExpr::Const(val), MappedExpr::Mapped(expr)) if *expr == switch_expr => {
                Some(ValueSpace::constrained_by(op.mirror(), *val))
            }
            _ => None,
        },
        Cond::Expr(_) => None,
    }
}
