use std::collections::HashSet;

use crate::ssa::{Cond, Stmt};

pub fn apply(code: &mut Vec<Stmt>) {
    if code.is_empty() {
        return;
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
