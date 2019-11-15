use crate::ssa::{LoopKind, Stmt};

pub fn apply(code: &mut Vec<Stmt>) {
    for stmt in code {
        use Stmt::*;
        match stmt {
            While(cond, body, kind) => {
                while let Some(If(_, if_body)) = body.first() {
                    if let Some(Break) = if_body.first() {
                        if let If(if_cond, _) = body.remove(0) {
                            cond.and_inplace(if_cond.not());
                        }
                    } else {
                        break;
                    }
                }
                if cond.is_const_true() {
                    while let Some(If(_, if_body)) = body.last() {
                        if let Some(Break) = if_body.first() {
                            if let Some(If(if_cond, _)) = body.pop() {
                                cond.and_inplace(if_cond.not());
                                *kind = LoopKind::DoWhile;
                            }
                        } else {
                            break;
                        }
                    }
                }
                apply(body);
            }
            If(_, body) => {
                apply(body);
            }
            IfElse(_, true_body, false_body) => {
                apply(true_body);
                apply(false_body);
            }
            Seq(body) => {
                apply(body);
            }
            _ => (),
        }
    }
}
