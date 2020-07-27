use crate::ssa::{LoopKind, Stmt};

pub fn apply(code: &mut Vec<Stmt>) {
    let mut for_loops = Vec::new();

    for (i, stmt) in code.iter_mut().enumerate() {
        use Stmt::*;
        match stmt {
            While(cond, body, kind) => {
                while let Some(If(_, if_body)) = body.first() {
                    if let Some(Break) = if_body.first() {
                        if let If(if_cond, _) = body.remove(0) {
                            cond.and_inplace(!if_cond);
                        }
                    } else {
                        break;
                    }
                }

                if cond.is_const_true() {
                    while let Some(If(_, if_body)) = body.last() {
                        if let Some(Break) = if_body.first() {
                            if let Some(If(if_cond, _)) = body.pop() {
                                cond.and_inplace(!if_cond);
                                *kind = LoopKind::DoWhile;
                            }
                        } else {
                            break;
                        }
                    }
                }

                if *kind == LoopKind::While {
                    let vars = cond.find_vars();
                    if let Some(Stmt::SetLocal(var, _)) = body.last() {
                        if vars.contains(var) {
                            if let Some(Stmt::SetLocal(var, post)) = body.pop() {
                                for_loops.push((i, var, post));
                            }
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

    let mut shift = 0;

    for (mut i, var, post) in for_loops {
        i -= shift;
        let mut init = None;
        if i > 0 {
            if let Some(Stmt::SetLocal(var2, _)) = code.get(i - 1) {
                if *var2 == var {
                    i -= 1;
                    shift += 1;
                    if let Stmt::SetLocal(_, init2) = code.remove(i) {
                        init = Some(init2);
                    }
                }
            }
        }
        replace_with::replace_with_or_abort(&mut code[i], |lop| {
            if let Stmt::While(cond, body, _) = lop {
                Stmt::ForLoop(var, init, cond, post, body)
            } else {
                unreachable!()
            }
        })
    }
}
