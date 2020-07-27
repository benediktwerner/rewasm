use super::cond::CmpOp;

#[derive(Debug, Clone)]
pub struct ValueSpace(pub Vec<(u32, u32)>);

impl ValueSpace {
    pub const fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn is_full(&self) -> bool {
        if let Some((0, u32::MAX)) = self.0.first() {
            true
        } else {
            false
        }
    }

    pub fn size(&self) -> u32 {
        self.0.iter().copied().map(|(start, end)| end - start + 1).sum()
    }

    pub fn start(&self) -> u32 {
        self.0[0].0
    }

    pub fn constrained_by(op: CmpOp, val: u32) -> Self {
        match op {
            CmpOp::Eq => Self(vec![(val, val)]),
            CmpOp::Neq => Self(vec![(0, val - 1), (val + 1, u32::MAX)]),
            CmpOp::Geq => Self(vec![(val, u32::MAX)]),
            CmpOp::Gt if val < u32::MAX => Self(vec![(val + 1, u32::MAX)]),
            CmpOp::Gt => Self(vec![]),
            CmpOp::Leq => Self(vec![(0, val)]),
            CmpOp::Lt if val > 0 => Self(vec![(0, val - 1)]),
            CmpOp::Lt => Self(vec![]),
        }
    }

    // pub fn constrain_by(&mut self, op: CmpOp, val: u32) {
    //     match op {
    //         CmpOp::Eq => {
    //             for (start, end) in self.0 {
    //                 if start <= val && val <= end {
    //                     self.0[0] = (val, val);
    //                     self.0.truncate(1);
    //                     return;
    //                 }
    //             }
    //             self.0.clear();
    //         }
    //         CmpOp::Neq => {
    //             for (i, (start, end)) in self.0.iter().copied().enumerate() {
    //                 if start <= val && val <= end {
    //                     if start == end {
    //                         self.0.remove(i);
    //                     } else if val == start {
    //                         self.0[i] = (start + 1, end);
    //                     } else if val == end {
    //                         self.0[i] = (start, end - 1);
    //                     } else {
    //                         self.0[i] = (start, val - 1);
    //                         self.0.insert(i + 1, (val + 1, end));
    //                         return;
    //                     }
    //                     break;
    //                 }
    //             }
    //         }
    //         CmpOp::Geq => {
    //             self.0.retain(|(start, end)| *end >= val);
    //             if let Some((start, end)) = self.0.first().copied() {
    //                 if start < val {
    //                     self.0[0] = (val, end);
    //                 }
    //             }
    //         }
    //         CmpOp::Gt => {
    //             self.0.retain(|(start, end)| *end > val);
    //             if let Some((start, end)) = self.0.first().copied() {
    //                 if start <= val {
    //                     self.0[0] = (val + 1, end);
    //                 }
    //             }
    //         }
    //         CmpOp::Leq => {
    //             self.0.retain(|(start, end)| *start <= val);
    //             if let Some((start, end)) = self.0.last().copied() {
    //                 if end > val {
    //                     self.0[self.0.len() - 1] = (start, val);
    //                 }
    //             }
    //         }
    //         CmpOp::Lt => {
    //             self.0.retain(|(start, end)| *start < val);
    //             if let Some((start, end)) = self.0.last().copied() {
    //                 if end > val {
    //                     self.0[self.0.len() - 1] = (start, val - 1);
    //                 }
    //             }
    //         }
    //     }
    // }

    pub fn intersect(&self, other: &Self) -> Self {
        let mut result = Vec::new();

        let mut a_iter = self.0.iter().copied();
        let mut b_iter = other.0.iter().copied();

        let mut a = a_iter.next();
        let mut b = b_iter.next();

        while let (Some(av), Some(bv)) = (a, b) {
            if av.1 < bv.0 {
                a = a_iter.next();
            } else if bv.1 < av.0 {
                b = b_iter.next();
            } else {
                let start = av.0.max(bv.0);
                let end = av.1.min(bv.1);
                result.push((start, end));
                if av.1 <= bv.1 {
                    a = a_iter.next();
                }
                if bv.1 <= av.1 {
                    b = b_iter.next();
                }
            }
        }

        Self(result)
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut result = Vec::new();

        let mut a_iter = self.0.iter().copied();
        let mut b_iter = other.0.iter().copied();

        let mut a = a_iter.next();
        let mut b = b_iter.next();

        while let (Some(av), Some(bv)) = (a, b) {
            if av.1 < bv.0 && av.1 + 1 != bv.0 {
                result.push(av);
                a = a_iter.next();
            } else if bv.1 < av.0 && bv.1 + 1 != av.0 {
                result.push(bv);
                b = b_iter.next();
            } else if av.1 == bv.1 {
                result.push((av.0.min(bv.0), av.1));
                a = a_iter.next();
                b = b_iter.next();
            } else if av.1 < bv.1 {
                b = Some((av.0.min(bv.0), bv.1));
                a = a_iter.next();
            } else {
                a = Some((av.0.min(bv.0), av.1));
                b = b_iter.next();
            }
        }

        while let Some(av) = a {
            result.push(av);
            a = a_iter.next();
        }

        while let Some(bv) = b {
            result.push(bv);
            b = b_iter.next();
        }

        Self(result)
    }
}

impl crate::fmt::CodeDisplay for ValueSpace {
    fn fmt_code(&self, f: &mut crate::fmt::CodeWriter) {
        if let Some(first) = self.0.first() {
            write_range(f, *first);
            for range in self.0.iter().copied().skip(1) {
                f.write(" | ");
                write_range(f, range);
            }
        }
    }
}

fn write_range(f: &mut crate::fmt::CodeWriter, (start, end): (u32, u32)) {
    if start == end {
        write!(f, "{}", start);
    } else {
        write!(f, "{}..{}", start, end);
    }
}
