// Adapted from https://github.com/paritytech/wasmi under the MIT license
//
// Copyright (c) 2018 Nikolay Volf
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

use core::cmp::{Ordering, PartialEq, PartialOrd};
use core::ops::{Add, Div, Mul, Neg, Rem, Sub};
use std::fmt;

macro_rules! impl_binop {
    ($for:ident, $is:ident, $op:ident, $func_name:ident) => {
        impl<T: Into<$for>> $op<T> for $for {
            type Output = Self;

            fn $func_name(self, other: T) -> Self {
                $for($op::$func_name($is::from_bits(self.0), $is::from_bits(other.into().0)).to_bits())
            }
        }
    };
}

macro_rules! float {
    ($for:ident, $rep:ident, $is:ident) => {
        float!($for, $rep, $is, 1 << (::core::mem::size_of::<$is>() * 8 - 1));
    };
    ($for:ident, $rep:ident, $is:ident, $sign_bit:expr) => {
        #[derive(Copy, Clone)]
        pub struct $for($rep);

        impl_binop!($for, $is, Add, add);
        impl_binop!($for, $is, Sub, sub);
        impl_binop!($for, $is, Mul, mul);
        impl_binop!($for, $is, Div, div);
        impl_binop!($for, $is, Rem, rem);

        impl $for {
            pub const fn from_bits(other: $rep) -> Self {
                $for(other)
            }

            pub const fn to_bits(self) -> $rep {
                self.0
            }

            pub fn from_float(fl: $is) -> Self {
                fl.into()
            }

            pub fn to_float(self) -> $is {
                self.into()
            }

            pub fn is_nan(self) -> bool {
                self.to_float().is_nan()
            }

            pub const fn abs(self) -> Self {
                $for(self.0 & !$sign_bit)
            }

            pub fn fract(self) -> Self {
                self.to_float().fract().into()
            }

            pub fn min(self, other: Self) -> Self {
                if self.is_nan() {
                    return self;
                }
                if other.is_nan() {
                    return other;
                }
                Self::from(self.to_float().min(other.to_float()))
            }

            pub fn max(self, other: Self) -> Self {
                if self.is_nan() {
                    return self;
                }
                if other.is_nan() {
                    return other;
                }
                Self::from(self.to_float().max(other.to_float()))
            }

            pub fn floor(self) -> Self {
                $is::from(self).floor().into()
            }

            pub fn ceil(self) -> Self {
                $is::from(self).floor().into()
            }

            pub fn trunc(self) -> Self {
                $is::from(self).floor().into()
            }

            pub fn round(self) -> Self {
                $is::from(self).floor().into()
            }

            #[allow(clippy::float_cmp)]
            pub fn nearest(self) -> Self {
                let round = self.round();
                if $is::from(self).fract().abs() != 0.5 {
                    return round;
                }

                use core::ops::Rem;
                if round.rem(2.0) == 1.0 {
                    self.floor()
                } else if round.rem(2.0) == -1.0 {
                    self.ceil()
                } else {
                    round
                }
            }

            pub fn sqrt(self) -> Self {
                $is::from(self).sqrt().into()
            }

            pub const fn copysign(self, other: Self) -> Self {
                let sign = other.0 & $sign_bit;
                Self::from_bits((self.0 & !$sign_bit) | sign)
            }

            #[allow(clippy::cast_lossless)]
            pub fn trunc_to_i32(self) -> Option<i32> {
                let self_float = self.to_float();
                if self.is_nan() || self_float >= -(i32::min_value() as $is) || self_float < i32::min_value() as $is {
                    None
                } else {
                    Some(self_float.trunc() as i32)
                }
            }

            pub fn trunc_to_i64(self) -> Option<i64> {
                let self_float = self.to_float();
                if self.is_nan() || self_float >= -(i64::min_value() as $is) || self_float < i64::min_value() as $is {
                    None
                } else {
                    Some(self_float.trunc() as i64)
                }
            }

            #[allow(clippy::cast_lossless)]
            pub fn trunc_to_u32(self) -> Option<u32> {
                let self_float = self.to_float();
                if self.is_nan() || self_float >= -(u32::min_value() as $is) * 2.0 || self_float <= -1.0 {
                    None
                } else {
                    Some(self_float.trunc() as u32)
                }
            }

            pub fn trunc_to_u64(self) -> Option<u64> {
                let self_float = self.to_float();
                if self.is_nan() || self_float >= -(u64::min_value() as $is) * 2.0 || self_float <= -1.0 {
                    None
                } else {
                    Some(self_float.trunc() as u64)
                }
            }
        }

        impl From<$is> for $for {
            fn from(other: $is) -> $for {
                $for(other.to_bits())
            }
        }

        impl From<$for> for $is {
            fn from(other: $for) -> $is {
                <$is>::from_bits(other.0)
            }
        }

        impl Neg for $for {
            type Output = Self;

            fn neg(self) -> Self {
                $for(self.0 ^ $sign_bit)
            }
        }

        impl<T: Into<$for> + Copy> PartialEq<T> for $for {
            fn eq(&self, other: &T) -> bool {
                $is::from(*self) == $is::from((*other).into())
            }
        }

        impl<T: Into<$for> + Copy> PartialOrd<T> for $for {
            fn partial_cmp(&self, other: &T) -> Option<Ordering> {
                $is::from(*self).partial_cmp(&$is::from((*other).into()))
            }
        }

        impl fmt::Debug for $for {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $is::from(*self).fmt(f)
            }
        }

        impl Default for $for {
            fn default() -> Self {
                Self::from_bits(0)
            }
        }

        impl fmt::Display for $for {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.to_float().fmt(f)
            }
        }
    };
}

float!(F32, u32, f32);
float!(F64, u64, f64);

impl From<u32> for F32 {
    fn from(other: u32) -> Self {
        Self::from_bits(other)
    }
}

impl From<F32> for u32 {
    fn from(other: F32) -> Self {
        other.to_bits()
    }
}

impl From<u64> for F64 {
    fn from(other: u64) -> Self {
        Self::from_bits(other)
    }
}

impl From<F64> for u64 {
    fn from(other: F64) -> Self {
        other.to_bits()
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;

    use super::{F32, F64};

    use core::{
        fmt::Debug,
        iter,
        ops::{Add, Div, Mul, Neg, Sub},
    };

    fn test_ops<T, F, I>(iter: I)
    where
        T: Add<Output = T>
            + Div<Output = T>
            + Mul<Output = T>
            + Sub<Output = T>
            + Neg<Output = T>
            + Copy
            + Debug
            + PartialEq,
        F: Into<T>
            + Add<Output = F>
            + Div<Output = F>
            + Mul<Output = F>
            + Sub<Output = F>
            + Neg<Output = F>
            + Copy
            + Debug,
        I: IntoIterator<Item = (F, F)>,
    {
        for (a, b) in iter {
            assert_eq!((a + b).into(), a.into() + b.into());
            assert_eq!((a - b).into(), a.into() - b.into());
            assert_eq!((a * b).into(), a.into() * b.into());
            assert_eq!((a / b).into(), a.into() / b.into());
            assert_eq!((-a).into(), -a.into());
            assert_eq!((-b).into(), -b.into());
        }
    }

    #[test]
    fn test_ops_f32() {
        let mut rng = rand::thread_rng();
        let iter = iter::repeat(()).map(|_| rng.gen());

        test_ops::<F32, f32, _>(iter.take(1000));
    }

    #[test]
    fn test_ops_f64() {
        let mut rng = rand::thread_rng();
        let iter = iter::repeat(()).map(|_| rng.gen());

        test_ops::<F64, f64, _>(iter.take(1000));
    }

    #[test]
    fn test_neg_nan_f32() {
        assert_eq!((-F32(0xff80_3210)).0, 0x7f80_3210);
    }

    #[test]
    fn test_neg_nan_f64() {
        assert_eq!((-F64(0xff80_3210_0000_0000)).0, 0x7f80_3210_0000_0000);
    }
}
