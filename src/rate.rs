use std::{iter::Sum, ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign}};

use crate::Efficiency;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rate {
    /// Number of outputs per packet
    pub amount: usize,
    /// Number of output packets per time unit
    pub freq: f64,
}

impl Rate {
    pub const UNIT: Self = Self { amount: 1, freq: 1.0 };
    pub const ZERO: Self = Self { amount: 0, freq: 1.0 };
    pub fn normalize(&self) -> f64 {
        self.amount as f64 / self.freq
    }
}

impl Mul<usize> for Rate {
    type Output = Rate;

    fn mul(self, rhs: usize) -> Self::Output {
        Rate {
            amount: self.amount * rhs,
            freq: self.freq,
        }
    }
}

impl Mul<f64> for Rate {
    type Output = Rate;
    
    fn mul(self, rhs: f64) -> Self::Output {
        Rate {
            amount: self.amount,
            freq: self.freq / rhs,
        }
    }
}

impl MulAssign<usize> for Rate {
    fn mul_assign(&mut self, rhs: usize) {
        self.amount *= rhs;
    }
}

impl MulAssign<f64> for Rate {
    fn mul_assign(&mut self, rhs: f64) {
        self.freq /= rhs;
    }
}

impl Div<f64> for Rate {
    type Output = Rate;

    fn div(self, rhs: f64) -> Self::Output {
        Rate {
            amount: self.amount,
            freq: self.freq * rhs,
        }
    }
}

impl DivAssign<f64> for Rate {
    fn div_assign(&mut self, rhs: f64) {
        self.freq *= rhs;
    }
}

impl Add<Rate> for Rate {
    type Output = Rate;

    fn add(self, rhs: Rate) -> Self::Output {
        if self.freq == rhs.freq {
            Self {
                amount: self.amount + rhs.amount,
                freq: self.freq
            }
        } else {
            let out = (self.normalize()) + (rhs.normalize());
            let time = 1.0 / out;

            Self {
                amount: 1,
                freq: time
            }
        }
    }
}

impl AddAssign<Rate> for Rate {
    fn add_assign(&mut self, rhs: Rate) {
        let new = *self + rhs;
        self.amount = new.amount;
        self.freq = new.freq;
    }
}

impl Div<Rate> for Rate {
    type Output = Efficiency;

    fn div(self, rhs: Rate) -> Self::Output {
        (self.normalize()) / (rhs.normalize())
    }
}

impl Sum<Rate> for Rate {
    fn sum<I: Iterator<Item = Rate>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |acc, r| acc + r)
    }
}