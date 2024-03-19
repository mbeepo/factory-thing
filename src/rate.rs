use std::{iter::Sum, ops::{Add, AddAssign, Div, Mul, MulAssign}};

use crate::Efficiency;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rate {
    /// Number of outputs per packet
    pub amount: u64,
    /// Number of output packets per time unit
    pub time: f64,
}

impl Rate {
    pub const UNIT: Self = Self { amount: 1, time: 1.0 };
    pub const ZERO: Self = Self { amount: 0, time: 1.0 };
    pub fn normalize(&self) -> f64 {
        self.amount as f64 / self.time
    }
}

impl Mul<u64> for Rate {
    type Output = Rate;

    fn mul(self, rhs: u64) -> Self::Output {
        Rate {
            amount: self.amount * rhs,
            time: self.time,
        }
    }
}

impl Mul<f64> for Rate {
    type Output = Rate;
    
    fn mul(self, rhs: f64) -> Self::Output {
        Rate {
            amount: self.amount,
            time: self.time * 1.0 / rhs,
        }
    }
}

impl MulAssign<u64> for Rate {
    fn mul_assign(&mut self, rhs: u64) {
        self.amount *= rhs;
    }
}

impl MulAssign<f64> for Rate {
    fn mul_assign(&mut self, rhs: f64) {
        self.time *= 1.0 / rhs;
    }
}

impl Add<Rate> for Rate {
    type Output = Rate;

    fn add(self, rhs: Rate) -> Self::Output {
        if self.time == rhs.time {
            Self {
                amount: self.amount + rhs.amount,
                time: self.time
            }
        } else {
            let out = (self.normalize()) + (rhs.normalize());
            let time = 1.0 / out;

            Self {
                amount: 1,
                time
            }
        }
    }
}

impl AddAssign<Rate> for Rate {
    fn add_assign(&mut self, rhs: Rate) {
        let new = *self + rhs;
        self.amount = new.amount;
        self.time = new.time;
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