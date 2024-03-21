use std::{iter::Sum, ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign}};

use crate::Efficiency;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rate {
    /// Number of outputs per packet
    pub amount: usize,
    /// Number of output packets per time unit
    pub time: f64,
}

impl Rate {
    pub const UNIT: Self = Self { amount: 1, time: 1.0 };
    pub const ZERO: Self = Self { amount: 0, time: 1.0 };
    pub fn normalize(&self) -> f64 {
        self.time / if self.amount == 0 { 1.0 } else { self.amount as f64 }
    }
}

impl Mul<usize> for Rate {
    type Output = Rate;

    fn mul(self, rhs: usize) -> Self::Output {
        assert!(rhs != 0);

        Rate {
            amount: self.amount * rhs,
            ..self
        }
    }
}

impl Mul<f64> for Rate {
    type Output = Rate;
    
    fn mul(self, efficiency: f64) -> Self::Output {
        Rate {
            time: self.time / efficiency,
            ..self
        }
    }
}

impl MulAssign<usize> for Rate {
    fn mul_assign(&mut self, rhs: usize) {
        assert!(rhs != 0);
        self.amount *= rhs;
    }
}

impl MulAssign<f64> for Rate {
    fn mul_assign(&mut self, efficiency: f64) {
        self.time /= efficiency;
    }
}

impl Div<f64> for Rate {
    type Output = Rate;

    fn div(self, efficiency: f64) -> Self::Output {
        Rate {
            amount: self.amount,
            time: self.time / efficiency,
        }
    }
}

impl DivAssign<f64> for Rate {
    fn div_assign(&mut self, efficiency: f64) {
        self.time /= efficiency;
    }
}

impl Add<Rate> for Rate {
    type Output = Rate;

    fn add(self, rhs: Rate) -> Self::Output {
        if self.time == rhs.time {
            Self {
                amount: self.amount + rhs.amount,
                ..self
            }
        } else {
            let time = (self.normalize()) + (rhs.normalize());
            println!("{} + {} = {time}", self.normalize(), rhs.normalize());

            Self {
                amount: 1,
                time,
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
        self.normalize() / rhs.normalize()
    }
}

impl Sum<Rate> for Rate {
    fn sum<I: Iterator<Item = Rate>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |acc, r| acc + r)
    }
}