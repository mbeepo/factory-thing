use std::{fmt::Display, iter::Sum, ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign}};

use crate::Efficiency;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rate {
    /// Number of outputs per packet
    pub amount: usize,
    /// Number of output packets per tick
    pub ticks: f64,
}

impl Rate {
    pub const UNIT: Self = Self { amount: 1, ticks: 1.0 };
    pub const ZERO: Self = Self { amount: 0, ticks: 1.0 };
    pub fn normalize(&self) -> f64 {
        self.amount as f64 / self.ticks
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
            ticks: self.ticks / efficiency,
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
        self.ticks /= efficiency;
    }
}

impl Div<f64> for Rate {
    type Output = Rate;

    fn div(self, rhs: f64) -> Self::Output {
        Rate {
            amount: self.amount,
            ticks: self.ticks * rhs,
        }
    }
}

impl DivAssign<f64> for Rate {
    fn div_assign(&mut self, rhs: f64) {
        self.ticks *= rhs;
    }
}

impl Add<Rate> for Rate {
    type Output = Rate;

    fn add(self, rhs: Rate) -> Self::Output {
        if self.ticks == rhs.ticks || rhs.amount == 0 {
            Self {
                amount: self.amount + rhs.amount,
                ..self
            }
        } else if self.amount == 0 {
            Self {
                amount: self.amount + rhs.amount,
                ..rhs
            }  
        } else {
            let ticks = (self.normalize()) + (rhs.normalize());

            Self {
                amount: 1,
                ticks: 1.0 / ticks,
            }
        }
    }
}

impl AddAssign<Rate> for Rate {
    fn add_assign(&mut self, rhs: Rate) {
        let new = *self + rhs;
        self.amount = new.amount;
        self.ticks = new.ticks;
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

impl PartialOrd for Rate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // some weirdness with fractions
        self.normalize().partial_cmp(&other.normalize())
    }
}

impl Display for Rate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{:.2}ms", self.amount, self.ticks)
    }
}