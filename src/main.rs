use std::{cell::RefCell, collections::HashMap, rc::Rc};

use chumsky::Parser;

use crate::{factory::Factory, rate::Rate};

mod factory;
mod lang;
mod rate;

fn main() {
    let source = include_str!("../assets/example/bigamount.bp");
    let lex = lang::lexer().parse(source).unwrap();
    let ast = lang::parser().parse(lex).unwrap();
    let mut factory = Factory::new();
    factory.add_mod(ast).unwrap();
}

#[derive(Clone, Copy, Debug, PartialEq, Hash, Default)]
pub struct Buffer {
    pub current: usize,
    pub max: usize,
}

impl Buffer {
    pub const ZERO: Self = Buffer { current: 0, max: 0 };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stream {
    pub mult: usize,
    pub recipe: Rc<RefCell<Recipe>>,
    pub inputs: InputStreams,
    pub buffer: HashMap<Product, Buffer>,
}

impl Stream {
    pub fn efficiency(&self) -> Efficiency {
        if self.inputs.inner.len() == 0 {
            return 1.0 as Efficiency;
        }

        self.recipe.borrow().inputs.iter().map(|i| {
            let rate = self.inputs.rate_of(&*i.product.borrow());
            let optimal_inflow = self.recipe.borrow().optimal_inflow_of(&*i.product.borrow()).unwrap();
            // println!("{} / ({} * {}) => {}%", rate, optimal_inflow, self.mult, (rate / (optimal_inflow * self.mult)) * 100.0);
            rate / (optimal_inflow * self.mult)
        }).reduce(Efficiency::min).unwrap_or(0.0).min(1.0)
    }

    pub fn rate_of(&self, product: &Product) -> Option<Rate> {
        let outflow = self.recipe.borrow().optimal_outflow_of(product)?;

        let eff = self.efficiency();
        Some(outflow * eff * self.mult)
    }

    pub fn until_full(&self, product: &Product) -> Option<usize> {
        let buffer = self.buffer.get(product)?;

        if buffer.max > 0 {
            let rate = self.rate_of(product)?;
            let packets = (buffer.max - buffer.current) / rate.amount;
            let time = packets as f64 * rate.time;

            Some(time as usize)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputStreams {
    inner: Vec<Rc<RefCell<Stream>>>
}

impl InputStreams {
    pub const NONE: Self = Self { inner: vec![] };
}

impl From<Vec<Rc<RefCell<Stream>>>> for InputStreams {
    fn from(value: Vec<Rc<RefCell<Stream>>>) -> Self {
        Self { inner: value }
    }
}

impl InputStreams {
    pub fn rate_of(&self, product: &Product) -> Rate {
        self.inner.iter().filter_map(|s| {
            s.borrow().rate_of(product)
        }).sum()
    }
}

pub type Efficiency = f64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Product {
    pub id: usize,
    pub module: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecipePart {
    pub product: Rc<RefCell<Product>>,
    pub amount: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Recipe {
    pub rate: Rate,
    pub inputs: Vec<RecipePart>,
    pub outputs: Vec<RecipePart>,
}

impl Recipe {
    pub fn optimal_inflow_of(&self, product: &Product) -> Option<Rate> {
        let inflow = self.inputs.iter().filter_map(|i| {
            if &*i.product.borrow() == product {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f);

        if inflow == Rate::ZERO {
            None
        } else {
            Some(inflow)
        }
    }

    pub fn optimal_outflow_of(&self, product: &Product) -> Option<Rate> {
        let outflow = self.outputs.iter().filter_map(|i| {
            if &*i.product.borrow() == product {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f);
        
        if outflow == Rate::ZERO {
            None
        } else {
            Some(outflow)
        }
    }
}