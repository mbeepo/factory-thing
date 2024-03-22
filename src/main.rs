use std::{cell::RefCell, collections::HashMap, rc::Rc};

use chumsky::Parser;

use crate::{factory::Factory, rate::Rate};

mod factory;
mod lang;
mod rate;

fn main() {
    let source = include_str!("../assets/example/basic.bp");
    let lex = lang::lexer().parse(source).unwrap();
    let ast = lang::parser().parse(lex).unwrap();
    let mut factory = Factory::new();
    factory.add_mod(ast).unwrap();

    let iron_plate_product = factory.products.get("__BASE::iron_plate").unwrap();
    let iron_plates = factory.streams.get("__BASE::ironPlates").unwrap();
    let iron_plate_rate = iron_plates.borrow().rate_of(&iron_plate_product.borrow()).unwrap();

    let copper_plate_product = factory.products.get("__BASE::copper_plate").unwrap();
    let copper_plates = factory.streams.get("__BASE::copperPlates").unwrap();
    let copper_plate_rate = copper_plates.borrow().rate_of(&copper_plate_product.borrow()).unwrap();

    let copper_wire_product = factory.products.get("__BASE::copper_wire").unwrap();
    let copper_wire = factory.streams.get("__BASE::copperWire").unwrap();
    let copper_wire_rate = copper_wire.borrow().rate_of(&copper_wire_product.borrow()).unwrap();

    let green_chip_product = factory.products.get("__BASE::green_chip").unwrap();
    let green_chips = factory.streams.get("__BASE::greenChips").unwrap();
    let green_chip_rate = green_chips.borrow().rate_of(&green_chip_product.borrow()).unwrap();

    println!("Iron plates working at {:.1}% efficiency", iron_plates.borrow().efficiency() * 100.0);
    println!("{} iron plates will be made every {}ms", iron_plate_rate.amount, iron_plate_rate.time);
    println!("Iron plate buffer will be full in {}ms", iron_plates.borrow().until_full(&iron_plate_product.borrow()).unwrap_or(0));
    println!();

    println!("Copper plates working at {:.1}% efficiency", copper_plates.borrow().efficiency() * 100.0);
    println!("{} copper plates will be made every {}ms", copper_plate_rate.amount, copper_plate_rate.time);
    println!("Copper plate buffer will be full in {}ms", copper_plates.borrow().until_full(&copper_plate_product.borrow()).unwrap_or(0));
    println!();

    println!("Copper wire working at {:.1}% efficiency", copper_wire.borrow().efficiency() * 100.0);
    println!("{} copper wire will be made every {}ms", copper_wire_rate.amount, copper_wire_rate.time);
    println!("Copper wire buffer will be full in {}ms", copper_wire.borrow().until_full(&copper_wire_product.borrow()).unwrap_or(0));
    println!();

    println!("Green chips working at {:.1}% efficiency", green_chips.borrow().efficiency() * 100.0);
    println!("{} green chips will be made every {}ms", green_chip_rate.amount, green_chip_rate.time);
    println!("Green chip buffer will be full in {}ms", green_chips.borrow().until_full(&green_chip_product.borrow()).unwrap_or(0));
    println!();
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
            let optimal_inflow = self.recipe.borrow().optimal_inflow_of(&*i.product.borrow());
            rate / (optimal_inflow * self.mult)
        }).reduce(Efficiency::min).unwrap_or(0.0).min(1.0)
    }

    pub fn rate_of(&self, product: &Product) -> Option<Rate> {
        let outflow = self.recipe.borrow().optimal_outflow_of(product);

        if outflow != Rate::ZERO {
            let eff = self.efficiency();
            Some(outflow * eff * self.mult)
        } else {
            None
        }
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
    pub fn optimal_inflow_of(&self, product: &Product) -> Rate {
        self.inputs.iter().filter_map(|i| {
            if &*i.product.borrow() == product {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f)
    }

    pub fn optimal_outflow_of(&self, product: &Product) -> Rate {
        self.outputs.iter().filter_map(|i| {
            if &*i.product.borrow() == product {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f)
    }
}