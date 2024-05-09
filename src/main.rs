use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt::Display, fs::{read_dir, read_to_string}, path::Path, rc::Rc, thread::sleep, time::Duration};

use chumsky::Parser;
use factory::Knowledge;
use lang::parser::Expr;

use crate::{factory::Factory, rate::Rate};

mod factory;
mod lang;
mod rate;

fn main() {
    let basemod_path = Path::new("assets/mods/basemod");
    let basemod = read_dir(basemod_path).unwrap();
    let mut basemod_src = String::with_capacity(1024);
    
    for entry in basemod {
        if let Ok(entry) = entry {
            if entry.metadata().unwrap().is_file() {
                if let Ok(src) = read_to_string(Path::join(basemod_path, entry.file_name())) {
                    basemod_src += &src;
                }
            }
        }
    }

    let lex = lang::lexer().parse(basemod_src).unwrap();
    let ast = lang::parser().parse(lex).unwrap();
    let mut factory = Factory::new();
    factory.add_mod(ast).unwrap();

    let factory_src = read_to_string("assets/factory/main.bp").unwrap();

    let lex = lang::lexer().parse(factory_src).unwrap();
    let ast = lang::parser().parse(lex).unwrap();
    factory.add_factory(ast).unwrap();

    let dur = Duration::from_millis(250);

    loop {
        sleep(dur);
        factory.tick(1000);

        for (name, stream) in factory.streams.iter() {
            println!("{name}: (next in {} ticks)", stream.borrow().next.unwrap_or(99999));

            for (product, buffer) in stream.borrow().buffers.iter() {
                println!("  {}: {buffer}", factory.product_names.get(product).unwrap());
            }
        }

        for (name, knowledge) in factory.knowledge.iter() {
            println!("{name}: {}", knowledge.borrow().progress);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Hash, Default)]
pub struct Buffer {
    pub current: usize,
    pub max: usize,
}

impl Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.current, self.max)
    }
}

impl Buffer {
    pub const ZERO: Self = Buffer { current: 0, max: 0 };

    pub fn space_left(&self) -> usize {
        self.max - self.current
    }

    pub fn fill_from(&mut self, other: &mut Buffer) {
        if other.current < self.space_left() {
            self.current += other.current;
            other.current = 0;
        } else {
            other.current -= self.space_left();
            self.current = self.max;
        }
    }

    pub fn fill_by(&mut self, amount: usize) {
        self.current += self.space_left().min(amount);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stream {
    pub mult: usize,
    pub recipe: Rc<RefCell<Recipe>>,
    pub inputs: InputStreams,
    pub buffers: HashMap<Product, Buffer>,
    /// How many ticks until next output if currently producing, or None if waiting for inputs
    pub next: Option<usize>,
    /// Maximum ticks between outputs
    pub ticks: usize,
}

impl Stream {
    pub fn efficiency(&self) -> Efficiency {
        if self.inputs.inner.len() == 0 {
            return 1.0 as Efficiency;
        }

        self.recipe.borrow().inputs.iter().map(|i| {
            let rate = self.inputs.rate_of(&*i.product.borrow());
            let optimal_inflow = self.recipe.borrow().optimal_inflow_of(&*i.product.borrow()).unwrap();
            // println!("{}: {} / ({} * {}) => {}%", i.product.borrow().id, rate, optimal_inflow, self.mult, (rate / (optimal_inflow * self.mult)) * 100.0);
            rate / (optimal_inflow * self.mult)
        }).reduce(Efficiency::min).unwrap_or(0.0).min(1.0)
    }

    pub fn rate_of(&self, product: &Product) -> Option<Rate> {
        let outflow = self.recipe.borrow().optimal_outflow_of(product)?;

        let eff = self.efficiency();
        Some(outflow * eff * self.mult)
    }

    pub fn optimal_inflow_of(&self, product: &Product) -> Option<Rate> {
        let inflow = self.recipe.borrow().optimal_inflow_of(product)?;

        Some(inflow * self.mult)
    }

    pub fn until_full(&self, product: &Product) -> Option<usize> {
        let buffer = self.buffers.get(product)?;

        if buffer.max > 0 {
            let rate = self.rate_of(product)?;
            let packets = (buffer.max - buffer.current) / rate.amount;
            let ticks = packets as f64 * rate.ticks;

            Some(ticks as usize)
        } else {
            None
        }
    }
    
    // before calling this, available products should be moved from output buffers into this stream's input buffers
    pub fn try_start_produce(&mut self) -> bool {
        let mut to_satisfy = self.recipe.borrow().inputs.len();

        for input in self.recipe.borrow().inputs.clone() {
            let buffered = self.buffers.get(&*input.product.borrow()).map(|b| b.current).unwrap_or(0);
            
            if buffered >= self.recipe.borrow().required_of(&*input.product.borrow()).unwrap() * self.mult {
                to_satisfy -= 1;
            }
        }

        if to_satisfy == 0 {
            if self.recipe.borrow().outputs.iter().all(|output| {
                output.amount * self.mult <= self.buffers.get(&*output.product.borrow()).unwrap().space_left()
            }) {
                for output in &self.recipe.borrow().outputs {
                    let existing = self.buffers.get_mut(&*output.product.borrow()).unwrap();
    
                    existing.current += output.amount * self.mult;
                }
    
                for (knowledge, amount) in &self.recipe.borrow().knowledge {
                    if knowledge.borrow().unlockable() {
                        knowledge.borrow_mut().progress_by(amount * self.mult);
                    }
                }
    
                for input in self.recipe.borrow().inputs.clone() {
                    let buffered = self.buffers.get_mut(&*input.product.borrow()).unwrap();
                    buffered.current -= input.amount * self.mult;
                }
    
                self.next = Some(self.ticks);
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputStreams {
    inner: Vec<(Rc<RefCell<Product>>, Rc<RefCell<Stream>>)>
}

impl InputStreams {
    pub const NONE: Self = Self { inner: vec![] };
}

impl From<Vec<(Rc<RefCell<Product>>, Rc<RefCell<Stream>>)>> for InputStreams {
    fn from(value: Vec<(Rc<RefCell<Product>>, Rc<RefCell<Stream>>)>) -> Self {
        Self { inner: value }
    }
}

impl InputStreams {
    pub fn rate_of(&self, product: &Product) -> Rate {
        self.inner.iter().filter_map(|s| {
            s.1.borrow().rate_of(product)
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
    pub knowledge: Vec<(Rc<RefCell<Knowledge>>, usize)>,
    pub unlocked: bool,
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

    pub fn required_of(&self, product: &Product) -> Option<usize> {
        let amount = self.inputs.iter().filter_map(|i| {
            if &*i.product.borrow() == product {
                Some(i.amount)
            } else {
                None
            }
        }).sum();

        if amount == 0 {
            None
        } else {
            Some(amount)
        }
    }
}