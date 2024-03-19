use basemod::ItemKind;

use crate::rate::Rate;

mod basemod;
mod rate;

fn main() {
    let iron_plate_item = Item { kind: basemod::ItemKind::IronPlate, module: basemod::MODULE };
    let copper_plate_item = Item { kind: basemod::ItemKind::CopperPlate, module: basemod::MODULE };
    let copper_wire_item = Item { kind: basemod::ItemKind::CopperWire, module: basemod::MODULE};
    let green_chip_item = Item { kind: basemod::ItemKind::GreenChip, module: basemod::MODULE };

    let iron_plates_recipe = Recipe {
        rate: Rate::UNIT,
        input: vec![],
        output: vec![
            RecipePart {
                item: iron_plate_item,
                amount: 1,
            }
        ]
    };
    let iron_plates = Stream {
        mult: 2,
        recipe: Box::new(&iron_plates_recipe),
        input: InputStreams::NONE
    };

    let copper_plates_recipe = Recipe {
        rate: Rate::UNIT,
        input: vec![],
        output: vec![
            RecipePart {
                item: copper_plate_item,
                amount: 1
            }
        ]
    };
    let copper_plates = Stream {
        mult: 3,
        recipe: Box::new(&copper_plates_recipe),
        input: InputStreams::NONE
    };

    let copper_wires_recipe = Recipe {
        rate: Rate::UNIT,
        input: vec![
            RecipePart {
                item: copper_plate_item,
                amount: 1
            }
        ],
        output: vec![
            RecipePart {
                item: copper_wire_item,
                amount: 2
            }
        ]
    };
    let copper_wires = Stream {
        mult: 3,
        recipe: Box::new(&copper_wires_recipe),
        input: vec![copper_plates].into()
    };

    let green_chips_recipe = Recipe {
        rate: Rate::UNIT,
        input: vec![
            RecipePart {
                item: iron_plate_item,
                amount: 1
            },
            RecipePart {
                item: copper_wire_item,
                amount: 3
            }
        ],
        output: vec![
            RecipePart {
                item: green_chip_item,
                amount: 1
            }
        ]
    };
    let green_chips = Stream {
        mult: 2,
        recipe: Box::new(&green_chips_recipe),
        input: vec![iron_plates, copper_wires].into()
    };

    let rate = green_chips.rate_of(green_chip_item).unwrap();
    println!("Green chips will be produced at {}% efficiency ({}/{:.2}s)", green_chips.efficiency() * 100.0, rate.amount, rate.time);
}

#[derive(Clone, Debug)]
pub struct Stream<'a> {
    pub mult: u64,
    pub recipe: Box<&'a Recipe>,
    pub input: InputStreams<'a>,
}

impl<'a> Stream<'a> {
    pub fn efficiency(&self) -> Efficiency {
        if self.input.inner.len() == 0 {
            return 1.0 as Efficiency;
        }

        self.recipe.input.iter().map(|i| {
            let rate = self.input.rate_of(i.item);
            let optimal_inflow = self.recipe.optimal_inflow_of(i.item);
            rate / (optimal_inflow * self.mult)
        }).reduce(Efficiency::min).unwrap_or(0.0).min(1.0)
    }

    pub fn rate_of(&self, item: Item) -> Option<Rate> {
        let outflow = self.recipe.optimal_outflow_of(item);

        if outflow != Rate::ZERO {
            let eff = self.efficiency();
            Some(outflow * eff * self.mult)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct InputStreams<'a> {
    inner: Vec<Stream<'a>>
}

impl<'a> InputStreams<'a> {
    pub const NONE: Self = Self { inner: vec![] };
}

impl<'a> From<Vec<Stream<'a>>> for InputStreams<'a> {
    fn from(value: Vec<Stream<'a>>) -> Self {
        Self { inner: value }
    }
}

impl<'a> InputStreams<'a> {
    pub fn rate_of(&self, item: Item) -> Rate {
        self.inner.iter().filter_map(|s| {
            s.rate_of(item)
        }).sum()
    }
}

pub type Efficiency = f64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Item {
    pub kind: ItemKind,
    pub module: u64,
}

#[derive(Clone, Debug)]
pub struct RecipePart {
    pub item: Item,
    pub amount: u64,
}

#[derive(Clone, Debug)]
pub struct Recipe {
    pub rate: Rate,
    pub input: Vec<RecipePart>,
    pub output: Vec<RecipePart>,
}

impl Recipe {
    pub fn optimal_inflow_of(&self, item: Item) -> Rate {
        self.input.iter().filter_map(|i| {
            if i.item == item {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f)
    }

    pub fn optimal_outflow_of(&self, item: Item) -> Rate {
        self.output.iter().filter_map(|i| {
            if i.item == item {
                Some(self.rate * i.amount)
            } else {
                None
            }
        }).fold(Rate::ZERO, |acc, f| acc + f)
    }
}