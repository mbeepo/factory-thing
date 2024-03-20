use std::collections::HashMap;

use crate::{Item, Recipe, Stream};

pub struct Factory<'a> {
    pub items: HashMap<String, Item>,
    pub recipes: HashMap<String, Recipe>,
    pub streams: HashMap<String, Stream<'a>>,
}

impl<'a> Factory<'a> {
}