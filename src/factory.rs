use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{lang::parser::{Expr, InfixOp, Literal}, rate::Rate, Buffer, Product, Recipe, RecipePart, Stream};

#[derive(Clone, Debug)]
pub struct Factory {
    pub products: HashMap<String, Rc<RefCell<Product>>>,
    pub product_names: HashMap<Product, String>,
    pub recipes: HashMap<String, Rc<RefCell<Recipe>>>,
    pub streams: HashMap<String, Rc<RefCell<Stream>>>,
    pub modules: HashMap<String, usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Product(String, Rc<RefCell<Product>>),
    Recipe(String, Rc<RefCell<Recipe>>),
    Stream(String, Rc<RefCell<Stream>>),
    RecipePart(RecipePart),
    Call(Box<Value>, Vec<Value>),
    MultRecipe(Box<Value>, usize),
    Method(Box<Method>),
    Int(isize),
    Float(f64),
    String(String),
    Bool(bool)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Method {
    pub object: Value,
    pub name: String,
}

#[derive(Clone, Copy, Debug)]
pub enum FactoryError {
    Glorp,
    Gleep,
    Exists,
    InvalidArguments,
    ItemNotFound,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Self::Product(name, _) => format!("Product {{ {name} }}"),
            Self::Recipe(name, _) => format!("Recipe {{ {name} }}"),
            Self::Stream(name, _) => format!("Stream {{ {name} }}"),
            Self::Call(lhs, rhs) => {
                let rhs = rhs.iter().fold(String::new(), |acc, e| format!("{acc}, {e}"));
                format!("Call {{ {lhs}({}) }}", rhs)
            },
            e => format!("{:?}", e),
        };

        write!(f, "{}", content)
    }
}

impl Factory {
    pub fn new() -> Self {
        let mut products = HashMap::new();
        let product_names = HashMap::new();
        let recipes = HashMap::new();
        let streams = HashMap::new();
        let mut modules = HashMap::new();

        products.insert("__next".to_owned(), Rc::new(RefCell::new(Product { id: 0, module: 0 })));
        modules.insert("factory".to_owned(), 0);

        Self {
            products,
            product_names,
            recipes,
            streams,
            modules,
        }
    }

    pub fn solve(&mut self, stream: Rc<RefCell<Stream>>) {
        let efficiency = stream.borrow().efficiency();
        let mut changes: Vec<(Rc<RefCell<Stream>>, usize)> = Vec::with_capacity(4);

        if efficiency < 1.0 {
            // balance and solve each input
            for input in &stream.borrow().inputs.inner {
                for ingredient in &stream.borrow().recipe.borrow().inputs {
                    let product = &*ingredient.product.borrow();
                    if let Some(optimal) = stream.borrow().recipe.borrow().optimal_inflow_of(product) {
                        let optimal = optimal * stream.borrow().mult;

                        if let Some(rate) = input.borrow().recipe.borrow().optimal_outflow_of(product) {
                            let rate = rate * input.borrow().mult;

                            if rate < optimal {
                                let efficiency = rate / optimal;
                                let mult = 1.0 / efficiency;
                                let new_mult = input.borrow().mult as f64 * mult;
                                changes.push((input.clone(), (new_mult - f64::EPSILON).ceil() as usize));
                            }
                        }
                        
                        if let Some(rate) = input.borrow().rate_of(product) {
                            if rate < optimal {
                                println!("{} < {}", rate, optimal);
                                self.solve(input.clone());
                            } else {
                                println!("{} > {}", rate, optimal);
                            }
                        }
                    }
                }
            }
        }

        for (stream, mult) in changes {
            println!("x{mult}");
            stream.borrow_mut().mult = mult;
            self.solve(stream.clone());
        }
    }

    pub fn add_mod(&mut self, ast: Vec<Expr>) -> Result<(), FactoryError> {
        for expr in ast {
            self.process_expr(expr, "factory")?;
        }

        Ok(())
    }

    fn process_expr(&mut self, expr: Expr, module: &str) -> Result<Option<Value>, FactoryError> {
        match expr {
            Expr::Product { name } => {
                self.register_product(&name, module)?;
                Ok(None)
            },
            Expr::Recipe { name, inputs, outputs, period } => {
                self.register_recipe(&name, inputs, outputs, *period, module)?;
                Ok(None)
            },
            Expr::Assign { name, rhs } => {
                self.register_stream(&name, *rhs, module)?;
                Ok(None)
            }
            Expr::Ident(ident) => {
                let id = id(&ident, module);
               
                if let Some(stream) = self.streams.get(&id) {
                    Ok(Some(Value::Stream(id, stream.clone())))
                } else if let Some(recipe) = self.recipes.get(&id) {
                    Ok(Some(Value::Recipe(id, recipe.clone())))
                } else if let Some(product) = self.products.get(&id) {
                    Ok(Some(Value::Product(id, product.clone())))
                } else {
                    panic!("Undefined identifier: {ident}");
                }
            },
            Expr::Call { lhs, args } => {
                let lhs = self.process_expr(*lhs, module)?.unwrap();
                let mut args_out = Vec::with_capacity(args.len());

                for expr in args {
                    if let Some(value) = self.process_expr(expr, module)? {
                        args_out.push(value);
                    } else {
                        return Err(FactoryError::Glorp)
                    }
                }

                match lhs {
                    Value::Method(method) => {
                        self.call(*method, args_out)
                    },
                    Value::Recipe(..) => {
                        Ok(Some(Value::Call(Box::new(lhs), args_out)))
                    },
                    _ => unimplemented!()
                }
            }
            Expr::InfixOp { lhs, op, rhs } => {
                // unwrapping is fine here cause only expressions that return Some from this method should be put in an InfixOp
                let lhs = self.process_expr(*lhs, module)?.unwrap();
                let rhs = self.process_expr(*rhs, module)?.unwrap();

                Ok(Some(self.process_op(lhs, op, rhs)))
            },
            Expr::Literal(literal) => {
                Ok(Some(match literal {
                    Literal::Int(e) => Value::Int(e),
                    Literal::Float(e)  => Value::Float(e),
                    Literal::String(e) => Value::String(e),
                    Literal::Bool(e) => Value::Bool(e),
                }))
            },
            Expr::Access { lhs, rhs } => {
                let lhs = self.process_expr(*lhs, module)?.unwrap();

                Ok(Some(lhs.access(&rhs)))
            },
            _ => todo!("{:?}", expr),
        }
    }

    fn process_op(&self, lhs: Value, op: InfixOp, rhs: Value) -> Value {
        match (lhs.clone(), op, rhs.clone()) {
            (Value::Product(_, product), InfixOp::Mul, Value::Int(amount))
            | (Value::Int(amount), InfixOp::Mul, Value::Product(_, product)) => {
                Value::RecipePart(RecipePart { product, amount: amount as usize })
            },
            (Value::Call(..), InfixOp::Mul, Value::Int(mult))
            | (Value::Int(mult), InfixOp::Mul, Value::Call(..)) => {
                Value::MultRecipe(Box::new(lhs), mult as usize)
            },
            (Value::MultRecipe(recipe, mult), InfixOp::Mul, Value::Int(mult2))
            | (Value::Int(mult2), InfixOp::Mul, Value::MultRecipe(recipe, mult)) => {
                Value::MultRecipe(recipe, mult * mult2 as usize)
            },
            (Value::Int(lhs), InfixOp::Mul, Value::Int(rhs)) => Value::Int(lhs * rhs),
            (lhs, op, rhs) => panic!("Invalid operation: `{lhs:?} {op:?} {rhs:?}`"),
        }
    }

    fn register_product(&mut self, name: &str, module: &str) -> Result<(), FactoryError> {
        if self.products.get(name).is_none() {
            let module_id = self.get_module(module);
            let product_id = self.products.get("__next").map(|i| i.borrow().id).unwrap_or(0);
            let product = Product { id: product_id, module: module_id };
            let name = id(name, module);

            self.products.insert("__next".to_owned(), Rc::new(RefCell::new(Product { id: product_id + 1, module: 0 })));
            self.products.insert(name.clone(), Rc::new(RefCell::new(product)));
            self.product_names.insert(product, name);

            Ok(())
        } else {
            Err(FactoryError::Exists)
        }
    }

    fn register_recipe(&mut self, name: &str, inputs: Vec<Expr>, outputs: Vec<Expr>, period: Expr, module: &str) -> Result<(), FactoryError> {
        if self.recipes.get(name).is_none() {
            let id = id(name, module);
            let inputs = self.parts_from_exprs(inputs, module)?;
            let outputs = self.parts_from_exprs(outputs, module)?;
            let period = self.usize_from_expr(period, module)?;
            let rate = Rate { amount: 1, ticks: period as f64 };
            let recipe = Recipe {
                rate,
                inputs,
                outputs,
            };

            self.recipes.insert(id, Rc::new(RefCell::new(recipe)));

            Ok(())
        } else {
            Err(FactoryError::Exists)
        }
    }

    fn register_stream(&mut self, name: &str, expr: Expr, module: &str) -> Result<(), FactoryError> {
        if self.streams.get(name).is_none() {
            let id = id(name, module);
            let stream = self.stream_from_expr(expr, module)?;

            self.streams.insert(id, stream);

            Ok(())
        } else {
            Err(FactoryError::Exists)
        }
    }

    fn get_module(&mut self, name: &str) -> usize {
        let Some(&id) = self.modules.get(name) else {
            let id = *self.modules.get("__next").unwrap_or(&1);
            self.modules.insert("__next".to_owned(), id + 1);
            return id 
        };

        id
    }

    fn parts_from_exprs(&mut self, exprs: Vec<Expr>, module: &str) -> Result<Vec<RecipePart>, FactoryError> {
        let mut parts = Vec::with_capacity(exprs.len());
        for expr in exprs {
            if let Some(value) = self.process_expr(expr, module)? {
                let recipe_part = match value {
                    Value::RecipePart(recipe_part) => recipe_part,
                    Value::Product(_, product) => RecipePart { product, amount: 1 },
                    _ => return Err(FactoryError::Gleep),
                };

                parts.push(recipe_part);
            } else {
                return Err(FactoryError::Glorp)
            }
        }

        Ok(parts)
    }

    fn usize_from_expr(&mut self, expr: Expr, module: &str) -> Result<usize, FactoryError> {
        if let Some(value) = self.process_expr(expr, module)? {
            match value {
                Value::Int(out) => Ok(out as usize),
                _ => Err(FactoryError::Gleep)
            }
        } else {
            Err(FactoryError::Glorp)
        }
    }

    fn stream_from_expr(&mut self, expr: Expr, module: &str) -> Result<Rc<RefCell<Stream>>, FactoryError> {
        if let Some(value) = self.process_expr(expr, module)? {
            match value {
                Value::Call(..) => {
                    self.parse_call(value)
                },
                Value::MultRecipe(call, mult) => {
                    self.parse_call(*call).inspect(|stream| stream.borrow_mut().mult = mult)
                },
                _ => Err(FactoryError::Gleep)
            }
        } else {
            Err(FactoryError::Glorp)
        }
    }

    fn parse_call(&mut self, call: Value) -> Result<Rc<RefCell<Stream>>, FactoryError> {
        let Value::Call(lhs, rhs) = call else {
            return Err(FactoryError::Gleep);
        };

        let Value::Recipe(_, recipe) = *lhs else {
            return Err(FactoryError::Gleep)
        };

        let mut inputs = Vec::with_capacity(rhs.len());

        for value in rhs {
            match value {
                Value::Stream(id, stream) => inputs.push(stream),
                Value::Call(..) => {
                    inputs.push(self.parse_call(value)?);
                },
                Value::MultRecipe(call, mult) => {
                    inputs.push(self.parse_call(*call).inspect(|stream| stream.borrow_mut().mult = mult)?);
                },
                _ => {
                    println!("{value}");
                    return Err(FactoryError::Gleep)
                },
            }
        }

        let mut buffer = HashMap::new();

        for output in &recipe.borrow().outputs {
            let product = output.product.borrow().clone();
            buffer.insert(product, Buffer::ZERO);
        }

        Ok(Rc::new(RefCell::new(Stream { mult: 1, recipe: recipe.clone(), inputs: inputs.into(), buffer, next: None })))
    }

    pub fn call(&mut self, method: Method, args: Vec<Value>) -> Result<Option<Value>, FactoryError> {
        match (method.object, method.name) {
            (Value::Stream(stream_name, stream), name) => {
                match name.as_ref() {
                    "buffer" => match args.as_slice() {
                        &[Value::Product(_, ref product), Value::Int(buffer)] => {
                            stream.borrow_mut().buffer.get_mut(&product.borrow()).unwrap().max = buffer as usize;
                            Ok(None)
                        },
                        _ => Err(FactoryError::InvalidArguments)
                    },
                    "solve" => match args.as_slice() {
                        &[] => {
                            self.solve(stream.clone());
                            Ok(None)
                        }
                        _ => Err(FactoryError::InvalidArguments)
                    },
                    "log" => {
                        let outputs = if args.len() == 0 {
                            stream.borrow().recipe.borrow().outputs.clone()
                        } else {
                            let mut out = Vec::new();

                            for product in args {
                                if let Value::Product(_, product) = product {
                                    if let Some(recipe_part) = stream.borrow().recipe.borrow().outputs.iter().find(|e| e.product == product) {
                                        out.push(recipe_part.clone());
                                    }
                                }

                            }

                            out
                        };

                        println!("----- {stream_name} -----");
                        for output in outputs {
                            // if the product isnt in the stream something went wrong so a panic is actually desired
                            let rate = stream.borrow().rate_of(&*output.product.borrow()).unwrap();
                            let name = self.product_names.get(&*output.product.borrow()).unwrap();
                            println!("  {} @ {}", name, rate);
                        }

                        Ok(None)
                    }
                    _ => unimplemented!()
                }
            },
            _ => unimplemented!()
        }
    }

    pub fn tick(&mut self, ticks: usize) {
        for stream in self.streams.values() {
            
        }
    }
}

fn id(name: &str, module: &str) -> String {
    format!("{module}::{name}")
}

impl Value {
    pub fn access(&self, rhs: &str) -> Value {
        match self {
            Self::Stream(..) => {
                match rhs {
                    "buffer"
                    | "solve"
                    | "log" => Value::Method(Box::new(Method { object: self.clone(), name: rhs.to_owned() })),
                    _ => unimplemented!(),
                }
            },
            _ => unimplemented!(),
        }
    }
}