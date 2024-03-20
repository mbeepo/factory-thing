   Compiling factory v0.1.0 (/home/bee/Projects/factory)
warning: unused variable: `name`
  --> src/parser.rs:47:28
   |
47 |                     let Ok(name) = lex.next().ok_or(ParseError::UnexpectedEOF(lex.span().into()))? else {
   |                            ^^^^ help: if this is intentional, prefix it with an underscore: `_name`
   |
   = note: `#[warn(unused_variables)]` on by default

warning: `factory` (bin "factory") generated 1 warning (run `cargo fix --bin "factory"` to apply 1 suggestion)
    Finished dev [unoptimized + debuginfo] target(s) in 4.16s
     Running `target/debug/factory`
Green chips will be produced at 100% efficiency (2/1.00s)
[src/main.rs:97:5] factory.items = {
    "copper_plate": Item {
        kind: 1,
        module: 0,
    },
    "copper_wire": Item {
        kind: 2,
        module: 0,
    },
    "green_chip": Item {
        kind: 3,
        module: 0,
    },
    "iron_plate": Item {
        kind: 0,
        module: 0,
    },
    "__next": Item {
        kind: 4,
        module: 0,
    },
}
*wraps you in a big hug* 💓
