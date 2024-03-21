pub const MODULE: usize = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProductKind {
    IronPlate = 0,
    CopperPlate,
    CopperWire,
    GreenChip,
}