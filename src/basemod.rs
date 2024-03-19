pub const MODULE: u64 = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ItemKind {
    IronPlate = 0,
    CopperPlate,
    CopperWire,
    GreenChip,
}