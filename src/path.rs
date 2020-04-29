use std::collections::VecDeque;

pub type Path = VecDeque<Seg>;

#[derive(Debug, Clone)]
pub enum Seg {
    Not,
    IfCond,
    IfCons,
    ChainN(i32),
    RecordRef,
    ForeachList,
    ForeachBody,
}
