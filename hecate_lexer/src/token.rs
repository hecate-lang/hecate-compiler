use hecate_util::ast::{BinaryOp, UnaryOp};

pub enum Token {
    Identifier,
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    AssignmentOp,
    Delimiter,
}
