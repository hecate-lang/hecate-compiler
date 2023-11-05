use hecate_util::ast::{BinaryOp, UnaryOp};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier,
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    AssignmentOp,
    Delimiter,
}
