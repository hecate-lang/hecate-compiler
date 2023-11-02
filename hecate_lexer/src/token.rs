use hecate_util::ast::BinaryOp;
use hecate_util::ast::UnaryOp;

pub enum Token {
    Identifier,
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    AssignmentOp,
    Delimiter
}
