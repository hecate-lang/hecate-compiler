use hecate_util::ast::BinaryOp;
use hecate_util::ast::UnaryOp;

// import all enum items to avoid typing and redundancy
use hecate_util::ast::BinaryOp::*;
use hecate_util::ast::UnaryOp::*;
use crate::token::AssignmentOp::*;

enum AssignmentOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

pub enum Token {
    Identifier,
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    AssignmentOp(AssignmentOp),
    Delimiter
}
