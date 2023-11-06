#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier,
    Operator(Operator),
    Assignment,
    Literal,
    Delimiter,
    EOF,
    Undefined,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Not,
    Plus,
    Minus,
    Mul,
    Div,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}
