#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Identifier(&'a str),
    ControlChar(char, bool),
    Literal(&'a str),
    EOF,
    Undefined(&'a str),
}
