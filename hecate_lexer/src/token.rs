#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Identifier(&'a str),
    ControlCharacter(char),
    Literal(&'a str),
    Delimiter(char),
    EOF,
    Undefined(&'a str),
}
