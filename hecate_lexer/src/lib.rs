use crate::token::Operator::*;
use crate::token::Token;
use hecate_util::span::{Source, Span, Spanned};
use std::{
    iter::{Iterator, Peekable},
    str::Chars,
};

pub mod token;

#[cfg(test)]
mod tests;

pub struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    current_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'_> {
        let iter = input.chars().peekable();
        Lexer {
            input,
            iter,
            current_pos: 0,
        }
    }

    fn advance(&mut self) {
        self.iter.next();
        self.current_pos += 1;
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<'a, Token>;
    fn next(&mut self) -> Option<Self::Item> {
        // skip all white spaces
        loop {
            match self.iter.peek() {
                None => {
                    return Option::Some(Spanned {
                        inner: Token::EOF,
                        loc: Span::Span {
                            source: Source::String,
                            content: self.input,
                            start: self.current_pos,
                            end: self.current_pos,
                        },
                    });
                }
                Some(ch) => {
                    if ch.is_whitespace() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let token_start = self.current_pos;

        let token = match self.iter.peek().unwrap() {
            token if token.is_alphabetic() => {
                self.advance_by_identifier();
                Token::Identifier
            }
            &'_' => {
                self.advance_by_identifier();
                Token::Identifier
            }
            token if token.is_numeric() => {
                self.advance_by_literal();
                Token::Literal
            }
            &'!' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(Ne)
                } else {
                    Token::Operator(Not)
                }
            }
            &'+' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(AddAssign)
                } else {
                    Token::Operator(Plus)
                }
            }
            &'-' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(SubAssign)
                } else if self.iter.peek().unwrap() == &'>' {
                    self.advance();
                    Token::ReturnType
                } else {
                    Token::Operator(Minus)
                }
            }
            &'*' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(MulAssign)
                } else {
                    Token::Operator(Mul)
                }
            }
            &'/' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(DivAssign)
                } else {
                    Token::Operator(Div)
                }
            }
            &'>' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(Ge)
                } else {
                    Token::Operator(Gt)
                }
            }
            &'<' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(Le)
                } else {
                    Token::Operator(Lt)
                }
            }
            &'=' => {
                if self.followed_by_equalsign() {
                    self.advance();
                    Token::Operator(Eq)
                } else {
                    Token::Assignment
                }
            }
            &'&' => {
                self.advance();
                if self.iter.peek().unwrap() == &'&' {
                    self.advance();
                    Token::Operator(And)
                } else {
                    self.advance_by_erroneous();
                    Token::Undefined
                }
            }
            &'|' => {
                self.advance();
                if self.iter.peek().unwrap() == &'|' {
                    self.advance();
                    Token::Operator(Or)
                } else {
                    self.advance_by_erroneous();
                    Token::Undefined
                }
            }
            &'(' | &')' | &'{' | &'}' | &':' | &';' => {
                self.advance();
                Token::Delimiter
            }
            _ => {
                // Should be handled by parser
                self.advance_by_erroneous();
                Token::Undefined
            }
        };

        let span = Span::Span {
            source: Source::String,
            content: self.input,
            start: token_start,
            end: self.current_pos,
        };

        let token = Spanned {
            inner: token,
            loc: span,
        };

        Option::Some(token)
    }
}

impl<'a> Lexer<'a> {
    fn advance_by_identifier(&mut self) {
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'_' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn advance_by_literal(&mut self) {
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'.' || ch == &'_' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn advance_by_erroneous(&mut self) {
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_whitespace() {
                break;
            } else {
                self.advance();
            }
        }
    }

    fn followed_by_equalsign(&mut self) -> bool {
        self.advance();
        if let Some(ch) = self.iter.peek() {
            if ch == &'=' {
                return true;
            }
        }
        false
    }
}
