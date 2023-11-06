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

struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    current_pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'_> {
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
                        loc: Span::Generated,
                    });
                }
                Some(ch) => {
                    if ch.is_whitespace() || ch == &'\n' {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let token_start = self.current_pos;
        let token_type;

        match self.iter.peek().unwrap() {
            token if token.is_alphabetic() => {
                token_type = Token::Identifier;
                self.collect_identifier();
            }
            &'_' => {
                token_type = Token::Identifier;
                self.collect_identifier();
            }
            token if token.is_numeric() => {
                token_type = Token::Literal;
                self.collect_literal();
            }
            &'!' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(Ne);
                } else {
                    token_type = Token::Operator(Not);
                }
            }
            &'+' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(AddAssign);
                } else {
                    token_type = Token::Operator(Plus);
                }
            }
            &'-' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(SubAssign);
                } else if self.iter.peek().unwrap() == &'>' {
                    self.advance();
                    token_type = Token::ReturnType;
                } else {
                    token_type = Token::Operator(Minus);
                }
            }
            &'*' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(MulAssign);
                } else {
                    token_type = Token::Operator(Mul);
                }
            }
            &'/' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(DivAssign);
                } else {
                    token_type = Token::Operator(Div);
                }
            }
            &'>' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(Ge);
                } else {
                    token_type = Token::Operator(Gt);
                }
            }
            &'<' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(Le);
                } else {
                    token_type = Token::Operator(Lt);
                }
            }
            &'=' => {
                if self.followed_by_equalsign() {
                    token_type = Token::Operator(Eq);
                } else {
                    token_type = Token::Assignment;
                }
            }
            &'&' => {
                if self.iter.peek().unwrap() == &'&' {
                    self.advance();
                    token_type = Token::Operator(And);
                } else {
                    token_type = Token::Undefined;
                    self.collect_erroneous();
                }
            }
            &'|' => {
                if self.iter.peek().unwrap() == &'|' {
                    self.advance();
                    token_type = Token::Operator(Or);
                } else {
                    token_type = Token::Undefined;
                    self.collect_erroneous();
                }
            }
            &'(' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            &')' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            &'{' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            &'}' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            &':' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            &';' => {
                self.advance();
                token_type = Token::Delimiter;
            }
            _ => {
                // Should be handled by parser
                token_type = Token::Undefined;
                self.collect_erroneous();
            }
        }

        let span = Span::Span {
            source: Source::String,
            content: &self.input,
            start: token_start,
            end: self.current_pos,
        };

        let token = Spanned {
            inner: token_type,
            loc: span,
        };

        Option::Some(token)
    }
}

impl<'a> Lexer<'a> {
    fn collect_identifier(&mut self) {
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'_' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn collect_literal(&mut self) {
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'.' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn collect_erroneous(&mut self) {
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
                self.advance();
                return true;
            }
        }
        false
    }
}
