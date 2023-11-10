#![feature(pattern)]
use crate::token::Token;
use hecate_util::span::{Source, Span, Spanned};
use std::{
    iter::{Iterator, Peekable},
    str::{pattern::Pattern, Chars},
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
    pub fn new(input: &'a str) -> Self {
        let input = input.replace('\r', "");
        let input = Box::new(input);
        let input: &'static str = Box::leak(input);
        let iter = input.chars().peekable();
        Lexer {
            input,
            iter,
            current_pos: 0,
        }
    }

    fn collect_next_char(&mut self) -> char {
        self.current_pos += 1;
        self.iter.next().unwrap()
    }
}

impl<'a> Lexer<'a> {
    fn next(&mut self) -> Spanned<Token<'a>> {
        let control_characters = "(){}<>;:+-*/=!&|";

        // skip all white spaces
        loop {
            match self.iter.peek() {
                None => {
                    return Spanned {
                        inner: Token::EOF,
                        loc: Span::Span {
                            source: Source::String,
                            content: self.input,
                            start: self.current_pos,
                            end: self.current_pos,
                        },
                    };
                }
                Some(ch) => {
                    if ch.is_whitespace() {
                        self.collect_next_char();
                    } else {
                        break;
                    }
                }
            }
        }

        let token_start = self.current_pos;

        let token = match self.iter.peek().unwrap() {
            ident if ident.is_alphabetic() => {
                self.advance_by_identifier();
                Token::Identifier(&self.input[token_start..self.current_pos])
            }
            &'_' => {
                self.advance_by_identifier();
                Token::Identifier(&self.input[token_start..self.current_pos])
            }
            lit if lit.is_numeric() => {
                self.advance_by_literal();
                Token::Literal(&self.input[token_start..self.current_pos])
            }
            control_char if control_char.is_contained_in(control_characters) => {
                Token::ControlCharacter(self.collect_next_char())
            }
            _ => {
                // Should be handled by parser
                self.advance_by_erroneous();
                Token::Undefined(&self.input[token_start..self.current_pos])
            }
        };

        let span = Span::Span {
            source: Source::String,
            content: self.input,
            start: token_start,
            end: self.current_pos,
        };

        Spanned {
            inner: token,
            loc: span,
        }
    }
}

impl<'a> Lexer<'a> {
    fn advance_by_identifier(&mut self) {
        self.collect_next_char();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'_' {
                self.collect_next_char();
            } else {
                break;
            }
        }
    }

    fn advance_by_literal(&mut self) {
        self.collect_next_char();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'.' || ch == &'_' {
                self.collect_next_char();
            } else {
                break;
            }
        }
    }

    fn advance_by_erroneous(&mut self) {
        self.collect_next_char();

        while let Some(ch) = self.iter.peek() {
            if !ch.is_whitespace() {
                self.collect_next_char();
            } else {
                break;
            }
        }
    }
}

impl<'a> Drop for Lexer<'a> {
    fn drop(&mut self) {
        unsafe {
            let _ = Box::from_raw((self.input as *const str).cast_mut());
        }
    }
}
