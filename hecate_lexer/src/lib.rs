use crate::token::Token;
use hecate_util::span::{Source, Span, Spanned};
use std::{
    iter::{Iterator, Peekable},
    str::Chars,
};

pub mod token;

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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<'a, Token>;
    fn next(&mut self) -> Option<Self::Item> {
        // skip all white spaces
        while self.iter.peek().unwrap().is_whitespace() {
            self.iter.next();
            self.current_pos += 1;
        }

        let token_start = self.current_pos;

        while self.iter.peek().unwrap().is_alphanumeric() {
            self.iter.next();
            self.current_pos += 1;
        }

        if self.current_pos > token_start {
            let span = Span::Span {
                source: Source::String,
                content: &self.input[token_start..self.current_pos + 1],
                start: token_start,
                end: self.current_pos,
            };
            let token = Spanned {
                inner: Token::Identifier,
                loc: span,
            };
            return Option::Some(token);
        }

        Some(Spanned {
            inner: Token::Identifier,
            loc: Span::Generated,
        })
    }
}
