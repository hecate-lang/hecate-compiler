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
                    if ch.is_whitespace() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let token_start;

        match self.iter.peek() {
            token if token.unwrap().is_alphabetic() => {
                token_start = self.collect_identifier();
            }
            Some(&'_') => {
                token_start = self.collect_identifier();
            }
            Some(_) => {
                token_start = self.current_pos;
            }
            None => {
                return Option::Some(Spanned {
                    inner: Token::EOF,
                    loc: Span::Generated,
                });
            }
        }

        let span = Span::Span {
            source: Source::String,
            content: &self.input,
            start: token_start,
            end: self.current_pos,
        };
        let token = Spanned {
            inner: Token::Identifier,
            loc: span,
        };
        Option::Some(token)
    }
}

impl<'a> Lexer<'a> {
    fn collect_identifier(&mut self) -> usize {
        // as the function is only called when the next character is matched against
        // is_alphabetic() or the underscore char '_' we can move the current_pos of the lexer
        let token_start = self.current_pos;
        self.advance();

        while let Some(ch) = self.iter.peek() {
            if ch.is_alphanumeric() || ch == &'_' {
                self.advance();
            } else {
                return token_start;
            }
        }

        token_start
    }

    fn advance(&mut self) {
        self.iter.next();
        self.current_pos += 1;
    }
}
