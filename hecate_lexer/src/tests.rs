use hecate_util::span::{self, Source::String, Span};

use crate::{token::Token, Lexer};

#[test]
fn one_ident() {
    let input = "test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 4);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let eof = lexer.next().unwrap();
    assert_eq!(eof.inner, Token::EOF);
    assert_eq!(eof.loc.as_str(), None);
}

#[test]
fn whitespaces_before_ident() {
    let input = "    test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 4, 8);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), None);
}

#[test]
fn whitespace_after_ident() {
    let input = "test      ";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 4);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), None);
}

#[test]
fn two_idents() {
    let input = "test1 test2";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(span::Source::String, input, 6, 11);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
}

#[test]
fn underscore_ident() {
    let input = "_test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), None);
}

#[test]
fn literal() {
    let input = "3452435.3454235";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 0, 15);
    assert_eq!(spanned.inner, Token::Literal);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), None);
}

#[test]
fn literal_and_identifier() {
    let input = "345.4   _test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Literal);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 8, 13);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), None);
}
