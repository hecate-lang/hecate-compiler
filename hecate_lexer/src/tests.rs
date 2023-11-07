use crate::{
    token::{Operator::*, Token},
    Lexer,
};
use hecate_util::span::{self, Source::String, Span};
use std::fs;

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

#[test]
fn operators() {
    let input = "+= + ! != -> -=";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 0, 2);
    assert_eq!(spanned.inner, Token::Operator(AddAssign));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 3, 4);
    assert_eq!(spanned.inner, Token::Operator(Plus));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 5, 6);
    assert_eq!(spanned.inner, Token::Operator(Not));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 7, 9);
    assert_eq!(spanned.inner, Token::Operator(Ne));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 10, 12);
    assert_eq!(spanned.inner, Token::ReturnType);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, input, 13, 15);
    assert_eq!(spanned.inner, Token::Operator(SubAssign));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
}

#[test]
fn empty_func() {
    let input = fs::read_to_string("../examples/hello_world.hec").expect("failed to read file");
    let mut lexer = Lexer::new(&input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 0, 3);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 4, 9);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 9, 10);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 10, 11);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 12, 13);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 15, 16);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
}

#[test]
fn binary_ops() {
    let input = fs::read_to_string("../test_cases/accept/binary_ops.hec").expect("failed to read file");
    let mut lexer = Lexer::new(&input);
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 0, 3);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 4, 9);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 9, 10);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 10, 11);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 12, 13);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 18, 29);
    assert_eq!(spanned.inner, Token::Identifier);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 29, 30);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 30, 31);
    assert_eq!(spanned.inner, Token::Literal);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 32, 33);
    assert_eq!(spanned.inner, Token::Operator(Plus));
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 34, 35);
    assert_eq!(spanned.inner, Token::Literal);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 35, 36);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
    let spanned = lexer.next().unwrap();
    let test_span = Span::from_source(String, &input, 36, 37);
    assert_eq!(spanned.inner, Token::Delimiter);
    assert_eq!(spanned.loc.as_str(), test_span.as_str());
}