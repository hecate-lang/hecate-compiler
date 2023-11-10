use crate::{token::Token, Lexer};
use hecate_util::span::{self, Source::String, Span};
use std::fs;

#[test]
fn one_ident() {
    let input = "test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    // let expected_span = Span::from_source(String, input, 0, 4);
    assert_eq!(spanned.inner, Token::Identifier("test"));
    // assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    // let eof = lexer.next().unwrap();
    // let expected_span = Span::from_source(String, input, 4, 4);
    // assert_eq!(eof.inner, Token::EOF);
    // assert_eq!(eof.loc.as_str(), expected_span.as_str());
}

#[test]
fn whitespaces_before_ident() {
    let input = "    test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 4, 8);
    assert_eq!(spanned.inner, Token::Identifier("test"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 8, 8);
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn whitespace_after_ident() {
    let input = "test      ";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 4);
    assert_eq!(spanned.inner, Token::Identifier("test"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 4, 4);
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn two_idents() {
    let input = "test1 test2";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Identifier("test1"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(span::Source::String, input, 6, 11);
    assert_eq!(spanned.inner, Token::Identifier("test2"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn underscore_ident() {
    let input = "_test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Identifier("_test"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 5, 5);
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn literal() {
    let input = "3452435.3454235";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 15);
    assert_eq!(spanned.inner, Token::Literal("3452435.3454235"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let expected_span = Span::from_source(String, input, 15, 15);
    let spanned = lexer.next().unwrap();
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn literal_and_identifier() {
    let input = "345.4   _test";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 5);
    assert_eq!(spanned.inner, Token::Literal("345.4"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 8, 13);
    assert_eq!(spanned.inner, Token::Identifier("_test"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 13, 13);
    assert_eq!(spanned.inner, Token::EOF);
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn operators() {
    let input = "+= + ! != -> -= && ||";
    let mut lexer = Lexer::new(input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 0, 1);
    assert_eq!(spanned.inner, Token::ControlCharacter('+'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 1, 2);
    assert_eq!(spanned.inner, Token::ControlCharacter('='));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 3, 4);
    assert_eq!(spanned.inner, Token::ControlCharacter('+'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 5, 6);
    assert_eq!(spanned.inner, Token::ControlCharacter('!'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, input, 7, 8);
    assert_eq!(spanned.inner, Token::ControlCharacter('!'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let _expected_span = Span::from_source(String, input, 8, 9);
    assert_eq!(spanned.inner, Token::ControlCharacter('='));
}

#[test]
fn empty_func() {
    let input = fs::read_to_string("../examples/hello_world.hec").expect("failed to read file");
    let sanatized = input.replace('\r', "");
    let mut lexer = Lexer::new(&input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 0, 3);
    assert_eq!(spanned.inner, Token::Identifier("fun"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 4, 9);
    assert_eq!(spanned.inner, Token::Identifier("entry"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 9, 10);
    assert_eq!(spanned.inner, Token::Delimiter('('));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 10, 11);
    assert_eq!(spanned.inner, Token::Delimiter(')'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 12, 13);
    assert_eq!(spanned.inner, Token::Delimiter('{'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 15, 16);
    assert_eq!(spanned.inner, Token::Delimiter('}'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}

#[test]
fn binary_ops() {
    let input =
        fs::read_to_string("../test_cases/accept/binary_ops.hec").expect("failed to read file");
    let sanatized = input.replace('\r', "");
    let mut lexer = Lexer::new(&input);
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 0, 3);
    assert_eq!(spanned.inner, Token::Identifier("fun"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 4, 9);
    assert_eq!(spanned.inner, Token::Identifier("entry"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 9, 10);
    assert_eq!(spanned.inner, Token::Delimiter('('));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 10, 11);
    assert_eq!(spanned.inner, Token::Delimiter(')'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 12, 13);
    assert_eq!(spanned.inner, Token::Delimiter('{'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 18, 29);
    assert_eq!(spanned.inner, Token::Identifier("__print_i32"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 29, 30);
    assert_eq!(spanned.inner, Token::Delimiter('('));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 30, 31);
    assert_eq!(spanned.inner, Token::Literal("1"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 32, 33);
    assert_eq!(spanned.inner, Token::ControlCharacter('+'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 34, 35);
    assert_eq!(spanned.inner, Token::Literal("1"));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 35, 36);
    assert_eq!(spanned.inner, Token::Delimiter(')'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
    let spanned = lexer.next().unwrap();
    let expected_span = Span::from_source(String, &sanatized, 36, 37);
    assert_eq!(spanned.inner, Token::Delimiter(';'));
    assert_eq!(spanned.loc.as_str(), expected_span.as_str());
}
