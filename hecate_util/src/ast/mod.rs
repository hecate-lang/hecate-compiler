use std::fmt::Debug;

use crate::span::Spanned;

pub trait AstInfo {
    type Type: Debug;
    type Ident: Debug;
    type ModuleData;
}

pub struct Module<'a, I: AstInfo> {
    pub name: Spanned<'a, I::Ident>,
    pub functions: Vec<Spanned<'a, Function<'a, I>>>,
    pub data: I::ModuleData
}

pub type SExpression<'a, I> = Spanned<'a, Expression<'a, I>>;

#[derive(Debug)]
pub struct Expression<'a, I: AstInfo> {
    pub expr: Expr<'a, I>,
    pub ty: I::Type
}

pub type SStatement<'a, I> = Spanned<'a, Statement<'a, I>>;

#[derive(Debug)]
pub enum Statement<'a, I: AstInfo> {
    Expression(Spanned<'a, Expression<'a, I>>),
    Let(Spanned<'a, I::Ident>, Spanned<'a, I::Type>, Spanned<'a, Expression<'a, I>>)
}

pub type Argument<'a, I> = (Spanned<'a, <I as AstInfo>::Type>, Spanned<'a, <I as AstInfo>::Ident>);

pub struct Function<'a, I: AstInfo> {
    pub name: Spanned<'a, I::Ident>,
    pub args: Vec<Argument<'a, I>>,
    pub ret: Spanned<'a, I::Type>,
    pub body: Spanned<'a, Expression<'a, I>>
}

#[derive(Debug)]
pub enum Expr<'a, I: AstInfo> {
    Binary(SBinaryOp<'a>, Box<SExpression<'a, I>>, Box<SExpression<'a, I>>),
    Unary(SUnaryOp<'a>, Box<SExpression<'a, I>>),
    Variable(Spanned<'a, I::Ident>),
    FunctionCall(Spanned<'a, I::Ident>, Vec<SExpression<'a, I>>),
    #[allow(clippy::type_complexity)]
    If(Vec<(Box<SExpression<'a, I>>, Box<SExpression<'a, I>>)>, Box<SExpression<'a, I>>),
    Block(Vec<Box<SStatement<'a, I>>>, Option<Box<SExpression<'a, I>>>),
    Return(Box<SExpression<'a, I>>),
    Literal(i32)
}


pub type SBinaryOp<'a> = Spanned<'a, BinaryOp>;

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne
}

pub type SUnaryOp<'a> = Spanned<'a, UnaryOp>;

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Minus,
    Plus
}