use crate::span::Spanned;

pub struct Module<'a, Type, Ident> {
    pub name: Spanned<'a, Ident>,
    pub functions: Vec<Spanned<'a, Function<'a, Type, Ident>>>
}

pub type SExpression<'a, Type, VarRef> = Spanned<'a, Expression<'a, Type, VarRef>>;

#[derive(Debug)]
pub struct Expression<'a, Type, Ident> {
    pub expr: Expr<'a, Type, Ident>,
    pub ty: Type
}

pub type SStatement<'a, Type, VarRef> = Spanned<'a, Statement<'a, Type, VarRef>>;

#[derive(Debug)]
pub enum Statement<'a, Type, VarRef> {
    Expression(Expression<'a, Type, VarRef>),
    Let()
}

pub struct Function<'a, Type, VarRef> {
    pub name: Spanned<'a, VarRef>,
    pub args: Vec<(Spanned<'a, Type>, Spanned<'a, VarRef>)>,
    pub ret: Spanned<'a, Type>
}

#[derive(Debug)]
pub enum Expr<'a, Type, Ident> {
    Binary(SBinaryOp<'a>, Box<SExpression<'a, Type, Ident>>, Box<SExpression<'a, Type, Ident>>),
    Unary(SUnaryOp<'a>, Box<SExpression<'a, Type, Ident>>),
    Variable(Spanned<'a, Ident>),
    FunctionCall(Spanned<'a, Ident>, Vec<SExpression<'a, Type, Ident>>),
    #[allow(clippy::type_complexity)]
    If(Vec<(Box<SExpression<'a, Type, Ident>>, Box<SExpression<'a, Type, Ident>>)>, Box<SExpression<'a, Type, Ident>>),
    Block(Vec<Box<SStatement<'a, Type, Ident>>>, Option<Box<SExpression<'a, Type, Ident>>>),
    Return(Box<SExpression<'a, Type, Ident>>)
}


pub type SBinaryOp<'a> = Spanned<'a, BinaryOp>;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Minus,
    Plus
}