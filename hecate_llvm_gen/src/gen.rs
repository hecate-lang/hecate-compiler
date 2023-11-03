use std::vec;

use hecate_resolver::{RefId, FullyResolved, ResolvedRef, ResolvedType};
use hecate_util::{span::Spanned, ast::{Module, Function, Expression, Statement, BinaryOp, UnaryOp}};

use crate::ir::{IRModule, IRLabel, IRInstr, IRFunction, IRValue};

pub struct FunctionCtx<'a> {
    end: RefId<IRLabel<'a>>,
    ret: RefId<IRLabel<'a>>,
    instrs: Vec<IRInstr<'a>>
}

pub fn build_module<'a>(module: &'a Module<FullyResolved>) -> IRModule<'a> {
    let functions = module.functions.iter()
        .map(|func| build_function(func)).collect();
    IRModule::<'a> { 
        name: *module.name,
        references: &module.data.references, 
        functions
    }
}

fn build_function<'a>(function: &'a Spanned<'a, Function<'a, FullyResolved>>) -> IRFunction<'a> {
    let instrs = vec![];
    let entry_block = RefId::new();
    let return_block = RefId::new();

    let mut ctx = FunctionCtx {
        end: return_block,
        ret: return_block,
        instrs
    };

    ctx.instrs.push(IRInstr::Block(entry_block));

    let r = ctx.build_expression(&function.body);

    ctx.instrs.push(IRInstr::Goto(return_block));

    ctx.instrs.push(IRInstr::Block(return_block));
    ctx.instrs.push(IRInstr::Return(r));

    IRFunction { 
        name: *function.name,
        instrs: ctx.instrs
    }
}

impl<'a> FunctionCtx<'a> {
    fn build_expression(&mut self, expression: &Spanned<'a, Expression<'a, FullyResolved>>) -> IRValue {
        match &expression.expr {
            hecate_util::ast::Expr::Binary(op, a, b) => self.build_binary(op, a, b),
            hecate_util::ast::Expr::Unary(op, expr) => self.build_unary(op, expr),
            hecate_util::ast::Expr::Variable(v) => IRValue::Ref(**v),
            hecate_util::ast::Expr::FunctionCall(func, args) => self.build_function_call(func, args),
            hecate_util::ast::Expr::If(if_do, otherwise) => self.build_if(if_do, otherwise),
            hecate_util::ast::Expr::Block(stmts, expr) => self.build_block(stmts, expr),
            hecate_util::ast::Expr::Return(expr) => self.build_return(expr),
            hecate_util::ast::Expr::Literal(v) => IRValue::Literal(*v),
        }
    }

    #[allow(clippy::type_complexity)]
    fn build_if(&mut self, if_do: &Vec<(Spanned<'a, Expression<'a, FullyResolved>>, Spanned<'a, Expression<'a, FullyResolved>>)>, otherwise: &Spanned<'a, Expression<'a, FullyResolved>>) -> IRValue {
        let continue_block = RefId::new();
        let mut phi = vec![];
        let mut last_else_block = RefId::new();
        for (cond, expr) in if_do {
            let c = self.build_expression(cond);
            let then_block = RefId::new();
            let else_block = RefId::new();
            self.instrs.push(IRInstr::Branch(c, then_block, else_block));
            self.instrs.push(IRInstr::Block(then_block));
            let v = self.build_expression(expr);
            phi.push((v, then_block));
            self.instrs.push(IRInstr::Goto(continue_block));
            self.instrs.push(IRInstr::Block(else_block));
            last_else_block = else_block;
        }
        let v = self.build_expression(otherwise);
        phi.push((v, last_else_block));
        self.instrs.push(IRInstr::Goto(continue_block));
        self.instrs.push(IRInstr::Block(continue_block));
        let r = IRValue::Ref(RefId::new());
        self.instrs.push(IRInstr::Phi(r, phi));
        r
    }

    fn build_return(&mut self, expr: &Spanned<'a, Expression<'a, FullyResolved>>) -> IRValue {
        self.instrs.push(IRInstr::Goto(self.ret));
        IRValue::None
    }

    fn build_block(&mut self, stmts: &Vec<Spanned<'a, Statement<'a, FullyResolved>>>, expr: &Option<Box<Spanned<'a, Expression<'a, FullyResolved>>>>) -> IRValue {
        let old_end = self.end;
        let old_ret = self.ret;
        self.ret = RefId::new();
        self.end = RefId::new();

        for stmt in stmts {
            self.build_statement(stmt);
        }
        let r = if let Some(expr) = expr {
            self.build_expression(expr)
        } else {
            IRValue::None
        };
        
        self.instrs.push(IRInstr::Goto(self.end));
        self.instrs.push(IRInstr::Block(self.ret));
        // TODO: ret cleanuo
        self.instrs.push(IRInstr::Goto(old_ret));

        self.instrs.push(IRInstr::Block(self.end));
        // TODO: end cleanuo
        
        self.end = old_end;
        self.ret = old_ret;
        
        r
    }

    fn build_binary(&mut self, op: &Spanned<'a, BinaryOp>, a: &Spanned<'a, Expression<'a, FullyResolved>>, b: &Spanned<'a, Expression<'a, FullyResolved>>) -> IRValue {
        self.build_expression(a);
        self.build_expression(b);
        self.instrs.push(IRInstr::Future);
        IRValue::None
    }

    fn build_unary(&mut self, op: &Spanned<'a, UnaryOp>, expr: &Spanned<'a, Expression<'a, FullyResolved>>) -> IRValue {
        self.build_expression(expr);
        self.instrs.push(IRInstr::Future);
        IRValue::None
    }

    fn build_function_call(&mut self, func: &Spanned<'a, RefId<ResolvedRef>>, args: &Vec<Spanned<'a, Expression<'a, FullyResolved>>>) -> IRValue {
        let mut arg_refs = vec![];
        for arg in args {
            arg_refs.push(self.build_expression(arg));
        }
        let r = IRValue::Ref(RefId::new());
        self.instrs.push(IRInstr::Call(r, **func, arg_refs));
        // TODO: arg cleanup
        r
    }


    fn build_statement(&mut self, stmt: &Spanned<'a, Statement<'a, FullyResolved>>) {
        match &**stmt {
            Statement::Expression(expr) => { let _ = self.build_expression(expr); },
            Statement::Let(var, ty, expr) => self.build_let_assign(var, ty, expr),
        };
    }

    fn build_let_assign(&mut self, var: &Spanned<'a, RefId<ResolvedRef>>, ty: &Spanned<'a, RefId<ResolvedType>>, expr: &Spanned<'a, Expression<'a, FullyResolved>>) {
        self.build_expression(expr);
        self.instrs.push(IRInstr::Future)
    }
}

