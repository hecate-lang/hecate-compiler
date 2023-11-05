use std::vec;

use hecate_resolver::{RefId, FullyResolved, ResolvedRef, ResolvedType};
use hecate_util::{span::Spanned, ast::{Module, Function, Expression, Statement, BinaryOp, UnaryOp}};

use crate::ir::{IRModule, IRLabel, IRInstr, IRFunction, IRValue};

pub struct FunctionCtx {
    end: RefId<IRLabel>,
    ret: RefId<IRLabel>,
    instrs: Vec<IRInstr>
}

impl<'a> IRModule<'a> {
    pub fn generate(module: &'a Module<FullyResolved>) -> IRModule<'a> {
        let functions = module.functions.iter()
            .map(|func| Self::build_function(func)).collect();
        IRModule::<'a> { 
            name: *module.name,
            references: &module.data.references, 
            functions
        }
    }
    
    fn build_function(function: &'a Spanned<'a, Function<'a, FullyResolved>>) -> IRFunction {
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
            args: function.args.iter().map(|(v, _t)| **v).collect(),
            name: *function.name,
            instrs: ctx.instrs
        }
    }
}


impl FunctionCtx {
    fn build_expression(&mut self, expression: &Spanned<Expression<FullyResolved>>) -> IRValue {
        match &expression.expr {
            hecate_util::ast::Expr::Binary(op, a, b) => self.build_binary(op, a, b),
            hecate_util::ast::Expr::Unary(op, expr) => self.build_unary(op, expr),
            hecate_util::ast::Expr::Variable(v) => IRValue::Var(**v),
            hecate_util::ast::Expr::FunctionCall(func, args) => self.build_function_call(func, args),
            hecate_util::ast::Expr::If(if_do, otherwise) => self.build_if(if_do, otherwise),
            hecate_util::ast::Expr::Block(stmts, expr) => self.build_block(stmts, expr),
            hecate_util::ast::Expr::Return(expr) => self.build_return(expr),
            hecate_util::ast::Expr::Literal(v) => IRValue::Literal(*v),
        }
    }

    #[allow(clippy::type_complexity)]
    fn build_if(&mut self, if_do: &Vec<(Spanned<Expression<FullyResolved>>, Spanned<Expression<FullyResolved>>)>, otherwise: &Spanned<Expression<FullyResolved>>) -> IRValue {
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
        let r = RefId::new();
        self.instrs.push(IRInstr::Phi(r, phi));
        IRValue::Ref(r)
    }

    fn build_return(&mut self, _expr: &Spanned<Expression<FullyResolved>>) -> IRValue {
        self.instrs.push(IRInstr::Goto(self.ret));
        IRValue::None
    }

    fn build_block(&mut self, stmts: &Vec<Spanned<Statement<FullyResolved>>>, expr: &Option<Box<Spanned<Expression<FullyResolved>>>>) -> IRValue {
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
        // TODO: cleanup of allocated values/drop
        self.instrs.push(IRInstr::Goto(old_ret));

        self.instrs.push(IRInstr::Block(self.end));
        // TODO: cleanup of allocated values/drop
        
        self.end = old_end;
        self.ret = old_ret;
        
        r
    }

    fn build_binary(&mut self, op: &Spanned<BinaryOp>, a: &Spanned<Expression<FullyResolved>>, b: &Spanned<Expression<FullyResolved>>) -> IRValue {
        let v1 = self.build_expression(a);
        let v2 = self.build_expression(b);
        let r = RefId::new();
        self.instrs.push(IRInstr::BinaryOp(r, **op, v1, v2));
        IRValue::Ref(r)
    }

    fn build_unary(&mut self, _op: &Spanned<UnaryOp>, expr: &Spanned<Expression<FullyResolved>>) -> IRValue {
        self.build_expression(expr);
        todo!("unary expr");
    }

    fn build_function_call(&mut self, func: &Spanned<RefId<ResolvedRef>>, args: &Vec<Spanned<Expression<FullyResolved>>>) -> IRValue {
        let mut arg_refs = vec![];
        for arg in args {
            arg_refs.push(self.build_expression(arg));
        }
        let r = RefId::new();
        self.instrs.push(IRInstr::Call(r, **func, arg_refs));
        // TODO: cleanup of allocated values/drop
        IRValue::Ref(r)
    }


    fn build_statement(&mut self, stmt: &Spanned<Statement<FullyResolved>>) {
        match &**stmt {
            Statement::Expression(expr) => { let _ = self.build_expression(expr); },
            Statement::Let(var, ty, expr) => self.build_let_assign(var, ty, expr),
        };
    }

    fn build_let_assign(&mut self, var: &Spanned<RefId<ResolvedRef>>, _ty: &Spanned<RefId<ResolvedType>>, expr: &Spanned<Expression<FullyResolved>>) {
        let v = self.build_expression(expr);
        self.instrs.push(IRInstr::Alloca(**var));
        self.instrs.push(IRInstr::Store(**var, v));
    }
}