use std::vec;

use hecate_resolver::{RefId, FullyResolved, ResolvedRef, ResolvedType};
use hecate_util::{span::Spanned, ast::{Module, Function, Expression, Statement, BinaryOp, UnaryOp, Expr}};

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
            Expr::Binary(op, a, b) => self.build_binary(op, a, b),
            Expr::Unary(op, expr) => self.build_unary(op, expr),
            Expr::Variable(v) => IRValue::Var(**v),
            Expr::FunctionCall(func, args) => self.build_function_call(func, args),
            Expr::If(if_do, otherwise) => self.build_if(if_do, otherwise),
            Expr::Block(stmts, expr) => self.build_block(stmts, expr),
            Expr::Return(expr) => self.build_return(expr),
            Expr::Literal(v) => IRValue::Literal(*v),
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

#[cfg(test)]
pub mod tests {
    use hecate_resolver::{RefId, ResolvedType, FullyResolved};
    use hecate_util::{ast::{Expr, BinaryOp, Expression, UnaryOp}, span::{Span, Spanned}};

    use crate::{gen::FunctionCtx, ir::{IRInstr, IRValue}};

    fn literal<'a>(i32_ty: RefId<ResolvedType>, v: i32) -> Spanned<'a, Expression<'a, FullyResolved<'a>>> {
        Span::dummied(Expression {
            expr: Expr::Literal(v),
            ty: i32_ty,
        })
    }

    macro_rules! test_expression {
        ($end: expr, $ret: expr, $r: ident: $expr: expr => $ir: expr) => { {
            let mut ctx = FunctionCtx {
                end: $end,
                ret: $ret,
                instrs: vec![]
            };
            let r = ctx.build_expression(&$expr);
            let $r = match r {
                IRValue::Ref(r) | IRValue::Var(r)=> r,
                _ => unreachable!()
            };
            let target: Vec<IRInstr> = $ir;

            let res_str = format!("{:?}", ctx.instrs);
            let target_str = format!("{:?}", target);
            assert_eq!(res_str, target_str);
        } };
    }

    macro_rules! get_label {
        ($ir_instr: expr) => {
            match $ir_instr {
                IRInstr::Block(l) => l,
                _ => panic!("invalid ir_instr: expected block, found {:?}", $ir_instr)
            }
        };
    }

    #[test]
    fn binary_to_ir() {
        let i32_ty = RefId::new();

        test_expression! {
            RefId::new(), RefId::new(), r_ref: 
            Span::dummied(Expression {
                expr: Expr::Binary(
                    Span::dummied(BinaryOp::Add), 
                    Box::new(literal(i32_ty, 3)),
                    Box::new(literal(i32_ty, 5))
                ),
                ty: i32_ty,
            }) =>
            vec![
                IRInstr::BinaryOp(r_ref, BinaryOp::Add, IRValue::Literal(3), IRValue::Literal(5))
            ]
        }
    }

    #[test]
    #[should_panic(expected = "not yet implemented: unary expr")]
    fn unary_to_ir() {
        let i32_ty = RefId::new();

        test_expression! {
            RefId::new(), RefId::new(), _r_ref: 
            Span::dummied(Expression {
                expr: Expr::Unary(
                    Span::dummied(UnaryOp::Minus), 
                    Box::new(literal(i32_ty, 3)),
                ),
                ty: i32_ty,
            }) =>
            vec![]
        }
    }

    #[test]
    fn variable_to_ir() {
        let i32_ty = RefId::new();
        let var = RefId::new();

        let expr = Span::dummied(Expression {
            expr: Expr::Variable(Span::dummied(var)),
            ty: i32_ty,
        });

        let mut ctx = FunctionCtx {
            end: RefId::new(),
            ret: RefId::new(),
            instrs: vec![]
        };
        let r = ctx.build_expression(&expr);
        
        let r_ref = match r {
            IRValue::Ref(r) | IRValue::Var(r)=> r,
            _ => unreachable!()
        };

        assert_eq!(var, r_ref)
    }

    #[test]
    fn function_call_to_ir() {
        let i32_ty = RefId::new();
        let unit_ty = RefId::new();
        let func = RefId::new();

        test_expression! {
            RefId::new(), RefId::new(), r_ref: 
            Span::dummied(Expression {
                expr: Expr::FunctionCall(
                    Span::dummied(func), 
                    vec![literal(i32_ty, 55)]
                ),
                ty: unit_ty,
            }) =>
            vec![
                IRInstr::Call(r_ref, func, vec![IRValue::Literal(55)])
            ]
        }
    }

    #[test]
    fn if_to_ir() {
        let bool_ty = RefId::new();
        let i32_ty = RefId::new();

        let expr = Span::dummied(Expression {
            expr: Expr::If(
                vec![(literal(bool_ty, 1), literal(i32_ty, 0))], 
                Box::new(literal(i32_ty, 99))
            ),
            ty: i32_ty,
        });

        let mut ctx = FunctionCtx {
            end: RefId::new(),
            ret: RefId::new(),
            instrs: vec![]
        };
        let r = ctx.build_expression(&expr);

        let then_block = get_label!(ctx.instrs[1]);
        let else_block = get_label!(ctx.instrs[3]);
        let continue_block = get_label!(ctx.instrs[5]);

        let r_ref = match r {
            IRValue::Ref(r) | IRValue::Var(r)=> r,
            _ => unreachable!()
        };

        let target = vec![
            IRInstr::Branch(IRValue::Literal(1), then_block, else_block),
            IRInstr::Block(then_block),
            IRInstr::Goto(continue_block),
            IRInstr::Block(else_block),
            IRInstr::Goto(continue_block),
            IRInstr::Block(continue_block),
            IRInstr::Phi(r_ref, vec![(IRValue::Literal(0), then_block), (IRValue::Literal(99), else_block)])
        ];
    
        let res_str = format!("{:?}", ctx.instrs);
        let target_str = format!("{:?}", target);
        assert_eq!(res_str, target_str);
    }

    #[test]
    fn block_to_ir() {
        let unit_ty = RefId::new();

        let expr = Span::dummied(Expression {
            expr: Expr::Block(vec![], None),
            ty: unit_ty,
        });

        let old_ret = RefId::new();

        let mut ctx = FunctionCtx {
            end: RefId::new(),
            ret: old_ret,
            instrs: vec![]
        };
        let r = ctx.build_expression(&expr);

        let ret_block = get_label!(ctx.instrs[1]);
        let end_block = get_label!(ctx.instrs[3]);

        let target = vec![
            IRInstr::Goto(end_block),
            IRInstr::Block(ret_block),
            IRInstr::Goto(old_ret),
            IRInstr::Block(end_block),
        ];
    
        let res_str = format!("{:?}", ctx.instrs);
        let target_str = format!("{:?}", target);
        assert_eq!(res_str, target_str);
    }
}