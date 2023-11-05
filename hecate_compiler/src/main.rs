use std::collections::HashMap;

use hecate_ir_gen::ir::IRModule;
use hecate_llvm_gen::{LLVMModuleCtx, OptimizationLevel};
use hecate_resolver::{ResolvedType, ResolvedRef, RefId, FullyResolved, ModData};
use hecate_util::{ast::{Module, Function, Expression, Expr, Statement}, span::{Spanned, Span}};

fn main() {
    let unit_ty = RefId::<ResolvedType>::new();
    let i32_ty = RefId::<ResolvedType>::new();
    let main = RefId::<ResolvedRef>::new();
    let var_a = RefId::<ResolvedRef>::new();
    let var_b = RefId::<ResolvedRef>::new();
    let var_c = RefId::<ResolvedRef>::new();
    
    let print_int = RefId::<ResolvedRef>::new();

    let module = RefId::<ResolvedRef>::new();

    let mut references = HashMap::new();
    references.insert(main, "main");
    references.insert(var_a, "a");
    references.insert(var_b, "b");
    references.insert(var_c, "c");
    references.insert(print_int, "print_int");
    references.insert(module, "test_module");


    let module = Module::<FullyResolved> {
        data: ModData {
            references
        },
        name: Span::dummied(module),
        functions: vec![
            Span::dummied(Function { 
                name: Span::dummied(main),
                args: vec![],
                ret: Span::dummied(unit_ty),
                body: Span::dummied(
                    Expression { 
                        expr: Expr::Block(vec![
                            Span::dummied(Statement::Let(
                                Span::dummied(var_a), 
                                Span::dummied(i32_ty),
                                Span::dummied(Expression { expr: Expr::Literal(42), ty: i32_ty })
                            )),
                            Span::dummied(Statement::Let(
                                Span::dummied(var_b), 
                                Span::dummied(i32_ty),
                                Span::dummied(Expression { expr: Expr::Literal(69), ty: i32_ty})
                            )),
                            Span::dummied(Statement::Let(
                                Span::dummied(var_c), 
                                Span::dummied(i32_ty),
                                Span::dummied(Expression { expr: Expr::Binary(
                                    Span::dummied(hecate_util::ast::BinaryOp::Add), 
                                    Box::new(Span::dummied(Expression { expr: Expr::Variable(Span::dummied(var_a)), ty: i32_ty })), 
                                    Box::new(Span::dummied(Expression { expr: Expr::Variable(Span::dummied(var_b)), ty: i32_ty }))
                                ), ty: i32_ty})
                            )),
                            Span::dummied(Statement::Expression(Span::dummied(
                                Expression { expr: Expr::If(vec![
                                    (
                                        Span::dummied(Expression { expr: Expr::Literal(1), ty: i32_ty }), 
                                        Span::dummied(
                                            Expression { expr: Expr::FunctionCall(Span::dummied(print_int), vec![
                                                Span::dummied(Expression { expr: Expr::Variable(Span::dummied(var_a)), ty: i32_ty})
                                            ]), ty: unit_ty}
                                        )
                                    )
                                ], Box::new(Span::dummied(
                                    Expression { expr: Expr::FunctionCall(Span::dummied(print_int), vec![
                                        Span::dummied(Expression { expr: Expr::Variable(Span::dummied(var_c)), ty: i32_ty})
                                    ]), ty: unit_ty}
                                ))), ty: unit_ty})
                            ))
                        ], None), 
                        ty: unit_ty
                    }
                )
            })
        ],
    };

    let ir = IRModule::generate(&module);
    std::fs::write("out.ir", ir.to_string()).unwrap();
    let llvm = LLVMModuleCtx::generate(&ir);
    llvm.build("out", OptimizationLevel::None);//
}
