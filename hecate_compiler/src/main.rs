

use hecate_ir_gen::ir::IRModule;
use hecate_llvm_gen::{LLVMModuleCtx, OptimizationLevel};
use hecate_parser::types::GenericType;
use hecate_resolver::{infer_types::{infer_types}, resolve_names::{resolve_names, RawSymbols}};
use hecate_util::{ast::{Module, Function, Expression, Expr, Statement}, span::{Span}};

fn main() {
    // let unit_ty = RefId::<ResolvedType>::new();
    // let i32_ty = RefId::<ResolvedType>::new();
    // let main = RefId::<ResolvedRef>::new();
    // let var_a = RefId::<ResolvedRef>::new();
    // let var_b = RefId::<ResolvedRef>::new();
    // let var_c = RefId::<ResolvedRef>::new();
    
    // let print_int = RefId::<ResolvedRef>::new();

    // let module = RefId::<ResolvedRef>::new();

    // let mut references = HashMap::new();
    // references.insert(main, "main");
    // references.insert(var_a, "a");
    // references.insert(var_b, "b");
    // references.insert(var_c, "c");
    // references.insert(print_int, "print_int");
    // references.insert(module, "test_module");


    let module = Module::<RawSymbols> {
        data: (),
        name: Span::dummied("test_module"),
        functions: vec![
            Span::dummied(Function { 
                name: Span::dummied("main"),
                args: vec![],
                ret: Span::dummied(GenericType::Tuple(vec![])),
                body: Span::dummied(
                    Expression {
                        expr: Expr::Block(vec![
                            Span::dummied(Statement::Let(
                                Span::dummied("a"), 
                                Span::dummied(GenericType::Generic("i32", vec![])),
                                Span::dummied(Expression { expr: Expr::Literal(42), ty: GenericType::Generic("i32", vec![]) })
                            )),
                            Span::dummied(Statement::Let(
                                Span::dummied("b"), 
                                Span::dummied(GenericType::Generic("i32", vec![])),
                                Span::dummied(Expression { expr: Expr::Literal(69), ty: GenericType::Generic("i32", vec![]) })
                            )),
                            Span::dummied(Statement::Let(
                                Span::dummied("c"), 
                                Span::dummied(GenericType::Generic("i32", vec![])),
                                Span::dummied(Expression { expr: Expr::Binary(
                                    Span::dummied(hecate_util::ast::BinaryOp::Add), 
                                    Box::new(Span::dummied(Expression { expr: Expr::Variable(Span::dummied("a")), ty: GenericType::Generic("i32", vec![]) })), 
                                    Box::new(Span::dummied(Expression { expr: Expr::Variable(Span::dummied("b")), ty: GenericType::Generic("i32", vec![]) }))
                                ), ty: GenericType::Generic("i32", vec![])})
                            )),
                            Span::dummied(Statement::Expression(Span::dummied(
                                Expression { expr: Expr::If(vec![
                                    (
                                        Span::dummied(Expression { expr: Expr::Literal(1), ty: GenericType::Generic("i32", vec![]) }), 
                                        Span::dummied(
                                            Expression { expr: Expr::FunctionCall(Span::dummied("__print_i32"), vec![
                                                Span::dummied(Expression { expr: Expr::Variable(Span::dummied("a")), ty: GenericType::Generic("i32", vec![])})
                                            ]), ty: GenericType::Tuple(vec![])}
                                        )
                                    )
                                ], Box::new(Span::dummied(
                                    Expression { expr: Expr::FunctionCall(Span::dummied("__print_i32"), vec![
                                        Span::dummied(Expression { expr: Expr::Variable(Span::dummied("c")), ty: GenericType::Generic("i32", vec![])})
                                    ]), ty: GenericType::Tuple(vec![])}
                                ))), ty: GenericType::Tuple(vec![])})
                            ))
                        ], None), 
                        ty: GenericType::Tuple(vec![])
                    }
                )
            })
        ],
    };
    let module = resolve_names(&module).unwrap();
    let module = infer_types(&module).unwrap();
    let ir = IRModule::generate(&module);
    std::fs::write("out.ir", ir.to_string()).unwrap();
    let llvm = LLVMModuleCtx::generate(&ir);
    llvm.build("out", OptimizationLevel::None);
}
