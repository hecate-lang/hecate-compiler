use hecate_llvm_gen::{generate_llvm, llvm_context, OptimizationLevel};
use hecate_resolver::{ResolvedType, ResolvedRef, RefId};
use hecate_util::{ast::{Module, Function}, span::{Spanned, Span}};

fn main() {
    let unit_ty = RefId::<ResolvedType>::new();
    let main = RefId::<ResolvedRef>::new();

    let module = RefId::<ResolvedRef>::new();

    let ast = Module::<ResolvedType, ResolvedRef> {
        name: Span::dummy().with(ResolvedRef {
            name: Span::dummy().with("test_module"),
            id: module
        }),
        functions: vec![
            Span::dummy().with(Function { 
                name: Span::dummy().with(ResolvedRef {
                    name: Span::dummy().with("main"),
                    id: main
                }),
                args: vec![],
                ret: Span::dummy().with(ResolvedType {
                    id: unit_ty
                })
            })
        ],
    };
    let context = llvm_context();
    let llvm = generate_llvm(&context, &ast);
    llvm.build("out", OptimizationLevel::Default)
}
