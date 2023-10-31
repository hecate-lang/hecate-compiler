
use std::{collections::HashMap, path::Path};

use hecate_resolver::{ResolvedType, ResolvedRef, RefId, FullyResulved, ModData};
use hecate_util::{ast::{Module, Function, Expression, Statement, BinaryOp}, span::{Spanned, Span}};
use inkwell::{context::Context, module::{Module as LLVMModule, Linkage}, builder::Builder, types::{AnyType, BasicType, AsTypeRef, StringRadix}, targets::{TargetMachine, Target, TargetTriple, InitializationConfig, RelocMode, CodeModel, FileType, TargetData}, values::{BasicValue, ArrayValue, AnyValue, IntValue, BasicValueEnum, FunctionValue, BasicMetadataValueEnum, AnyValueEnum}, AddressSpace, basic_block::BasicBlock };

pub use inkwell::OptimizationLevel;

pub struct LLVMIRModuleGen<'a>{
    context: &'a Context,
    module: LLVMModule<'a>,
    builder: Builder<'a>,
    mod_data: &'a ModData<'a>,
    types: HashMap<RefId<ResolvedType>, &'a dyn AnyType<'a>>,
    functions: HashMap<RefId<ResolvedRef>, FunctionValue<'a>>,
    variables: HashMap<RefId<ResolvedRef>, BasicValueEnum<'a>>,
    func_stack: Vec<RefId<ResolvedRef>>,
    block_stack: Vec<BasicBlock<'a>>
}

pub fn llvm_context() -> Context {
    Context::create()
}

pub fn generate_llvm<'a>(context: &'a Context, module: &'a Module<'a, FullyResulved>) -> LLVMIRModuleGen<'a> {
    let mut modgen = {
        let m = context.create_module(module.data.references[&*module.name]);
        let builder = context.create_builder();
        LLVMIRModuleGen {
            context,
            module: m,
            builder,
            mod_data: &module.data,
            types: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            func_stack: vec![],
            block_stack: vec![]
        }
    };
    
    modgen.generate_builtins();

    for function in &module.functions {
        modgen.declare_function(function);
    }

    for function in &module.functions {
        modgen.build_function(function);
    }
    modgen
}

impl<'a> LLVMIRModuleGen<'a> {
    fn generate_builtins(&mut self) {
        let fmt_str = "debug: %d";

        let printf_t = self.context.i32_type();
        let printf_fn_type = printf_t.fn_type(&[ self.context.i8_type().array_type(fmt_str.len() as u32).ptr_type(AddressSpace::default()).into() ], true);
        let printf = self.module.add_function("printf", printf_fn_type, None);
        
        let print_int_t = self.context.void_type();
        let print_int_fn_type = print_int_t.fn_type(&[ self.context.i32_type().into() ], false);
        let print_int = self.module.add_function("print_int", print_int_fn_type, None);
        let basic_block = self.context.append_basic_block(print_int, "entry");
        let int = print_int.get_nth_param(0).unwrap();
        self.builder.position_at_end(basic_block);
        
        let global_string = self.builder.build_global_string_ptr(fmt_str, "fmt_str").unwrap();
        self.builder.build_call(printf, &[ global_string.as_basic_value_enum().into(), int.into() ], "printf").unwrap();
        self.builder.build_return(None).unwrap();

        let mut id = None;
        for (k, v) in &self.mod_data.references {
            if *v == "print_int" {
                id = Some(k)
            }
        }

        let id = id.unwrap();
        self.functions.insert(*id, print_int);

        //let t = self.context.void_type();
        //let fn_type = t.fn_type(&[], false);
        //let function = self.module.add_function("main", fn_type, None);
        //let basic_block = self.context.append_basic_block(function, "entry");
        //self.builder.position_at_end(basic_block);
        //let i = self.context.i32_type().const_int(42, false);
        //self.builder.build_call(print_int, &[ BasicValueEnum::IntValue(i).into() ], "print_int").unwrap();
        //self.builder.build_return(None).unwrap();
    }

    fn declare_function(&mut self, function: &Spanned<'a, Function<'a, FullyResulved>>) {
        let t = self.context.void_type();
        let fn_type = t.fn_type(&[], false);
        let func = self.module.add_function(self.mod_data.references[&*function.name], fn_type, None);
        self.functions.insert(*function.name, func);
    }

    fn build_function(&mut self, function: &Spanned<'a, Function<'a, FullyResulved>>) {
        let func = self.functions[&*function.name];
        self.func_stack.push(*function.name);
        let entry_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry_block);
        self.block_stack.push(entry_block);

        let r = self.build_expression(&function.body);
        self.builder.build_return(None).unwrap();
        self.block_stack.pop();
        self.func_stack.pop();
    }

    fn build_expression(&mut self, expression: &Spanned<'a, Expression<'a, FullyResulved>>) -> Option<BasicValueEnum<'a>> {
        match &expression.expr {
            hecate_util::ast::Expr::Binary(op, a, b) => self.build_binary(op, a, b),
            hecate_util::ast::Expr::Unary(_, _) => todo!(),
            hecate_util::ast::Expr::Variable(v) => Some(self.variables[&**v]),
            hecate_util::ast::Expr::FunctionCall(func, args) => self.build_function_call(func, args),
            hecate_util::ast::Expr::If(_, _) => todo!(),
            hecate_util::ast::Expr::Block(stmts, expr) => self.build_block(stmts, expr),
            hecate_util::ast::Expr::Return(_) => todo!(),
            hecate_util::ast::Expr::Literal(v) => Some(self.context.i32_type().const_int(*v as u64, false).as_basic_value_enum()),
        }
    }

    fn build_block(&mut self, stmts: &Vec<Box<Spanned<'a, Statement<'a, FullyResulved>>>>, expr: &Option<Box<Spanned<'a, Expression<'a, FullyResulved>>>>) -> Option<BasicValueEnum<'a>> {
        for stmt in stmts {
            self.build_statement(stmt);
        }
        if let Some(expr) = expr {
            self.build_expression(expr)
        } else {
            None
        }
    }

    fn build_binary(&mut self, op: &Spanned<'a, BinaryOp>, a: &Spanned<'a, Expression<'a, FullyResulved>>, b: &Spanned<'a, Expression<'a, FullyResulved>>) -> Option<BasicValueEnum<'a>> {
        let va = self.build_expression(a).unwrap();
        let vb = self.build_expression(b).unwrap();
        let r = match **op {
            BinaryOp::Add => self.builder.build_int_add(va.into_int_value(), vb.into_int_value(), "").unwrap(),
            BinaryOp::Sub => todo!(),
            BinaryOp::Mul => todo!(),
            BinaryOp::Div => todo!(),
            BinaryOp::Gt => todo!(),
            BinaryOp::Lt => todo!(),
            BinaryOp::Ge => todo!(),
            BinaryOp::Le => todo!(),
            BinaryOp::Eq => todo!(),
            BinaryOp::Ne => todo!(),
        };
        Some(r.as_basic_value_enum())
    }

    fn build_function_call(&mut self, func: &Spanned<'a, RefId<ResolvedRef>>, args: &Vec<Spanned<'a, Expression<'a, FullyResulved>>>) -> Option<BasicValueEnum<'a>> {
        let mut arg_refs = vec![];
        for arg in args {
            arg_refs.push(BasicMetadataValueEnum::from(self.build_expression(arg).unwrap()));
        }
        let r = self.builder.build_call(self.functions[func], &arg_refs[..], "").unwrap();
        None
    }


    fn build_statement(&mut self, stmt: &Spanned<'a, Statement<'a, FullyResulved>>) {
        match &**stmt {
            Statement::Expression(expr) => { let _ = self.build_expression(expr); },
            Statement::Let(var, ty, expr) => self.build_let_assign(var, ty, expr),
        };
    }

    fn build_let_assign(&mut self, var: &Spanned<'a, RefId<ResolvedRef>>, ty: &Spanned<'a, RefId<ResolvedType>>, expr: &Spanned<'a, Expression<'a, FullyResulved>>) {
        let v = self.build_expression(expr).unwrap();
        self.variables.insert(**var, v);
    }

    pub fn build(&self, path: impl AsRef<Path>, opt: OptimizationLevel) {
        // TODO: find out why and how and what
        Target::initialize_all(&InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: true,
            info: true,
            machine_code: true,
        });
        let host_triple = TargetMachine::get_default_triple();
        let host_cpu = TargetMachine::get_host_cpu_name().to_string();
        let host_features = TargetMachine::get_host_cpu_features().to_string();
        let target = Target::from_triple(&host_triple).expect("AAAAAA");
        let target_machine = target.create_target_machine(
            &host_triple,
            host_cpu.as_str(),
            host_features.as_str(),
            opt,
            RelocMode::Default,
            CodeModel::Default
        ).unwrap();
        let mut obj_path = path.as_ref().to_path_buf();
        obj_path.set_extension("o");
        let mut exe_path = path.as_ref().to_path_buf();
        #[cfg(windows)]
        exe_path.set_extension("exe");
        let mut assembly = path.as_ref().to_path_buf();
        assembly.set_extension("asm");
        let mut llvm_ir = path.as_ref().to_path_buf();
        llvm_ir.set_extension("ll");
        self.module.print_to_file(llvm_ir.as_path()).unwrap();
        
        target_machine.write_to_file(&self.module, FileType::Assembly, assembly.as_path()).unwrap();
        target_machine.write_to_file(&self.module, FileType::Object, obj_path.as_path()).unwrap();
        std::process::Command::new("gcc").arg(obj_path.to_str().unwrap()).arg("-o").arg(exe_path.to_str().unwrap()).output().unwrap();
    }
}

