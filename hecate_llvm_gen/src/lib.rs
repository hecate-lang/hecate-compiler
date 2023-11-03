pub mod ir;
pub mod gen;

use std::{collections::HashMap, path::Path};

use hecate_resolver::{ResolvedType, ResolvedRef, RefId, FullyResolved, ModData};
use hecate_util::{ast::{Module, Function, Expression, Statement, BinaryOp, UnaryOp}, span::{Spanned}};
use inkwell::{context::Context, module::Module as LLVMModule, builder::Builder, types::AnyType, targets::{TargetMachine, Target, TargetTriple, InitializationConfig, RelocMode, CodeModel, FileType, TargetData}, values::{BasicValue, ArrayValue, AnyValue, IntValue, BasicValueEnum, FunctionValue, BasicMetadataValueEnum, AnyValueEnum, InstructionValue}, AddressSpace, basic_block::BasicBlock, attributes::Attribute, InlineAsmDialect, memory_buffer::MemoryBuffer };

pub use inkwell::OptimizationLevel;

struct FunctionCtx<'a> {
    func: RefId<ResolvedRef>,
    end: BasicBlock<'a>,
    ret: BasicBlock<'a>,
    current: BasicBlock<'a>
}

pub struct LLVMIRModuleGen<'a>{
    context: &'a Context,
    module: LLVMModule<'a>,
    builder: Builder<'a>,
    mod_data: &'a ModData<'a>,
    types: HashMap<RefId<ResolvedType>, &'a dyn AnyType<'a>>,
    functions: HashMap<RefId<ResolvedRef>, FunctionValue<'a>>,
    variables: HashMap<RefId<ResolvedRef>, BasicValueEnum<'a>>,
    func: Option<FunctionCtx<'a>>
}

pub fn llvm_context() -> Context {
    Context::create()
}

pub fn generate_llvm<'a>(context: &'a Context, module: &'a Module<'a, FullyResolved>) -> LLVMIRModuleGen<'a> {
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
            func: None
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
        self.builder.build_call(printf, &[ global_string.as_basic_value_enum().into(), int.into() ], "r").unwrap();
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

    fn declare_function(&mut self, function: &Spanned<'a, Function<'a, FullyResolved>>) {
        let t = self.context.void_type();
        let fn_type = t.fn_type(&[], false);
        let func = self.module.add_function(self.mod_data.references[&*function.name], fn_type, None);
        self.functions.insert(*function.name, func);
    }

    fn build_function(&mut self, function: &Spanned<'a, Function<'a, FullyResolved>>) {
        let func = self.functions[&*function.name];
        let entry_block = self.context.append_basic_block(func, "entry");
        let return_block = self.context.append_basic_block(func, "return");
        self.func = Some(FunctionCtx { func: *function.name, end: return_block, ret: return_block, current: entry_block });
        
        self.builder.position_at_end(entry_block);
        let r = self.build_expression(&function.body);
        self.builder.build_unconditional_branch(return_block).unwrap();

        self.builder.position_at_end(return_block);
        self.builder.build_return(None).unwrap();
        self.func = None
    }

    fn build_expression(&mut self, expression: &Spanned<'a, Expression<'a, FullyResolved>>) -> Option<BasicValueEnum<'a>> {
        match &expression.expr {
            hecate_util::ast::Expr::Binary(op, a, b) => self.build_binary(op, a, b),
            hecate_util::ast::Expr::Unary(op, expr) => self.build_unary(op, expr),
            hecate_util::ast::Expr::Variable(v) => Some(self.variables[&**v]),
            hecate_util::ast::Expr::FunctionCall(func, args) => self.build_function_call(func, args),
            hecate_util::ast::Expr::If(if_do, otherwise) => self.build_if(if_do, otherwise),
            hecate_util::ast::Expr::Block(stmts, expr) => self.build_block(stmts, expr),
            hecate_util::ast::Expr::Return(expr) => self.build_return(expr),
            hecate_util::ast::Expr::Literal(v) => Some(self.context.i32_type().const_int(*v as u64, false).as_basic_value_enum()),
        }
    }

    #[allow(clippy::type_complexity)]
    fn build_if(&mut self, if_do: &Vec<(Spanned<'a, Expression<'a, FullyResolved>>, Spanned<'a, Expression<'a, FullyResolved>>)>, otherwise: &Spanned<'a, Expression<'a, FullyResolved>>) -> Option<BasicValueEnum<'a>> {
        let continue_block = self.context.insert_basic_block_after(self.func.as_mut().unwrap().current, "contine");
        for (cond, expr) in if_do {
            let c = self.build_expression(cond);
            let then_block = self.context.insert_basic_block_after(self.func.as_mut().unwrap().current, "then");
            let else_block = self.context.insert_basic_block_after(then_block, "else");
            self.builder.build_conditional_branch(c.unwrap().into_int_value(), then_block, else_block).unwrap();

            self.builder.position_at_end(then_block);
            self.func.as_mut().unwrap().current = then_block;
            self.build_expression(expr);
            self.builder.build_unconditional_branch(continue_block).unwrap();
            
            self.builder.position_at_end(else_block);
        }
        self.build_expression(otherwise);
        self.builder.build_unconditional_branch(continue_block).unwrap();
        self.builder.position_at_end(continue_block);
        self.func.as_mut().unwrap().current = continue_block;
        None
    }

    fn build_return(&mut self, expr: &Spanned<'a, Expression<'a, FullyResolved>>) -> Option<BasicValueEnum<'a>> {
        self.builder.build_unconditional_branch(self.func.as_ref().unwrap().ret).unwrap();
        None
    }

    fn build_block(&mut self, stmts: &Vec<Spanned<'a, Statement<'a, FullyResolved>>>, expr: &Option<Box<Spanned<'a, Expression<'a, FullyResolved>>>>) -> Option<BasicValueEnum<'a>> {
        let (old_end, old_ret) = {
            let f = self.func.as_mut().unwrap();
            let scope_end = self.context.prepend_basic_block(f.end, "block_end");
            let scope_ret = self.context.prepend_basic_block(f.end, "block_ret");
            let old_end = f.end;
            let old_ret = f.ret;
            f.end = scope_end;
            f.ret = scope_ret;
            (old_end, old_ret)
        };

        for stmt in stmts {
            self.build_statement(stmt);
        }
        let r = if let Some(expr) = expr {
            self.build_expression(expr)
        } else {
            None
        };

        {
            let f = self.func.as_mut().unwrap();
            
            self.builder.build_unconditional_branch(f.end).unwrap();

            self.builder.position_at_end(f.ret);
            self.builder.build_unconditional_branch(old_ret).unwrap();
            
            self.builder.position_at_end(f.end);
            
            f.current = f.end;
            f.end = old_end;
            f.ret = old_ret;
        }
        
        r
    }

    fn build_binary(&mut self, op: &Spanned<'a, BinaryOp>, a: &Spanned<'a, Expression<'a, FullyResolved>>, b: &Spanned<'a, Expression<'a, FullyResolved>>) -> Option<BasicValueEnum<'a>> {
        let va = self.build_expression(a).unwrap();
        let vb = self.build_expression(b).unwrap();
        let r = match **op {
            BinaryOp::Add => self.builder.build_int_add(va.into_int_value(), vb.into_int_value(), "").unwrap().as_basic_value_enum(),
            BinaryOp::Sub => self.builder.build_int_sub(va.into_int_value(), vb.into_int_value(), "").unwrap().as_basic_value_enum(),
            BinaryOp::Mul => self.builder.build_int_mul(va.into_int_value(), vb.into_int_value(), "").unwrap().as_basic_value_enum(),
            BinaryOp::Div => self.builder.build_int_signed_div(va.into_int_value(), vb.into_int_value(), "").unwrap().as_basic_value_enum(),
            _ => todo!()
        };
        Some(r)
    }

    fn build_unary(&mut self, op: &Spanned<'a, UnaryOp>, expr: &Spanned<'a, Expression<'a, FullyResolved>>) -> Option<BasicValueEnum<'a>> {
        let v = self.build_expression(expr).unwrap();
        let r = match **op {
            UnaryOp::Not => self.builder.build_not(v.into_int_value(), "").unwrap().as_basic_value_enum(),
            UnaryOp::Minus => self.builder.build_int_neg(v.into_int_value(), "").unwrap().as_basic_value_enum(),
            UnaryOp::Plus => v,
        };
        Some(r)
    }

    fn build_function_call(&mut self, func: &Spanned<'a, RefId<ResolvedRef>>, args: &Vec<Spanned<'a, Expression<'a, FullyResolved>>>) -> Option<BasicValueEnum<'a>> {
        let mut arg_refs = vec![];
        for arg in args {
            arg_refs.push(BasicMetadataValueEnum::from(self.build_expression(arg).unwrap()));
        }
        let r = self.builder.build_call(self.functions[func], &arg_refs[..], "").unwrap();
        None
    }


    fn build_statement(&mut self, stmt: &Spanned<'a, Statement<'a, FullyResolved>>) {
        match &**stmt {
            Statement::Expression(expr) => { let _ = self.build_expression(expr); },
            Statement::Let(var, ty, expr) => self.build_let_assign(var, ty, expr),
        };
    }

    fn build_let_assign(&mut self, var: &Spanned<'a, RefId<ResolvedRef>>, ty: &Spanned<'a, RefId<ResolvedType>>, expr: &Spanned<'a, Expression<'a, FullyResolved>>) {
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

