
use std::{collections::HashMap, path::Path};

use hecate_resolver::{ResolvedType, ResolvedRef, RefId};
use hecate_util::{ast::{Module, Function}, span::{Spanned, Span}};
use inkwell::{context::Context, module::{Module as LLVMModule, Linkage}, builder::Builder, types::{AnyType, BasicType, AsTypeRef, StringRadix}, targets::{TargetMachine, Target, TargetTriple, InitializationConfig, RelocMode, CodeModel, FileType, TargetData}, values::{BasicValue, ArrayValue, AnyValue, IntValue, BasicValueEnum}, AddressSpace };

pub use inkwell::OptimizationLevel;

pub struct LLVMIRModuleGen<'a>{
    context: &'a Context,
    module: LLVMModule<'a>,
    builder: Builder<'a>,
    types: HashMap<RefId<ResolvedType>, &'a dyn AnyType<'a>>,
}

pub fn llvm_context() -> Context {
    Context::create()
}

pub fn generate_llvm<'a>(context: &'a Context, module: &Module<'a, ResolvedType, ResolvedRef>) -> LLVMIRModuleGen<'a> {
    let mut modgen = {
        let module = context.create_module("sum");
        let builder = context.create_builder();
        LLVMIRModuleGen {
            context,
            module,
            builder,
            types: HashMap::new(),
        }
    };
    
    modgen.generate_builtins();

    for function in &module.functions {
        modgen.generate_function(function);
    }

    modgen
}

impl<'a> LLVMIRModuleGen<'a> {
    pub fn generate_builtins(&mut self) {
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
        
        
        let mut fmt_chars: Vec<ArrayValue> = Vec::with_capacity(fmt_str.len()); 
        let i8_type = self.context.i8_type();
        let i8_array_type = i8_type.array_type(fmt_str.len() as u32);
        for b in fmt_str.bytes() {
            let v = i8_array_type.const_zero();
            fmt_chars.push(v);
        }
        
        //let const_str_array = i8_array_type.const_array(&fmt_chars[..]);
        let global_string = self.builder.build_global_string_ptr(fmt_str, "fmt_str").unwrap();
        //self.module.add_global(i8_array_type, None, "fmt_str");
        //global_string.set_initializer(&const_str_array);

        self.builder.build_call(printf, &[ global_string.as_basic_value_enum().into(), int.into() ], "printf").unwrap();
        self.builder.build_return(None).unwrap();

        let t = self.context.void_type();
        let fn_type = t.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        let i = self.context.i32_type().const_int(42, false);
        self.builder.build_call(print_int, &[ BasicValueEnum::IntValue(i).into() ], "print_int").unwrap();
        self.builder.build_return(None).unwrap();
    }

    pub fn generate_function(&mut self, function: &Spanned<'a, Function<'a, ResolvedType, ResolvedRef>>) {
        //let t = self.context.void_type();
        //let fn_type = t.fn_type(&[], false);
        //let function = self.module.add_function(*function.name.name, fn_type, None);
        //let basic_block = self.context.append_basic_block(function, "entry");
        //self.builder.position_at_end(basic_block);
        //self.builder.build_call(printf, &[ fmt_str.as_basic_value_enum().into(), int.into() ], "printf").unwrap();
        //self.builder.build_return(None).unwrap();
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

