use std::{collections::HashMap, path::Path, ffi::CStr};

use hecate_ir_gen::ir::{IRModule, IRFunction, IRLabel, IRValue, IRInstr};
use hecate_resolver::{RefId, ResolvedRef};

use hecate_util::ast::BinaryOp;
use llvm_sys::{core::*, *, prelude::LLVMBool, bit_writer::LLVMWriteBitcodeToFile, target_machine::*, target::*};

macro_rules! c_str {
    () => { 
        #[allow(unused_unsafe)]
        unsafe { std::ffi::CStr::from_ptr("\0".as_ptr() as *const i8) } 
    };
    ($s:literal) => (
        #[allow(unused_unsafe)]
        unsafe { std::ffi::CStr::from_ptr(concat!($s, "\0").as_ptr() as *const i8) }
    );
    ($s:expr) => (
        #[allow(unused_unsafe)]
        unsafe { std::ffi::CStr::from_ptr(($s.to_string() + "\0").as_ptr() as *const i8) }
    );
}

macro_rules! c_str_ptr {
    () => (
        c_str!().as_ptr()
    );
    ($s:literal) => (
        c_str!($s).as_ptr()
    );
    ($s:expr) => (
        c_str!($s).as_ptr()
    );
}

#[repr(u32)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OptimizationLevel {
    None = 0,
    Less = 1,
    Default = 2,
    Aggressive = 3,
}

pub struct LLVMModuleCtx {
    functions: HashMap<RefId<ResolvedRef>, FunctionBuilder>,
    llvm_functions: HashMap<RefId<ResolvedRef>, LLVMFunction>,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
}

impl LLVMModuleCtx {
    pub fn generate(ir_module: &IRModule) -> Self {
        let module = unsafe { LLVMModuleCreateWithName(c_str_ptr!(ir_module.references[&ir_module.name])) };
        let builder = unsafe { core::LLVMCreateBuilder() };

        let mut ctx = Self { 
            functions: HashMap::new(),
            llvm_functions: HashMap::new(),
            module,
            builder
        };

        ctx.generate_builtins(ir_module.references);

        for func in &ir_module.functions {
            ctx.register_function(func, ir_module.references);
        }

        for func in &ir_module.functions {
            ctx.build_function(func)
        }

        ctx
    }

    // temporary function to be replaced once apporpiate types and linking to c functions is implemented
    fn generate_builtins(&mut self, references: &HashMap<RefId<ResolvedRef>, &str>) {
        let printf_t = unsafe { LLVMInt32Type() };
        let printf_fn_type = unsafe { LLVMFunctionType(printf_t, [ LLVMPointerType(LLVMArrayType(LLVMInt8Type(), 0), 0)].as_mut_ptr(), 1, true as LLVMBool)};
        let printf = unsafe { LLVMAddFunction(self.module, c_str_ptr!("printf"), printf_fn_type) };
        
        let print_int_t = unsafe { LLVMVoidType() };
        let print_int_fn_type = unsafe { LLVMFunctionType(print_int_t, [ LLVMInt32Type() ].as_mut_ptr(), 1, true as LLVMBool)};
        let print_int = unsafe { LLVMAddFunction(self.module, c_str_ptr!("print_int"), print_int_fn_type) };
        let entry = unsafe { LLVMAppendBasicBlock(print_int, c_str_ptr!("entry")) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, entry); }
        let value = unsafe { LLVMGetParam(print_int, 0) };
        let fmt_str = unsafe { LLVMBuildGlobalStringPtr(self.builder, c_str_ptr!("debug: %d"), c_str_ptr!("print_int_fmt_str")) };
        unsafe { LLVMBuildCall2(self.builder, printf_fn_type, printf, [ fmt_str, value ].as_mut_ptr(), 2, c_str_ptr!()); }
        unsafe { LLVMBuildRetVoid(self.builder); }
        let mut id = None;
        for (k, v) in references {
            if *v == "print_int" {
                id = Some(k)
            }
        }
        let id = id.expect("print_int not defined in resolved references");
        self.llvm_functions.insert(*id, LLVMFunction { 
            llvm_func: print_int, 
            llvm_ty: print_int_fn_type 
        });
    }

    fn register_function(&mut self, func: &IRFunction, references: &HashMap<RefId<ResolvedRef>, &str>) {
        let llvm_ty = unsafe {
            core::LLVMFunctionType(LLVMVoidType(), &mut [].as_mut_ptr(), 0, false as LLVMBool)
        };
        let llvm_func = unsafe { LLVMAddFunction(self.module, c_str_ptr!(references[&func.name]), llvm_ty) };

        self.functions.insert(func.name, FunctionBuilder {
            blocks: HashMap::default(),
            values: HashMap::default()
        });

        self.llvm_functions.insert(func.name, LLVMFunction { 
            llvm_func, 
            llvm_ty
        });
    }

    fn build_function(&mut self, func: &IRFunction) {
        let function = self.functions.get_mut(&func.name).unwrap();
        let llvm_func = self.llvm_functions[&func.name].llvm_func;
        
        // register labels
        for instr in &func.instrs {
            if let IRInstr::Block(b) = instr {
                unsafe {
                    let block = core::LLVMAppendBasicBlock(llvm_func, c_str_ptr!());
                    function.blocks.insert(*b, block);
                }
            }
        }

        for instr in &func.instrs {
            match instr {
                IRInstr::Block(b) => function.build_block(self.builder, b),
                IRInstr::Goto(b) => function.build_goto(self.builder, b),
                IRInstr::Phi(_, _) => (),
                IRInstr::Branch(v, then_block, else_block) => function.build_branch(self.builder, v, then_block, else_block),
                IRInstr::Call(r, f, args) => function.build_call(self.builder, r, f, args,  &self.llvm_functions[f]),
                IRInstr::Return(v) => function.build_return(self.builder, v),
                IRInstr::Alloca(r) => function.build_alloca(self.builder, r),
                IRInstr::Store(p, v) => function.build_store(self.builder, p, v),
                IRInstr::Load(r, p) => function.build_load(self.builder, r, p),
                IRInstr::BinaryOp(r, op, a, b) => function.build_binary_op(self.builder, r, op, a, b),
            }
        }
    }

    pub fn build(&self, path: impl AsRef<Path>, opt: OptimizationLevel) {
        let mut obj_path = path.as_ref().to_path_buf();
        obj_path.set_extension("o");
        let mut exe_path = path.as_ref().to_path_buf();
        #[cfg(windows)]
        exe_path.set_extension("exe");
        let mut assembly = path.as_ref().to_path_buf();
        assembly.set_extension("asm");
        let mut llvm_ir = path.as_ref().to_path_buf();
        llvm_ir.set_extension("ll");
        let mut llvm_bc = path.as_ref().to_path_buf();
        llvm_bc.set_extension("bc");
        
        unsafe {
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();
            LLVM_InitializeAllDisassemblers();
            LLVM_InitializeAllTargetMCs();

            LLVMPrintModuleToFile(self.module, c_str_ptr!(llvm_ir.to_str().unwrap()), std::ptr::null_mut());
            LLVMWriteBitcodeToFile(self.module, c_str_ptr!(llvm_bc.to_str().unwrap()));

            let triple = LLVMGetDefaultTargetTriple();
            let features = LLVMGetHostCPUFeatures();
            let cpu = LLVMGetHostCPUName();
            let err_string = std::ptr::null_mut();
            let mut target = std::ptr::null_mut();
            let target_ret = LLVMGetTargetFromTriple(triple, &mut target, err_string);
            if target_ret != 0 {
                panic!("LLVM Error while getting target from triple {}: {}", 
                    CStr::from_ptr(triple).to_string_lossy(), 
                    CStr::from_ptr(*err_string).to_string_lossy()
                );
            }
            let target_machine = LLVMCreateTargetMachine(
                target,
                triple,
                cpu,
                features,
                match opt {
                    OptimizationLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                    OptimizationLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
                    OptimizationLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                    OptimizationLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
                },
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );
            if target_machine.is_null() {
                panic!("Could not create LLVM target machine");
            }

            let err_string = std::ptr::null_mut();
            let return_code = LLVMTargetMachineEmitToFile(target_machine, self.module, c_str_ptr!(obj_path.to_str().unwrap()) as *mut _, LLVMCodeGenFileType::LLVMObjectFile, err_string);
            if return_code != 0 {
                panic!("LLVM Error while compiling to object file: {}", CStr::from_ptr(*err_string).to_string_lossy());
            }

            let err_string = std::ptr::null_mut();
            let return_code = LLVMTargetMachineEmitToFile(target_machine, self.module, c_str_ptr!(assembly.to_str().unwrap()) as *mut _, LLVMCodeGenFileType::LLVMAssemblyFile, err_string);
            if return_code != 0 {
                panic!("LLVM Error while compiling to object file: {}", CStr::from_ptr(*err_string).to_string_lossy());
            }

            LLVMDisposeTargetMachine(target_machine);
        }
        // TODO: replace with clang-sys (static linking) so no runtime deps are needed
        let r = std::process::Command::new("gcc").arg(obj_path.to_str().unwrap()).arg("-o").arg(exe_path.to_str().unwrap()).arg("-fpie").output().expect("failed to compile with gcc");
        println!("{}", String::from_utf8_lossy(&r.stdout[..]));
        println!("{}", String::from_utf8_lossy(&r.stderr[..]));
    }
}

impl Drop for LLVMModuleCtx {
    fn drop(&mut self) {
        #[cfg(debug_assertions)]
        unsafe {
            if std::thread::panicking() {
                LLVMPrintModuleToFile(self.module, c_str_ptr!("dump_on_err.ll"), std::ptr::null_mut());
            }
        }
        unsafe {
            LLVMDisposeModule(self.module);
            LLVMDisposeBuilder(self.builder);
        }
    }
}

struct LLVMFunction {
    llvm_func: *mut LLVMValue,
    llvm_ty: *mut LLVMType,
}

struct FunctionBuilder {
    blocks: HashMap<RefId<IRLabel>, *mut LLVMBasicBlock>,
    values: HashMap<RefId<ResolvedRef>, *mut LLVMValue>
}

impl FunctionBuilder {
    fn llvm_val(&mut self, builder: *mut LLVMBuilder, v: &IRValue) -> *mut LLVMValue {
        unsafe {
            match v {
                IRValue::Ref(r) => self.values[r],
                IRValue::Var(v) => LLVMBuildLoad2(builder, LLVMInt32Type(), self.values[v], c_str_ptr!()),
                IRValue::Literal(v) => LLVMConstInt(LLVMInt32Type(), *v as u64, true as LLVMBool),
                IRValue::None => panic!("tried to get llvm value of a none value"),
            }
        }
    }

    fn build_binary_op(&mut self, builder: *mut LLVMBuilder, r: &RefId<ResolvedRef>, op: &BinaryOp, a: &IRValue, b: &IRValue) {
        let a = self.llvm_val(builder, a);
        let b = self.llvm_val(builder, b);
        let res = unsafe {
            match op {
                BinaryOp::Add => LLVMBuildAdd(builder, a, b, c_str_ptr!()),
                BinaryOp::Sub => LLVMBuildSub(builder, a, b, c_str_ptr!()),
                BinaryOp::Mul => LLVMBuildMul(builder, a, b, c_str_ptr!()),
                BinaryOp::Div => LLVMBuildSDiv(builder, a, b, c_str_ptr!()),
                BinaryOp::Gt => todo!(),
                BinaryOp::Lt => todo!(),
                BinaryOp::Ge => todo!(),
                BinaryOp::Le => todo!(),
                BinaryOp::Eq => todo!(),
                BinaryOp::Ne => todo!(),
            }
        };
        self.values.insert(*r, res);
    }

    fn build_return(&mut self, builder: *mut LLVMBuilder, v: &IRValue) {
        unsafe {
            match v {
                IRValue::None =>  LLVMBuildRetVoid(builder),
                _ => LLVMBuildRet(builder, self.llvm_val(builder, v))
            };
        }
    }

    fn build_call(&mut self, builder: *mut LLVMBuilder, r: &RefId<ResolvedRef>, _f: &RefId<ResolvedRef>, args: &Vec<IRValue>, called: &LLVMFunction) {
        unsafe {
            let res = LLVMBuildCall2(builder, called.llvm_ty, called.llvm_func, args.iter()
                .map(|v| self.llvm_val(builder, v)).collect::<Vec<_>>().as_mut_ptr(), args.len() as u32,
                 c_str_ptr!());
            self.values.insert(*r, res);
        }
    }

    fn build_branch(&mut self, builder: *mut LLVMBuilder, v: &IRValue, then_block: &RefId<IRLabel>, else_block: &RefId<IRLabel>) {
        unsafe {
            LLVMBuildCondBr(builder, self.llvm_val(builder, v), self.blocks[then_block], self.blocks[else_block]);
        }
    }

    fn build_goto(&mut self, builder: *mut LLVMBuilder, b: &RefId<IRLabel>) {
        unsafe {
            LLVMBuildBr(builder, self.blocks[b]);
        }
    }

    fn build_block(&mut self, builder: *mut LLVMBuilder, b: &RefId<IRLabel>) {
        unsafe {
            LLVMPositionBuilderAtEnd(builder, self.blocks[b]);
        }
    }

    fn build_alloca(&mut self, builder: *mut LLVMBuilder, r: &RefId<ResolvedRef>) {
        unsafe {
            let res = LLVMBuildAlloca(builder, LLVMInt32Type(), c_str_ptr!());
            self.values.insert(*r, res);
        }
    }

    fn build_store(&mut self, builder: *mut LLVMBuilder, p: &RefId<ResolvedRef>, v: &IRValue) {
        unsafe {
            LLVMBuildStore(builder, self.llvm_val(builder, v), self.values[p]);
        }
    }

    fn build_load(&mut self, builder: *mut LLVMBuilder, p: &RefId<ResolvedRef>, r: &RefId<ResolvedRef>) {
        unsafe {
            let res = LLVMBuildLoad2(builder, LLVMInt32Type(), self.values[p], c_str_ptr!());
            self.values.insert(*r, res);
        }
    }
}