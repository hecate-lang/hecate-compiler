use std::{collections::HashMap, cell::Ref, fmt::Display, sync::Mutex};

use hecate_resolver::{RefId, ResolvedRef};
use hecate_util::{span::Spanned, ast::BinaryOp};
use once_cell::sync::Lazy;

#[derive(Debug)]
pub struct IRModule<'a> {
    pub name: RefId<ResolvedRef>,
    pub references: &'a HashMap<RefId<ResolvedRef>, &'a str>,
    pub functions: Vec<IRFunction>
}

pub struct IRLabel;

#[derive(Debug)]
pub struct IRFunction {
    pub name: RefId<ResolvedRef>,
    pub args: Vec<RefId<ResolvedRef>>,
    pub instrs: Vec<IRInstr>
}

#[derive(Clone, Copy, Debug)]
pub enum IRValue {
    Ref(RefId<ResolvedRef>),
    Var(RefId<ResolvedRef>),
    Literal(i32),
    None
}

#[derive(Debug)]
pub enum IRInstr {
    Block(RefId<IRLabel>),
    Goto(RefId<IRLabel>),
    Phi(RefId<ResolvedRef>, Vec<(IRValue, RefId<IRLabel>)>),
    /// cond_val, then_block, else_block
    Branch(IRValue, RefId<IRLabel>, RefId<IRLabel>),
    /// return_val, func_name, args
    Call(RefId<ResolvedRef>, RefId<ResolvedRef>, Vec<IRValue>),
    Return(IRValue),
    Alloca(RefId<ResolvedRef>),
    Store(RefId<ResolvedRef>, IRValue),
    Load(RefId<ResolvedRef>, IRValue),
    BinaryOp(RefId<ResolvedRef>, BinaryOp, IRValue, IRValue)
}

impl<'a> Display for IRModule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "// module_name = \"{}\"\n\n{}", self.references[&self.name],
        self.functions.iter().map(|f| {
            unsafe { VALUE_NAME_COUNTER = 0; }
            format!("fun {} [{}]:\n{}", self.references[&f.name],
            f.args.iter().map(|v| format!("@{v}")).collect::<Vec<_>>().join(", "),
            f.instrs.iter().map(|i| i.stringify(&self)).collect::<Vec<_>>().join("\n")
        )}).collect::<Vec<_>>().join("\n\n"))
    }
}

static mut VALUE_NAME_COUNTER: usize = 0;
static NAMES: Lazy<Mutex<HashMap<u64, String>>> = Lazy::new(|| Mutex::new(HashMap::new()));

fn name_for_ref<T>(id: &RefId<T>) -> String {
    let mut names = NAMES.lock().unwrap();
    if let Some(name) = names.get(&id.id) {
        name.to_string()
    } else {
        let mut v = unsafe { 
            VALUE_NAME_COUNTER += 1; 
            VALUE_NAME_COUNTER
        };
        v -= 1;
        let mut name = String::new();
        loop {
            let i = v % 26;
            v /= 26;
            name.push((b'a' + i as u8) as char);
            if v == 0 { break}
        }
        names.insert(id.id, name.clone());
        name
    }
}

impl Display for IRValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = match self {
            IRValue::Ref(r) => r,
            IRValue::Var(r) => r,
            IRValue::Literal(l) => return write!(f, "{l}"),
            IRValue::None => return write!(f, "()"),
        };

        let name = name_for_ref(id);
        
        write!(f, "{}", match self {
            IRValue::Ref(_) => format!("@{name}"),
            IRValue::Var(_) => format!("${name}"),
            _ => unreachable!()
        })
    }
}

impl IRInstr {
    fn stringify(&self, module: &IRModule) -> String {
        match self {
            IRInstr::Block(l) => format!("  {}:", name_for_ref(l)),
            IRInstr::Goto(l) => format!("    goto {}", name_for_ref(l)),
            IRInstr::Phi(r, v) => format!("    @{} = psi [{}]", name_for_ref(r), v.iter().map(|(v, l)| format!("{v} from {}", name_for_ref(l))).collect::<Vec<_>>().join(", ")),
            IRInstr::Branch(c, i, e) => format!("    branch {c} ? {} : {}", name_for_ref(i), name_for_ref(e)),
            IRInstr::Call(r, c, v) => format!("    @{} = call {} [{}]", name_for_ref(r), module.references[c], v.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", ")),
            IRInstr::Return(v) => format!("    return {v}"),
            IRInstr::Alloca(r) => format!("    @{} = alloca <todo>", name_for_ref(r)),
            IRInstr::Store(r, v) => format!("    store {r} = {v}"),
            IRInstr::Load(r, v) => format!("    @{} = load {v}", name_for_ref(r)),
            IRInstr::BinaryOp(r, op, a, b) => format!("    @{} = {op:?} {a}, {b}", name_for_ref(r))
        }
    }
}