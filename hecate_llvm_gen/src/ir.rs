use std::{collections::HashMap, cell::Ref, fmt::Display};

use hecate_resolver::{RefId, ResolvedRef};
use hecate_util::span::Spanned;

#[derive(Debug)]
pub struct IRModule<'a> {
    pub name: RefId<ResolvedRef>,
    pub references: &'a HashMap<RefId<ResolvedRef>, &'a str>,
    pub functions: Vec<IRFunction<'a>>
}

pub struct IRLabel<'a>(&'a str);

#[derive(Debug)]
pub struct IRFunction<'a> {
    pub name: RefId<ResolvedRef>,
    pub instrs: Vec<IRInstr<'a>>
}

#[derive(Clone, Copy, Debug)]
pub enum IRValue {
    Ref(RefId<ResolvedRef>),
    Literal(i32),
    None
}

#[derive(Debug)]
pub enum IRInstr<'a> {
    Block(RefId<IRLabel<'a>>),
    Goto(RefId<IRLabel<'a>>),
    Phi(IRValue, Vec<(IRValue, RefId<IRLabel<'a>>)>),
    /// cond_val, then_block, else_block
    Branch(IRValue, RefId<IRLabel<'a>>, RefId<IRLabel<'a>>),
    /// return_val, func_name, args
    Call(IRValue, RefId<ResolvedRef>, Vec<IRValue>),
    Return(IRValue),
    Future
}

impl<'a> Display for IRModule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "// module_name = \"{}\"\n\n{}", self.references[&self.name],
        self.functions.iter().map( |f|
            format!("fun {}:\n{}", self.references[&f.name],
                f.instrs.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("\n")
            )
        ).collect::<Vec<_>>().join("\n\n"))
    }
}

impl<'a> Display for IRValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            IRValue::Ref(r) => format!("@{r}"),
            IRValue::Literal(l) => format!("{l}"),
            IRValue::None => format!("()"),
        })
    }
}
impl<'a> Display for IRInstr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            IRInstr::Block(l) => format!("  {l}"),
            IRInstr::Goto(l) => format!("       goto {l}"),
            IRInstr::Phi(r, v) => format!("     {r} = psi [{}]", v.iter().map(|(v, l)| format!("{v} from {l}")).collect::<Vec<_>>().join(", ")),
            IRInstr::Branch(c, i, e) => format!("       branch {c} ? {i} : {e}"),
            IRInstr::Call(r, c, v) => format!("     {r} = call {c} [{}]", v.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", ")),
            IRInstr::Return(v) => format!("     return {v}"),
            IRInstr::Future => format!("        <future>")
        })
    }
}