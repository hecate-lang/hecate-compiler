use std::{marker::PhantomData, fmt::{Debug, Display}, collections::HashMap, hash::Hash};

use hecate_util::{ast::AstInfo};
use rand::random;

pub struct RefId<T> {
    pub id: u64,
    phantom: PhantomData<T>
}

impl<T> PartialEq<RefId<T>> for RefId<T> {
    fn eq(&self, other: &RefId<T>) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for RefId<T> {}

impl<T> Hash for RefId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> Debug for RefId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:016X}", ("::".to_string() + std::any::type_name::<T>()).rsplit_once("::").unwrap().1, self.id)
    }
}

impl<T> Display for RefId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016X}", self.id)
    }
}

impl<T> Clone for RefId<T> {
    #[allow(clippy::non_canonical_clone_impl)]
    fn clone(&self) -> Self {
        Self { id: self.id, phantom: self.phantom }
    }
}

impl<T> Copy for RefId<T> {}

impl<T> RefId<T> {
    pub fn new() -> Self {
        Self { 
            id: random(),
            phantom: Default::default()
        }
    }
}

impl<T> Default for RefId<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct FullyResolved<'a>(std::marker::PhantomData<&'a()>);

pub struct ModData<'a> {
    pub references: HashMap<RefId<ResolvedRef>, &'a str>
}

impl<'a> AstInfo for FullyResolved<'a> {
    type Type = RefId<ResolvedType>;
    type Ident = RefId<ResolvedRef>;
    type ModuleData = ModData<'a>;
}

#[derive(Debug)]
pub struct ResolvedType;

#[derive(Debug)]
pub struct ResolvedRef;