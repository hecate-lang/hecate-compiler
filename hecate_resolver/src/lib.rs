use std::marker::PhantomData;

use hecate_util::span::Spanned;
use rand::random;

#[derive(Clone, Copy)]
pub struct RefId<T> {
    id: u64,
    phantom: PhantomData<T>
}

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

pub struct ResolvedType {
    pub id: RefId<ResolvedType>
}

pub struct ResolvedRef<'a> {
    pub name: Spanned<'a, &'a str>,
    pub id: RefId<ResolvedRef<'a>>
}