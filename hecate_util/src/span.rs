use std::{ops::{Deref, DerefMut}, fmt::Debug};

#[derive(Clone)]
pub struct Spanned<'a, T> {
    inner: T,
    loc: Span<'a>
}

impl<'a, T> Deref for Spanned<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, T> DerefMut for Spanned<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a, T: Debug> Debug for Spanned<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ {:?} @ <{:?}> }}", self.inner, self.loc)
    }
}

#[derive(Copy, Clone)]
pub enum Span<'a> {
    Span {
        source: Source<'a>,
        content: &'a str,
        start: usize,
        end: usize
    },
    Generated
}

impl<'a> Debug for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Span { start, end, .. } => write!(f, "{}:{}", start, end),
            Self::Generated => write!(f, "Generated"),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Source<'a> {
    File {
        path: &'a str,
    },
    String
}

impl<'a> Span<'a> {
    pub fn from_source(source: Source<'a>, content: &'a str, start: usize, end: usize) -> Self {
        Self::Span { source, content, start, end }
    }

    pub fn dummy() -> Self {
        Self::Generated
    }

    pub fn join(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Span::Span { source: source_a,  content, start: start_a, end: end_a }, 
                Span::Span { source: source_b, content: _, start: start_b, end: end_b })
                if source_a == source_b => 
                    Some(Span::Span { source: source_a, content: content, start: start_a.min(start_b), end: end_a.max(end_b) }),
            (Span::Generated, Span::Generated) => Some(Span::Generated),
            (_, _) => None
        }
    }
    
    pub fn as_str(self) -> Option<&'a str> {
        match self {
            Span::Span { content, start, end, .. } => Some(&content[start..end]),
            Span::Generated => None,
        }
    }

    pub fn with<T>(&self, t: T) -> Spanned<'a, T> {
        Spanned { 
            inner: t, loc: *self
        }
    }
}