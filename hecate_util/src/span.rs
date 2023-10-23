use std::ops::{Deref, DerefMut};

#[derive(Clone)]
struct Spanned<'a, T> {
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

#[derive(Copy, Clone)]
enum Span<'a> {
    Span {
        source: Source<'a>,
        content: &'a str,
        start: usize,
        end: usize
    },
    Generated
}

#[derive(Copy, Clone, PartialEq)]
enum Source<'a> {
    File {
        path: &'a str,
    },
    String
}

impl<'a> Span<'a> {
    fn join(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Span::Span { source: source_a,  content, start: start_a, end: end_a }, 
                Span::Span { source: source_b, content: _, start: start_b, end: end_b })
                if source_a == source_b => 
                    Some(Span::Span { source: source_a, content: content, start: start_a.min(start_b), end: end_a.max(end_b) }),
            (Span::Generated, Span::Generated) => Some(Span::Generated),
            (_, _) => None
        }
    }
    fn as_str(self) -> Option<&'a str> {
        match self {
            Span::Span { content, start, end, .. } => Some(&content[start..end]),
            Span::Generated => None,
        }
    }
}

impl<'a> Span<'a> {
    fn with<T>(&self, t: T) -> Spanned<'a, T> {
        Spanned { 
            inner: t, loc: *self
        }
    }
}