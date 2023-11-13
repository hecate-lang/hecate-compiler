use std::fmt::{Display, Debug};

use hecate_util::span::Spanned;

#[derive(Default)]
pub enum GenericType<'a, T: Display + PartialEq> {
    #[default]
    Dummy,
    Never,
    Generic(T, Vec<Spanned<'a, GenericType<'a, T>>>),
    Tuple(Vec<Spanned<'a, GenericType<'a, T>>>),
    Slice(Box<Spanned<'a, GenericType<'a, T>>>),
    Array(Box<Spanned<'a, GenericType<'a, T>>>, Spanned<'a, usize>),
}

impl<'a, T: Display + PartialEq> GenericType<'a, T> {
    pub fn is_unit(&self) -> bool {
        match self {
            GenericType::Tuple(v) => v.is_empty(),
            _ => false
        }
    }

    pub fn is_never(&self) -> bool {
        matches!(self, GenericType::Never)
    }

    pub fn is_dummy(&self) -> bool {
        matches!(self, GenericType::Dummy)
    }

    pub fn unit() -> Self {
        Self::Tuple(vec![])
    } 
    
    pub fn simple(t: T) -> Self {
        Self::Generic(t, vec![])
    }
}

impl<'a, T: Display + PartialEq> PartialEq for GenericType<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Dummy, _) | (_, Self::Dummy) => true,
            (Self::Never, Self::Never) => false,
            (Self::Generic(lname, lgen), Self::Generic(rname, rgen)) => lname == rname && lgen.len() == rgen.len() && lgen.iter().zip(rgen).all(|(l, r)| **l == **r),
            (Self::Tuple(ltys), Self::Tuple(rtys)) => ltys.len() == rtys.len() && ltys.iter().zip(rtys).all(|(l, r)| **l == **r),
            (Self::Slice(lty), Self::Slice(rty)) => ***lty == ***rty,
            (Self::Array(lty, ls), Self::Array(rty, rs)) => ***lty == ***rty && **ls == **rs,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<'a, T: Display + PartialEq> Debug for GenericType<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl<'a, T: Display + PartialEq> Display for GenericType<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericType::Never => write!(f, "!"),
            GenericType::Dummy => write!(f, "_"),
            GenericType::Generic(name, v) => if v.is_empty() { 
                write!(f, "{}", name) 
            } else { 
                write!(f, "{}<{}>", name, v.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")) 
            },
            GenericType::Tuple(v) => if v.len() == 1 { 
                write!(f, "({},)", *v[0]) 
            } else { 
                write!(f, "({})", v.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")) 
            },
            GenericType::Slice(t) => write!(f, "[{}]", ***t),
            GenericType::Array(t, l) => write!(f, "[{};{}]", ***t, **l) 
        }
    }
}

#[cfg(test)]
mod tests {
    use hecate_util::span::Span;

    use crate::types::GenericType;

    #[test]
    fn type_printing() {
        assert_eq!(
            GenericType::Generic("Boof", vec![
                Span::dummied(GenericType::simple("i32")), 
                Span::dummied(GenericType::Tuple(vec![
                    Span::dummied(GenericType::Dummy),
                    Span::dummied(GenericType::Array(
                        Box::new(Span::dummied(GenericType::simple("i32"))), 
                        Span::dummied(16)
                    ))
                ]))
            ]).to_string(),
            "Boof<i32, (_, [i32;16])>"
        );
    }

    #[test]
    fn special_types() {
        assert!(GenericType::<&str>::Never.is_never());
        assert!(GenericType::<&str>::Dummy.is_dummy());
        assert!(GenericType::<&str>::Tuple(vec![]).is_unit());
        assert_eq!(GenericType::<&str>::Tuple(vec![]), GenericType::unit());
        assert!(!GenericType::<&str>::Tuple(vec![Span::dummied(GenericType::Never)]).is_unit());
    }

    #[test]
    fn types_comp() {
        assert_eq!(
            GenericType::Dummy,
            GenericType::Generic("Foo", vec![Span::dummied(GenericType::simple("i32"))])
        );
        assert_eq!(
            GenericType::Generic("Boof", vec![
                Span::dummied(GenericType::simple("i32")), 
                Span::dummied(GenericType::Tuple(vec![
                    Span::dummied(GenericType::simple("u1024")),
                    Span::dummied(GenericType::Dummy)
                ]))
            ]),
            GenericType::Generic("Boof", vec![
                Span::dummied(GenericType::simple("i32")), 
                Span::dummied(GenericType::Tuple(vec![
                    Span::dummied(GenericType::Dummy),
                    Span::dummied(GenericType::Array(
                        Box::new(Span::dummied(GenericType::simple("i32"))), 
                        Span::dummied(16)
                    ))
                ]))
            ])
        )
    }
}