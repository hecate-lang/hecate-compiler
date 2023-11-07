use std::fmt::{Display, Debug};

use hecate_util::span::Spanned;

#[derive(Default)]
pub enum RawType<'a> {
    #[default]
    Dummy,
    Never,
    Generic(&'a str, Vec<Spanned<'a, RawType<'a>>>),
    Tuple(Vec<Spanned<'a, RawType<'a>>>),
    Slice(Box<Spanned<'a, RawType<'a>>>),
    Array(Box<Spanned<'a, RawType<'a>>>, Spanned<'a, usize>),
}

impl<'a> RawType<'a> {
    pub fn is_unit(&self) -> bool {
        match self {
            RawType::Tuple(v) => v.is_empty(),
            _ => false
        }
    }

    pub fn is_never(&self) -> bool {
        matches!(self, RawType::Never)
    }

    pub fn is_dummy(&self) -> bool {
        matches!(self, RawType::Dummy)
    }

    pub fn unit() -> Self {
        Self::Tuple(vec![])
    } 
    
    pub fn simple(t: &'a str) -> Self {
        Self::Generic(t, vec![])
    }
}

impl<'a> PartialEq for RawType<'a> {
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

impl<'a> Debug for RawType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl<'a> Display for RawType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawType::Never => write!(f, "!"),
            RawType::Dummy => write!(f, "_"),
            RawType::Generic(name, v) => if v.is_empty() { 
                write!(f, "{}", name) 
            } else { 
                write!(f, "{}<{}>", name, v.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")) 
            },
            RawType::Tuple(v) => if v.len() == 1 { 
                write!(f, "({},)", *v[0]) 
            } else { 
                write!(f, "({})", v.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")) 
            },
            RawType::Slice(t) => write!(f, "[{}]", ***t),
            RawType::Array(t, l) => write!(f, "[{};{}]", ***t, **l) 
        }
    }
}

#[cfg(test)]
mod tests {
    use hecate_util::span::Span;

    use crate::types::RawType;

    #[test]
    fn type_printing() {
        assert_eq!(
            RawType::Generic("Boof", vec![
                Span::dummied(RawType::simple("i32")), 
                Span::dummied(RawType::Tuple(vec![
                    Span::dummied(RawType::Dummy),
                    Span::dummied(RawType::Array(
                        Box::new(Span::dummied(RawType::simple("i32"))), 
                        Span::dummied(16)
                    ))
                ]))
            ]).to_string(),
            "Boof<i32, (_, [i32;16])>"
        );
    }

    #[test]
    fn special_types() {
        assert!(RawType::Never.is_never());
        assert!(RawType::Dummy.is_dummy());
        assert!(RawType::Tuple(vec![]).is_unit());
        assert_eq!(RawType::Tuple(vec![]), RawType::unit());
        assert!(!RawType::Tuple(vec![Span::dummied(RawType::Never)]).is_unit());
    }

    #[test]
    fn types_comp() {
        assert_eq!(
            RawType::Dummy,
            RawType::Generic("Foo", vec![Span::dummied(RawType::simple("i32"))])
        );
        assert_eq!(
            RawType::Generic("Boof", vec![
                Span::dummied(RawType::simple("i32")), 
                Span::dummied(RawType::Tuple(vec![
                    Span::dummied(RawType::simple("u1024")),
                    Span::dummied(RawType::Dummy)
                ]))
            ]),
            RawType::Generic("Boof", vec![
                Span::dummied(RawType::simple("i32")), 
                Span::dummied(RawType::Tuple(vec![
                    Span::dummied(RawType::Dummy),
                    Span::dummied(RawType::Array(
                        Box::new(Span::dummied(RawType::simple("i32"))), 
                        Span::dummied(16)
                    ))
                ]))
            ])
        )
    }
}