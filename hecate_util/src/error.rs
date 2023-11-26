use colored::Colorize;
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};
use InternalResult::*;

use crate::span::{Span, Spanned};

pub trait HecateError: ToString {}

pub trait MessageFormat {
    fn format_message(&self) -> String;
}

impl<'span> MessageFormat for SpannedBox<'span, dyn HecateError> {
    fn format_message(&self) -> String {
        into_context(
            &Span::dummy(),
            format!(
                "{}: {}",
                "error".red().bold(),
                self.to_string().white().bold()
            ),
        )
    }
}

impl<'span> MessageFormat for SpannedBox<'span, dyn HecateWarning> {
    fn format_message(&self) -> String {
        into_context(
            &Span::dummy(),
            format!(
                "{}: {}",
                "warning".yellow().bold(),
                self.to_string().white().bold()
            ),
        )
    }
}

fn into_context(context: &Span, message: String) -> String {
    // Example representation (prototype)
    /*
    [file:line:column] warning: warning message
        |
    101 | let A = 1 + 2;
        |     ^- rename to "a"
    102 | /* some other code here */
    103 | let some_var = 10;
    */
    todo!()
}

type SpannedBox<'a, T> = Spanned<'a, Box<T>>;

pub trait HecateWarning: ToString {}

pub struct Unwrapped<T>(T);

pub struct Wrapped<T>(T);

pub struct ReportMeta<'span> {
    warnings: Vec<SpannedBox<'span, dyn HecateWarning>>,
    errors: Vec<SpannedBox<'span, dyn HecateError>>,
}

impl<'span> ReportMeta<'span> {
    pub fn new() -> Self {
        ReportMeta {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn add_error<'err: 'span>(
        &mut self,
        error: SpannedBox<'err, dyn HecateError>,
    ) -> &mut Self {
        self.errors.push(error);
        self
    }

    pub fn add_warning<'warn: 'span>(
        &mut self,
        warning: SpannedBox<'warn, dyn HecateWarning>,
    ) -> &mut Self {
        self.warnings.push(warning);
        self
    }

    pub fn pack<T>(self, value: T) -> HecateReport<'span, Wrapped<T>> {
        HecateReport::from_meta(self, Wrapped(value))
    }

    pub fn as_fatal<T>(self) -> HecateReport<'span, Unwrapped<Infallible>> {
        HecateReport::<Unwrapped<T>>::as_fatal(self)
    }

    fn pack_unwrapped<T>(self, value: T) -> HecateReport<'span, Unwrapped<T>> {
        HecateReport::from_meta(self, Unwrapped(value))
    }
}

enum InternalResult<'span, T> {
    Success(T),
    Fail(Vec<SpannedBox<'span, dyn HecateError>>, T),
    Fatal(Vec<SpannedBox<'span, dyn HecateError>>),
}

impl<'span, T> InternalResult<'span, T> {
    pub fn is_fatal(&self) -> bool {
        match self {
            Fatal(_) => true,
            _ => false,
        }
    }
}

pub struct HecateReport<'span, T> {
    warnings: Vec<SpannedBox<'span, dyn HecateWarning>>,
    internal_result: InternalResult<'span, T>,
}

impl<'span, T> HecateReport<'span, T> {
    pub fn is_fatal(&self) -> bool {
        self.internal_result.is_fatal()
    }

    pub fn warnings(&self) -> &Vec<SpannedBox<'span, dyn HecateWarning>> {
        &self.warnings
    }

    fn from_meta(meta: ReportMeta<'span>, value: T) -> Self {
        HecateReport {
            warnings: meta.warnings,
            internal_result: if meta.errors.is_empty() {
                Success(value)
            } else {
                Fail(meta.errors, value)
            },
        }
    }
}

impl<'span, T> HecateReport<'span, Wrapped<T>> {
    pub fn pure(value: T) -> Self {
        ReportMeta::new().pack(value)
    }

    pub fn result(&self) -> Result<&T, &Vec<SpannedBox<'span, dyn HecateError>>> {
        match &self.internal_result {
            Success(value) => Ok(&value.0),
            Fatal(errors) | Fail(errors, _) => Err(errors),
        }
    }

    pub fn unpack(mut self, meta: &mut ReportMeta<'span>) -> HecateReport<'span, Unwrapped<T>> {
        match self.internal_result {
            Fatal(mut errors) => {
                self.warnings.append(&mut meta.warnings);
                errors.append(&mut meta.errors);
                HecateReport {
                    warnings: self.warnings,
                    internal_result: Fatal(errors),
                }
            }
            Fail(mut errors, value) => {
                meta.errors.append(&mut errors);
                meta.warnings.append(&mut self.warnings);
                HecateReport::pure_unwrapped(value.0)
            }
            Success(value) => {
                meta.warnings.append(&mut self.warnings);
                HecateReport::pure_unwrapped(value.0)
            }
        }
    }
}

impl<'span, T> HecateReport<'span, Unwrapped<T>> {
    fn pure_unwrapped(value: T) -> Self {
        ReportMeta::new().pack_unwrapped(value)
    }

    fn to_residual(self) -> <HecateReport<'span, Unwrapped<T>> as Try>::Residual {
        match self.internal_result {
            Success(_) => HecateReport {
                warnings: self.warnings,
                internal_result: Fatal(Vec::new()),
            },
            Fatal(errors) | Fail(errors, _) => HecateReport {
                warnings: self.warnings,
                internal_result: Fatal(errors),
            },
        }
    }

    fn as_fatal(meta: ReportMeta) -> HecateReport<Unwrapped<Infallible>> {
        HecateReport {
            warnings: meta.warnings,
            internal_result: Fatal(meta.errors),
        }
    }
}

impl<'span, T> FromResidual for HecateReport<'span, Unwrapped<T>> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        match residual.internal_result {
            Fatal(error) => HecateReport {
                warnings: residual.warnings,
                internal_result: Fatal(error),
            },
            _ => unreachable!(),
        }
    }
}

impl<'span, T> FromResidual<<HecateReport<'span, Unwrapped<T>> as Try>::Residual>
    for HecateReport<'span, Wrapped<T>>
{
    fn from_residual(residual: <HecateReport<'span, Unwrapped<T>> as Try>::Residual) -> Self {
        match residual.internal_result {
            Fatal(error) => HecateReport {
                warnings: residual.warnings,
                internal_result: Fatal(error),
            },
            _ => unreachable!(),
        }
    }
}

impl<'span, T> Try for HecateReport<'span, Unwrapped<T>> {
    type Output = T;

    type Residual = HecateReport<'span, Wrapped<Infallible>>;

    fn from_output(output: Self::Output) -> Self {
        HecateReport::pure_unwrapped(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self.internal_result {
            Fail(_, value) | Success(value) => ControlFlow::Continue(value.0),
            _ => ControlFlow::Break(self.to_residual()),
        }
    }
}

#[macro_export]
macro_rules! hecate_warning {
    ($meta:ident, $warning:expr) => {
        $meta.add_warning(hecate_util::span::Span::dummied(Box::new($warning)));
    };
    ($meta:ident, $span:expr, $warning:expr) => {
        $meta.add_warning($span.with(Box::new($warning)));
    };
}

#[macro_export]
macro_rules! hecate_error {
    ($meta:ident, $error:expr) => {
        $meta.add_error(hecate_util::span::Span::dummied(Box::new($error)));
    };
    ($meta:ident, $span:expr, $error:expr) => {
        $meta.add_error($span.with(Box::new($error)));
    };
}

#[macro_export(local_inner_macros)]
macro_rules! hecate_fatal_error {
    ($meta:ident, $fatal_error:expr) => {
        hecate_error!($meta, $fatal_error);
        $meta.as_fatal::<()>()?;
        std::unreachable!()
    };
}
