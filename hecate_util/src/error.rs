use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};
use InternalResult::*;

#[derive(Debug)]
pub enum HecateError {
    PlaceholderError(String),
}

#[derive(Debug)]
pub enum HecateWarning {
    PlaceholderWarning(String),
}

pub struct Unwrapped<T>(T);

pub struct Wrapped<T>(T);

pub struct ReportMeta {
    warnings: Vec<HecateWarning>,
    errors: Vec<HecateError>,
}

impl ReportMeta {
    pub fn new() -> Self {
        ReportMeta {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn add_error(&mut self, error: HecateError) -> &mut Self {
        self.errors.push(error);
        self
    }

    pub fn add_warning(&mut self, warning: HecateWarning) -> &mut Self {
        self.warnings.push(warning);
        self
    }

    pub fn pack<T>(self, value: T) -> HecateReport<Wrapped<T>> {
        HecateReport::from_meta(self, Wrapped(value))
    }

    pub fn as_fatal<T>(self) -> HecateReport<Unwrapped<Infallible>> {
        HecateReport::<Unwrapped<T>>::as_fatal(self)
    }

    fn pack_unwrapped<T>(self, value: T) -> HecateReport<Unwrapped<T>> {
        HecateReport::from_meta(self, Unwrapped(value))
    }
}

enum InternalResult<T> {
    Success(T),
    Fail(Vec<HecateError>, T),
    Fatal(Vec<HecateError>),
}

impl<T> InternalResult<T> {
    pub fn is_fatal(&self) -> bool {
        match self {
            Fatal(_) => true,
            _ => false,
        }
    }
}

pub struct HecateReport<T> {
    warnings: Vec<HecateWarning>,
    internal_result: InternalResult<T>,
}

impl<T> HecateReport<T> {
    pub fn is_fatal(&self) -> bool {
        self.internal_result.is_fatal()
    }

    pub fn warnings(&self) -> &Vec<HecateWarning> {
        &self.warnings
    }

    fn from_meta(meta: ReportMeta, value: T) -> Self {
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

impl<T> HecateReport<Wrapped<T>> {
    pub fn pure(value: T) -> Self {
        ReportMeta::new().pack(value)
    }

    pub fn result(&self) -> Result<&T, &Vec<HecateError>> {
        match &self.internal_result {
            Success(value) => Ok(&value.0),
            Fatal(errors) | Fail(errors, _) => Err(errors),
        }
    }

    pub fn unpack(mut self, meta: &mut ReportMeta) -> HecateReport<Unwrapped<T>> {
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

impl<T> HecateReport<Unwrapped<T>> {
    fn pure_unwrapped(value: T) -> Self {
        ReportMeta::new().pack_unwrapped(value)
    }

    fn to_residual(self) -> <HecateReport<Unwrapped<T>> as Try>::Residual {
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

impl<T> FromResidual for HecateReport<Unwrapped<T>> {
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

impl<T> FromResidual<<HecateReport<Unwrapped<T>> as Try>::Residual> for HecateReport<Wrapped<T>> {
    fn from_residual(residual: <HecateReport<Unwrapped<T>> as Try>::Residual) -> Self {
        match residual.internal_result {
            Fatal(error) => HecateReport {
                warnings: residual.warnings,
                internal_result: Fatal(error),
            },
            _ => unreachable!(),
        }
    }
}

impl<T> Try for HecateReport<Unwrapped<T>> {
    type Output = T;

    type Residual = HecateReport<Wrapped<Infallible>>;

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
    ($meta:ident, $($arg:tt)*) => {
        $meta.add_warning(hecate_util::error::HecateWarning::PlaceholderWarning(format!($($arg)*)));
    };
}

#[macro_export]
macro_rules! hecate_error {
    ($meta:ident, $($arg:tt)*) => {
        $meta.add_error(hecate_util::error::HecateError::PlaceholderError(format!($($arg)*)));
    };
}

#[macro_export(local_inner_macros)]
macro_rules! hecate_fatal_error {
    ($meta:ident, $($arg:tt)*) => {
        hecate_error!($meta, $($arg)*);
        $meta.as_fatal::<()>()?;
        std::unreachable!()
    };
}
