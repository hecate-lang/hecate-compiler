use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};
use InternalHecateResult::*;

#[derive(Debug)]
pub enum HecateError {
    PlaceholderError(String),
}

#[derive(Debug)]
pub enum HecateWarning {
    PlaceholderWarning(String),
}

enum InternalHecateResult<T> {
    Success(T),
    Fail(Vec<HecateError>, Option<T>),
}

pub struct HecateMeta {
    warnings: Vec<HecateWarning>,
    errors: Vec<HecateError>,
}

impl HecateMeta {
    pub fn new() -> Self {
        Self::from_raw(Vec::new(), Vec::new())
    }

    pub fn add_error(&mut self, error: HecateError) -> &mut Self {
        self.errors.push(error);
        self
    }

    pub fn add_warning(&mut self, warning: HecateWarning) -> &mut Self {
        self.warnings.push(warning);
        self
    }

    pub fn join(&mut self, mut report_data: HecateMeta) {
        self.warnings.append(&mut report_data.warnings);
        self.errors.append(&mut report_data.errors);
    }

    pub fn pack<T>(self, value: T) -> HecateReport<T> {
        HecateReport::from_data(HecateReportData::from_meta(self, value))
    }

    pub fn to_fatal_error<T>(self) -> HecateReport<Infallible> {
        HecateReport::<T>::from_meta_as_fatal(self)
    }

    fn from_raw(warnings: Vec<HecateWarning>, errors: Vec<HecateError>) -> Self {
        HecateMeta {
            warnings: warnings,
            errors: errors,
        }
    }
}

pub struct HecateReportData<T> {
    value: T,
    warnings: Vec<HecateWarning>,
    errors: Vec<HecateError>,
}

impl<T> HecateReportData<T> {
    pub fn unpack(self, meta: &mut HecateMeta) -> T {
        meta.join(HecateMeta::from_raw(self.warnings, self.errors));
        self.value
    }

    fn from_meta(meta: HecateMeta, value: T) -> Self {
        HecateReportData {
            value: value,
            warnings: meta.warnings,
            errors: meta.errors,
        }
    }
}

pub struct HecateReport<T> {
    warnings: Vec<HecateWarning>,
    internal_result: InternalHecateResult<T>,
}

impl<T> HecateReport<T> {
    pub fn pure(value: T) -> Self {
        HecateMeta::new().pack(value)
    }

    pub fn result(&self) -> Result<&T, &Vec<HecateError>> {
        match &self.internal_result {
            Success(value) => Ok(value),
            Fail(errors, _) => Err(errors),
        }
    }

    pub fn is_fatal_error(&self) -> bool {
        match &self.internal_result {
            Fail(_, None) => true,
            _ => false,
        }
    }

    pub fn warnings(&self) -> &Vec<HecateWarning> {
        &self.warnings
    }

    fn to_data(self) -> HecateReportData<T> {
        match self.internal_result {
            Fail(errors, Some(value)) => HecateReportData {
                value: value,
                warnings: self.warnings,
                errors: errors,
            },
            Success(value) => HecateReportData {
                value: value,
                warnings: self.warnings,
                errors: Vec::new(),
            },
            _ => unreachable!(),
        }
    }

    fn to_residual(self) -> <HecateReport<T> as Try>::Residual {
        match self.internal_result {
            Success(_) => HecateReport {
                warnings: self.warnings,
                internal_result: Fail(Vec::new(), None),
            },
            Fail(errors, _) => HecateReport {
                warnings: self.warnings,
                internal_result: Fail(errors, None),
            },
        }
    }

    fn from_data(record_data: HecateReportData<T>) -> Self {
        if record_data.errors.is_empty() {
            HecateReport {
                warnings: record_data.warnings,
                internal_result: Success(record_data.value),
            }
        } else {
            HecateReport {
                warnings: record_data.warnings,
                internal_result: Fail(record_data.errors, Some(record_data.value)),
            }
        }
    }

    fn from_meta_as_fatal(meta: HecateMeta) -> HecateReport<Infallible> {
        HecateReport {
            warnings: meta.warnings,
            internal_result: Fail(meta.errors, None),
        }
    }
}

impl<T> FromResidual for HecateReport<T> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        match residual.internal_result {
            Fail(error, _) => HecateReport {
                warnings: residual.warnings,
                internal_result: Fail(error, None),
            },
            _ => unreachable!(),
        }
    }
}

impl<T> Try for HecateReport<T> {
    type Output = HecateReportData<T>;

    type Residual = HecateReport<Infallible>;

    fn from_output(output: Self::Output) -> Self {
        HecateReport::from_data(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        if self.is_fatal_error() {
            ControlFlow::Break(self.to_residual())
        } else {
            ControlFlow::Continue(self.to_data())
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
        $meta.to_fatal_error::<()>()?;
        std::unreachable!()
    };
}
