#![deny(clippy::print_stdout)]

use itertools::Itertools;
use log::log_enabled;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Result, Write};
use std::sync::Arc;

/// Analyzed PIL
pub mod analyzed;
/// A typed-checked ASM + PIL AST optimised for analysis
pub mod asm_analysis;
/// An AST for PIL objects
pub mod object;
/// A parsed ASM + PIL AST
pub mod parsed;

#[derive(Default, Clone)]
/// A monitor of the changes applied to the program as we run through the analysis pipeline
pub struct DiffMonitor {
    previous: Option<String>,
    current: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
pub struct SourceRef {
    pub file: Option<Arc<str>>,
    pub line: usize,
    pub col: usize,
}

impl SourceRef {
    pub fn unknown() -> Self {
        Self {
            file: None,
            line: 0,
            col: 0,
        }
    }
}

impl DiffMonitor {
    /// push a new program and log::trace! how it differs from the previous one, if any
    pub fn push<S: ToString>(&mut self, s: S) {
        if log_enabled!(log::Level::Trace) {
            std::mem::swap(&mut self.previous, &mut self.current);
            self.current = Some(s.to_string());
            if let (Some(current), Some(previous)) = (&self.current, &self.previous) {
                for diff in diff::lines(previous, current) {
                    match diff {
                        diff::Result::Left(l) => log::trace!("-{}", l),
                        diff::Result::Both(..) => {}
                        diff::Result::Right(r) => log::trace!("+{}", r),
                    }
                }
                log::trace!("");
            }
        }
    }
}

/// quick and dirty String to String indentation
pub fn indent<S: ToString>(s: S, indentation: usize) -> String {
    s.to_string()
        .split('\n')
        .map(|line| match line {
            "" => "".to_string(),
            _ => format!("{}{line}", "    ".repeat(indentation)),
        })
        .join("\n")
}

pub fn write_indented_by<S, W>(f: &mut W, s: S, indentation: usize) -> Result
where
    S: Display,
    W: Write,
{
    write!(f, "{}", indent(s, indentation))
}

fn write_items<S, I, W>(f: &mut W, items: I) -> Result
where
    S: Display,
    I: IntoIterator<Item = S>,
    W: Write,
{
    write_items_indented_by(f, items, 0)
}

fn write_items_indented<S, I, W>(f: &mut W, items: I) -> Result
where
    S: Display,
    I: IntoIterator<Item = S>,
    W: Write,
{
    write_items_indented_by(f, items, 1)
}

fn write_items_indented_by<S, I, W>(f: &mut W, items: I, by: usize) -> Result
where
    S: Display,
    I: IntoIterator<Item = S>,
    W: Write,
{
    for item in items.into_iter() {
        write_indented_by(f, item, by)?;
        writeln!(f)?;
    }
    Ok(())
}
