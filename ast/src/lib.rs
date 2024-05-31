#![deny(clippy::print_stdout)]

use itertools::Itertools;
use std::fmt::{Display, Result, Write};

/// Analyzed PIL
pub mod analyzed;
/// A typed-checked ASM + PIL AST optimized for analysis
pub mod asm_analysis;
/// An AST for PIL objects
pub mod object;
/// A parsed ASM + PIL AST
pub mod parsed;

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

pub fn writeln_indented_by<S, W>(f: &mut W, s: S, indentation: usize) -> Result
where
    S: Display,
    W: Write,
{
    writeln!(f, "{}", indent(s, indentation))
}

pub fn writeln_indented<S, W>(f: &mut W, s: S) -> Result
where
    S: Display,
    W: Write,
{
    writeln_indented_by(f, s, 1)
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
