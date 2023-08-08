use std::{
    fmt::{Display, Formatter, Result},
    iter::once,
};

use super::{
    AnalysisASMFile, AssignmentStatement, CallableSymbol, CallableSymbolDeclarationRef,
    DebugDirective, DegreeStatement, FunctionBody, FunctionStatement, FunctionStatements,
    Incompatible, IncompatibleSet, InstructionDefinitionStatement, InstructionStatement,
    LabelStatement, Machine, PilBlock, RegisterDeclarationStatement, RegisterTy, Rom,
};

impl<T: Display> Display for AnalysisASMFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (name, machine) in &self.machines {
            write!(f, "machine {name}{machine}")?;
        }
        Ok(())
    }
}

/// quick and dirty String to String indentation
fn indent<S: ToString>(s: S, indentation: usize) -> String {
    s.to_string()
        .split('\n')
        .map(|line| format!("{}{line}", "\t".repeat(indentation)))
        .collect::<Vec<_>>()
        .join("\n")
}

/// quick and dirty String to String way to comment out
fn comment_out<S: ToString>(s: S) -> String {
    s.to_string()
        .split('\n')
        .map(|line| format!("//{line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match (&self.latch, &self.operation_id) {
            (Some(latch), Some(operation_id)) => write!(f, "({latch}, {operation_id})"),
            (None, None) => write!(f, ""),
            (Some(latch), None) => write!(f, "({latch}, _)"),
            (None, Some(operation_id)) => write!(f, "(_, {operation_id})"),
        }?;

        writeln!(f, " {{")?;

        // TODO: implement indentation properly (passing a context to the visitor)
        for s in &self.degree {
            writeln!(f, "{}", indent(s, 1))?;
        }
        writeln!(f)?;
        for s in &self.registers {
            writeln!(f, "{}", indent(s, 1))?;
        }
        writeln!(f)?;
        for i in &self.instructions {
            writeln!(f, "{}", indent(i, 1))?;
        }
        writeln!(f)?;
        for c in self.callable.iter() {
            writeln!(f, "{}", indent(c, 1))?;
        }
        writeln!(f)?;
        if let Some(rom) = &self.rom {
            writeln!(f, "{}", indent(comment_out(rom), 1),)?;
            writeln!(f)?;
        }
        for s in &self.constraints {
            writeln!(f, "{}", indent(s, 1))?;
        }
        writeln!(f, "}}")
    }
}

impl<T: Display> Display for Rom<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "rom {{")?;
        writeln!(f, "{}", indent(&self.statements, 1))?;
        write!(f, "}}")
    }
}

impl Display for DegreeStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "degree {};", self.degree)
    }
}

impl<T: Display> Display for FunctionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionStatement::Assignment(s) => write!(f, "{s}"),
            FunctionStatement::Instruction(s) => write!(f, "{s}"),
            FunctionStatement::Label(s) => write!(f, "{s}"),
            FunctionStatement::DebugDirective(d) => write!(f, "{d}"),
        }
    }
}

impl<T: Display> Display for AssignmentStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{} <={}= {};",
            self.lhs.join(", "),
            self.using_reg
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_default(),
            self.rhs
        )
    }
}

impl Display for DebugDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.directive)
    }
}

impl<T: Display> Display for InstructionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{};",
            self.instruction,
            if self.inputs.is_empty() {
                "".to_string()
            } else {
                format!(
                    " {}",
                    self.inputs
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        )
    }
}

impl Display for LabelStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}::", self.name)
    }
}

impl<T: Display> Display for PilBlock<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "constraints {{")?;
        for statement in &self.statements {
            writeln!(f, "{}", indent(statement, 1))?;
        }
        writeln!(f, "}}")
    }
}

impl Display for RegisterDeclarationStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "reg {}{};", self.name, self.ty,)
    }
}

impl Display for RegisterTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Assignment => write!(f, "[<=]"),
            Self::Write => write!(f, ""),
            Self::ReadOnly => write!(f, "[@r]"),
            Self::Pc => write!(f, "[@pc]"),
        }
    }
}

impl<T: Display> Display for InstructionDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "instr {}{} {{ {} }}", self.name, self.params, self.body)
    }
}

impl<'a, T: Display> Display for CallableSymbolDeclarationRef<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.symbol {
            CallableSymbol::Function(s) => {
                writeln!(f, "function {}{} {{", self.name, s.params)?;
                writeln!(f, "{}", indent(&s.body, 1))?;
                write!(f, "}}")
            }
            CallableSymbol::Operation(s) => {
                write!(f, "operation {}{}{};", self.name, s.id, s.params)
            }
        }
    }
}

impl<T: Display> Display for FunctionStatements<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let res = match self.batches.is_some() {
            true => self
                .iter_batches()
                .flat_map(|batch| {
                    batch
                        .statements
                        .iter()
                        .map(|s| s.to_string())
                        .chain(once(format!(
                            "// END BATCH{}",
                            batch
                                .reason
                                .as_ref()
                                .map(|reason| format!(" {reason}"))
                                .unwrap_or_default()
                        )))
                })
                .collect::<Vec<String>>()
                .join("\n"),
            false => self
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
        };
        write!(f, "{res}")
    }
}

impl<T: Display> Display for FunctionBody<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.statements)
    }
}

impl Display for Incompatible {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self)
    }
}

impl Display for IncompatibleSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
