use std::{
    fmt::{Display, Formatter, Result},
    iter::once,
};

use super::{
    AnalysisASMFile, AssignmentStatement, DebugDirective, DegreeStatement, FunctionBody,
    FunctionDefinitionStatement, FunctionStatement, FunctionStatements, Incompatible,
    IncompatibleSet, Instruction, InstructionDefinitionStatement, InstructionStatement,
    LabelStatement, Machine, PilBlock, RegisterDeclarationStatement, RegisterTy, Return, Rom,
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

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match (&self.latch, &self.function_id) {
            (Some(latch), Some(function_id)) => write!(f, "({latch}, {function_id})"),
            (None, None) => write!(f, ""),
            (Some(latch), None) => write!(f, "({latch}, _)"),
            (None, Some(function_id)) => write!(f, "(_, {function_id})"),
        }?;

        writeln!(f, " {{")?;

        // TODO: implement indentation properly (passing a context to the visitor)
        for s in &self.degree {
            writeln!(f, "{}", indent(s, 1))?;
        }
        for s in &self.registers {
            writeln!(f, "{}", indent(s, 1))?;
        }
        for s in &self.constraints {
            writeln!(f, "{}", indent(s, 1))?;
        }
        for i in &self.instructions {
            writeln!(f, "{}", indent(i, 1))?;
        }
        for o in &self.functions {
            writeln!(f, "{}", indent(o, 1))?;
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
            FunctionStatement::Return(r) => write!(f, "{r}"),
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

impl<T: Display> Display for Return<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "return{};",
            if self.values.is_empty() {
                "".to_string()
            } else {
                format!(
                    " {}",
                    self.values
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
        write!(f, "instr {}{}", self.name, self.instruction)
    }
}

impl<T: Display> Display for Instruction<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}{}", self.params, self.body)
    }
}

impl<T: Display> Display for FunctionDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(
            f,
            "function {}{}{} {{",
            self.name,
            self.id
                .as_ref()
                .map(|id| format!("<{id}>"))
                .unwrap_or_default(),
            self.params
        )?;
        writeln!(f, "{}", indent(&self.body, 1))?;
        write!(f, "}}")
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
