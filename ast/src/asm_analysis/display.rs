use std::fmt::{Display, Formatter, Result};

use super::{
    AnalysisASMFile, AssignmentStatement, DebugDirective, DegreeStatement, FunctionBody,
    FunctionDefinitionStatement, FunctionStatement, FunctionStatements, Incompatible,
    IncompatibleSet, InstructionDefinitionStatement, InstructionStatement, LabelStatement, Machine,
    PilBlock, RegisterDeclarationStatement, Rom,
};

impl<T: Display> Display for AnalysisASMFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (name, machine) in &self.machines {
            write!(f, "machine {name}{machine}")?;
        }
        Ok(())
    }
}

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match (&self.latch, &self.function_id) {
            (Some(latch), Some(function_id)) => write!(f, "({latch}, {function_id})"),
            (None, None) => write!(f, ""),
            (Some(latch), None) => write!(f, "({latch}, ?)"),
            (None, Some(function_id)) => write!(f, "(?, {function_id})"),
        }?;

        writeln!(f, " {{")?;

        // TODO: implement indentation properly (passing a context to the visitor)
        for s in &self.degree {
            writeln!(f, "{s}")?;
        }
        for s in &self.registers {
            writeln!(f, "{s}")?;
        }
        for s in &self.constraints {
            writeln!(f, "{s}")?;
        }
        for i in &self.instructions {
            writeln!(f, "{i}")?;
        }
        for o in &self.functions {
            writeln!(f, "{o}")?;
        }
        if let Some(rom) = &self.rom {
            writeln!(
                f,
                "{}",
                rom.to_string()
                    .split('\n')
                    .map(|line| format!("\t //{line}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            )?;
        }
        writeln!(f, "}}")
    }
}

impl<T: Display> Display for Rom<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "rom {{")?;
        writeln!(f, "{}", self.statements)?;
        write!(f, "}}")
    }
}

impl Display for DegreeStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "\tdegree {};", self.degree)
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
        writeln!(f, "\tconstraints {{")?;
        for statement in &self.statements {
            writeln!(f, "\t\t{statement}")?;
        }
        writeln!(f, "\t}}")
    }
}

impl Display for RegisterDeclarationStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "\treg {}{};",
            self.name,
            self.flag
                .as_ref()
                .map(|flag| format!("[{flag}]"))
                .unwrap_or_default()
        )
    }
}

impl<T: Display> Display for InstructionDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "\tinstr {}{} {{ {} }}",
            self.name, self.params, self.body
        )
    }
}

impl<T: Display> Display for FunctionDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(
            f,
            "\tfunction {}{}{} {{",
            self.name,
            self.id
                .as_ref()
                .map(|id| format!("<{id}>"))
                .unwrap_or_default(),
            self.params
        )?;
        write!(f, "{}", self.body,)?;
        writeln!(f, "\t}}")
    }
}

impl<T: Display> Display for FunctionStatements<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for batch in self.iter_batches() {
            for statement in batch.statements {
                writeln!(f, "\t{}", statement)?;
            }
            writeln!(
                f,
                "\t// END BATCH{}",
                batch
                    .reason
                    .as_ref()
                    .map(|reason| format!(" {reason}"))
                    .unwrap_or_default()
            )?;
        }
        Ok(())
    }
}

impl<T: Display> Display for FunctionBody<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "{}", self.statements)
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
