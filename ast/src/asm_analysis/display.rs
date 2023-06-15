use std::fmt::{Display, Formatter, Result};

use super::{
    AnalysisASMFile, AssignmentStatement, DegreeStatement, Incompatible, IncompatibleSet,
    InstructionDefinitionStatement, InstructionStatement, LabelStatement, Machine, PilBlock,
    Program, ProgramStatement, RegisterDeclarationStatement,
};

impl<T: Display> Display for AnalysisASMFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (name, machine) in &self.machines {
            writeln!(f, "machine {name} {{")?;
            writeln!(f, "{}", machine)?;
            writeln!(f, "}}")?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<T: Display> Display for Machine<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for s in &self.degree {
            writeln!(f, "\t{s}")?;
        }
        for s in &self.registers {
            writeln!(f, "\t{s}")?;
        }
        for s in &self.constraints {
            writeln!(f, "\t{s}")?;
        }
        for i in &self.instructions {
            writeln!(f, "\t{i}")?;
        }
        writeln!(f, "{}", self.program)?;
        Ok(())
    }
}

impl<T: Display> Display for Program<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "\tprogram {{")?;
        let statements = &mut self.statements.iter();
        match &self.batches {
            Some(batches) => {
                for batch in batches {
                    for statement in statements.take(batch.size) {
                        writeln!(f, "\t\t{}", statement)?;
                    }
                    writeln!(
                        f,
                        "\t\t// END BATCH{}",
                        batch
                            .reason
                            .as_ref()
                            .map(|reason| format!(" {reason}"))
                            .unwrap_or_default()
                    )?;
                }
            }
            None => {
                for statement in statements {
                    writeln!(f, "\t\t{}", statement)?;
                }
            }
        }
        write!(f, "\t}}")
    }
}

impl Display for DegreeStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "degree {};", self.degree)
    }
}

impl<T: Display> Display for ProgramStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ProgramStatement::Assignment(s) => write!(f, "{s}"),
            ProgramStatement::Instruction(s) => write!(f, "{s}"),
            ProgramStatement::Label(s) => write!(f, "{s}"),
            ProgramStatement::DebugDirective(d) => write!(f, "{d}"),
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
        write!(
            f,
            "pil{{\n{}\n}}",
            self.statements
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Display for RegisterDeclarationStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "reg {}{};",
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
        write!(f, "instr {}{} {{{}}}", self.name, self.params, self.body)
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
