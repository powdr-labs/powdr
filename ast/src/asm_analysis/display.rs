use std::fmt::{Display, Formatter, Result};

use super::{
    AnalysisASMFile, AssignmentStatement, DebugDirective, DegreeStatement, FunctionBody,
    FunctionDefinitionStatement, FunctionStatement, Incompatible, IncompatibleSet,
    InstructionDefinitionStatement, InstructionStatement, LabelStatement, Machine, PilBlock,
    RegisterDeclarationStatement, Rom,
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
        // TODO: implement indentation properly (passing a context to the visitor)
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
        for o in &self.functions {
            writeln!(f, "\t{o}")?;
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
        Ok(())
    }
}

impl<T: Display> Display for Rom<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "rom {{")?;
        let statements = &mut self.statements.iter();
        match &self.batches {
            Some(batches) => {
                for batch in batches {
                    for statement in statements.take(batch.size) {
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
            }
            None => {
                for statement in statements {
                    writeln!(f, "\t {}", statement)?;
                }
            }
        }
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
        write!(
            f,
            "constraints {{\n{}\n}}",
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
        write!(f, "instr {}{} {{ {} }}", self.name, self.params, self.body)
    }
}

impl<T: Display> Display for FunctionDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "function {}{} {{\n{}\n\t}}",
            self.name, self.params, self.body,
        )
    }
}

impl<T: Display> Display for FunctionBody<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for s in &self.statements {
            writeln!(f, "\t\t{s}")?;
        }
        Ok(())
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
