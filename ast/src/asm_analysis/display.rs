use std::{
    fmt::{Display, Formatter, Result},
    iter::once,
};

use super::{
    AnalysisASMFile, AssignmentStatement, DebugDirective, DegreeStatement, FunctionBody,
    FunctionDefinitionStatement, FunctionStatement, FunctionStatements, Incompatible,
    IncompatibleSet, InstructionDefinitionStatement, InstructionStatement, LabelStatement,
    LinkDefinitionStatement, Machine, PilBlock, RegisterDeclarationStatement, Rom,
    SubmachineDeclaration,
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
        match (&self.latch, &self.function_id) {
            (Some(latch), Some(function_id)) => write!(f, "({latch}, {function_id})"),
            (None, None) => write!(f, ""),
            (Some(latch), None) => write!(f, "({latch}, ?)"),
            (None, Some(function_id)) => write!(f, "(?, {function_id})"),
        }?;

        writeln!(f, " {{")?;

        for s in &self.degree {
            writeln!(f, "{}", indent(s, 1))?;
        }
        for (name, ty) in &self.machine_types {
            writeln!(f, "machine {} {{ {} }}", name, indent(ty, 1))?;
        }
        for d in &self.submachines {
            writeln!(f, "{}", indent(d, 1))?;
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
        for l in &self.links {
            writeln!(f, "{}", indent(l, 1))?;
        }
        if let Some(rom) = &self.rom {
            writeln!(f, "{}", indent(comment_out(rom), 1),)?;
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

impl Display for SubmachineDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {};", self.ty, self.name)
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

impl<T: Display> Display for LinkDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "link {} {} = {};", self.flag, self.params, self.to)
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
        let res = self
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
            .join("\n");
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
