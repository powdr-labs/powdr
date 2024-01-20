use std::{
    fmt::{Display, Formatter, Result},
    iter::once,
};

use itertools::Itertools;

use crate::{
    indent,
    parsed::asm::{AbsoluteSymbolPath, Part},
    write_indented_by, write_items_indented,
};

use super::{
    AnalysisASMFile, AssignmentStatement, CallableSymbol, CallableSymbolDefinitionRef,
    DebugDirective, DegreeStatement, FunctionBody, FunctionStatement, FunctionStatements,
    Incompatible, IncompatibleSet, Instruction, InstructionDefinitionStatement,
    InstructionStatement, Item, LabelStatement, LinkDefinitionStatement, Machine,
    RegisterDeclarationStatement, RegisterTy, Return, Rom, SubmachineDeclaration,
};

impl<T: Display> Display for AnalysisASMFile<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut current_path = AbsoluteSymbolPath::default();

        for (path, item) in &self.items {
            let relative_path = path.relative_to(&current_path);
            let name = relative_path.name();
            // Skip the name (last) part
            for part in relative_path.parts().rev().skip(1).rev() {
                match part {
                    Part::Super => {
                        current_path.pop();
                        write_indented_by(f, "}\n", current_path.len())?;
                    }
                    Part::Named(m) => {
                        write_indented_by(f, format!("mod {m} {{\n"), current_path.len())?;
                        current_path.push(m.clone());
                    }
                }
            }

            match item {
                Item::Machine(machine) => {
                    write_indented_by(f, format!("machine {name}{machine}"), current_path.len())?;
                }
                Item::Expression(expression) => write_indented_by(
                    f,
                    format!("let {name} = {expression};\n"),
                    current_path.len(),
                )?,
            }
        }
        for i in (0..current_path.len()).rev() {
            write_indented_by(f, "}\n", i)?;
        }

        Ok(())
    }
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

        write_items_indented(f, &self.degree)?;
        write_items_indented(f, &self.submachines)?;
        write_items_indented(f, &self.registers)?;
        write_items_indented(f, &self.instructions)?;
        write_items_indented(f, &self.callable)?;
        write_items_indented(f, &self.pil)?;
        write_items_indented(f, &self.links)?;

        writeln!(f, "}}")
    }
}

impl<T: Display> Display for LinkDefinitionStatement<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "link {}{} = {};",
            self.flag,
            self.params.prepend_space_if_non_empty(),
            self.to
        )
    }
}

impl Display for SubmachineDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.ty, self.name)
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
            self.lhs().format(", "),
            self.assignment_registers().format(", "),
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
                format!(" {}", self.inputs.iter().format(", "))
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
                format!(" {}", self.values.iter().format(", "))
            }
        )
    }
}

impl Display for LabelStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}:", self.name)
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
        write!(
            f,
            "{}{}",
            self.params.prepend_space_if_non_empty(),
            self.body
        )
    }
}

impl<'a, T: Display> Display for CallableSymbolDefinitionRef<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.symbol {
            CallableSymbol::Function(s) => {
                writeln!(
                    f,
                    "function {}{} {{",
                    self.name,
                    s.params.prepend_space_if_non_empty()
                )?;
                writeln!(f, "{}", indent(&s.body, 1))?;
                write!(f, "}}")
            }
            CallableSymbol::Operation(s) => {
                write!(
                    f,
                    "operation {}{}{};",
                    self.name,
                    s.id,
                    s.params.prepend_space_if_non_empty()
                )
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
                .join("\n"),
            false => self.iter().format("\n").to_string(),
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
        write!(f, "{}", self.0.iter().format(", "))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parsed::asm::parse_absolute_path;
    use powdr_number::GoldilocksField;
    use pretty_assertions::assert_eq;

    #[test]
    fn display_asm_analysis_file() {
        let file = AnalysisASMFile::<GoldilocksField> {
            items: [
                "::x::Y",
                "::x::r::T",
                "::x::f::Y",
                "::M",
                "::t::x::y::R",
                "::t::F",
                "::X",
            ]
            .into_iter()
            .map(|s| (parse_absolute_path(s), Item::Machine(Machine::default())))
            .collect(),
        };
        assert_eq!(
            file.to_string(),
            r#"machine M {
}
machine X {
}
mod t {
    machine F {
    }
    mod x {
        mod y {
            machine R {
            }
        }
    }
}
mod x {
    machine Y {
    }
    mod f {
        machine Y {
        }
    }
    mod r {
        machine T {
        }
    }
}
"#
        );
    }
}
