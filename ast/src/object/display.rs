use std::fmt::{Display, Formatter, Result};

use crate::{asm_analysis::combine_flags, write_items_indented};

use super::{Link, LinkFrom, LinkTo, Location, Machine, MachineInstanceGraph, Object, Operation};

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.limbs.join("_"))
    }
}

impl Display for MachineInstanceGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "// Utilities")?;
        for (module_path, statements) in &self.statements {
            writeln!(f, "mod {module_path} {{")?;
            write_items_indented(f, statements)?;
            writeln!(f, "}}")?;
        }
        for (location, object) in &self.objects {
            writeln!(f, "// Object {location}")?;
            writeln!(f, "{object}")?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "// Degree {}", self.degree)?;
        for s in &self.pil {
            writeln!(f, "{s}")?;
        }
        if !self.links.is_empty() {
            writeln!(f, "// Links:")?;
            for link in &self.links {
                writeln!(f, "// {link}")?;
            }
        }
        Ok(())
    }
}

impl Display for Link {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} links to {}", self.from, self.to)
    }
}

impl Display for LinkFrom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let flag = combine_flags(self.instr_flag.clone(), self.link_flag.clone());
        write!(f, "{flag} {}", self.params)
    }
}

impl Display for LinkTo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} in {}", self.operation, self.machine)
    }
}

impl Display for Machine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "object at location \"{}\" with latch \"{:?}\" and operation_id \"{:?}\"",
            self.location, self.latch, self.operation_id
        )
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "operation \"{}\" with id {:?} with params {}",
            self.name,
            self.id.as_ref().map(|id| id.to_string()),
            self.params,
        )
    }
}
