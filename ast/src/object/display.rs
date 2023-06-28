use std::fmt::{Display, Formatter, Result};

use super::{Instr, Link, LinkFrom, LinkTo, Location, Object, Operation, PILGraph};

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.limbs.join("_"))
    }
}

impl<T: Display> Display for PILGraph<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (location, object) in &self.objects {
            writeln!(f, "// Object {}", location)?;
            writeln!(f, "{object}")?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<T: Display> Display for Object<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "// Degree {}", self.degree)?;
        for s in &self.pil {
            writeln!(f, "{s}")?;
        }
        writeln!(f, "// Links:")?;
        for link in &self.links {
            writeln!(f, "// {link}")?;
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
        write!(f, "{}", self.instr)
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "instr {} with params {}", self.name, self.params)
    }
}

impl Display for LinkTo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{} in object {} with latch {}",
            self.operation,
            self.loc,
            self.latch
                .as_ref()
                .map(|i| i.to_string())
                .unwrap_or("TBD".to_string())
        )
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "operation {} with id {} with params {}",
            self.name,
            self.index
                .as_ref()
                .map(|i| i.to_string())
                .unwrap_or("TBD".to_string()),
            self.params
                .as_ref()
                .map(|p| p.to_string())
                .unwrap_or("TBD".to_string())
        )
    }
}
