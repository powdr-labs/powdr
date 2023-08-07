use std::fmt::{Display, Formatter, Result};

use super::{Function, Instr, Link, LinkFrom, LinkTo, Location, Object, PILGraph};

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
        if let Some(degree) = self.degree {
            writeln!(f, "// Degree {}", degree)?;
        }
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

impl<T: Display> Display for Link<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "// {} links to {}", self.from, self.to)
    }
}

impl Display for LinkFrom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.instr)
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "// instr {} with params {}", self.name, self.params)
    }
}

impl<T: Display> Display for LinkTo<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "// {} in object with latch \"{}\" and function_id \"{}\"",
            self.function, self.latch, self.function_id
        )
    }
}

impl<T: Display> Display for Function<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "// function {} with id {} with params {}",
            self.name, self.id, self.params,
        )
    }
}
