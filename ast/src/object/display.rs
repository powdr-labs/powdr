use std::fmt::{Display, Formatter, Result};

use super::{Link, LinkFrom, LinkTo, Location, Machine, Object, Operation, PILGraph};

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
        write!(f, "{} links to {}", self.from, self.to)
    }
}

impl<T: Display> Display for LinkFrom<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.flag, self.params)
    }
}

impl<T: Display> Display for LinkTo<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} in {}", self.operation, self.machine)
    }
}

impl Display for Machine {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "object at location \"{}\" with latch \"{}\" and operation_id \"{}\"",
            self.location, self.latch, self.operation_id
        )
    }
}

impl<T: Display> Display for Operation<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "operation \"{}\" with id {} with params {}",
            self.name, self.id, self.params,
        )
    }
}
