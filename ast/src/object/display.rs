use std::fmt::{Display, Formatter, Result};

use super::{ExtInstr, Instr, Link, LinkFrom, LinkTo, Location, Object, PILGraph};

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

impl<T: Display> Display for Link<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} -> {}", self.from, self.to)
    }
}

impl Display for LinkFrom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.instr)
    }
}

impl Display for ExtInstr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "instr {} with params {}", self.name, self.params)
    }
}

impl<T: Display> Display for LinkTo<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} in object {}", self.instr, self.loc)
    }
}

impl<T: Display> Display for Instr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "instr {} with params {}",
            self.name,
            self.params
                .as_ref()
                .map(|p| p.to_string())
                .unwrap_or("TBD".to_string())
        )
    }
}
