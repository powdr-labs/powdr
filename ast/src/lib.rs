/// Analyzed PIL
pub mod analyzed;
/// A typed-checked ASM + PIL AST optimised for analysis
pub mod asm_analysis;
/// An AST for PIL objects
pub mod object;
/// A parsed ASM + PIL AST
pub mod parsed;

#[derive(Default)]
/// A monitor of the changes applied to the program as we run through the analysis pipeline
pub struct DiffMonitor {
    previous: Option<String>,
    current: Option<String>,
}

impl DiffMonitor {
    /// push a new program and log::trace! how it differs from the previous one, if any
    pub fn push<S: ToString>(&mut self, s: S) {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = Some(s.to_string());

        if let Some(current) = &self.current {
            if let Some(previous) = &self.previous {
                for diff in diff::lines(previous, current) {
                    match diff {
                        diff::Result::Left(l) => log::trace!("-{}", l),
                        diff::Result::Both(..) => {}
                        diff::Result::Right(r) => log::trace!("+{}", r),
                    }
                }
                log::trace!("");
            }
        }
    }
}
