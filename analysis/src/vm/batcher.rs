//! Batch compatible statements in each function of each machine

use itertools::Itertools;
use powdr_ast::{
    asm_analysis::{
        AnalysisASMFile, BatchMetadata, FunctionStatement, Incompatible, IncompatibleSet, Machine,
    },
    parsed::asm::AbsoluteSymbolPath,
};

pub fn batch(file: AnalysisASMFile) -> AnalysisASMFile {
    RomBatcher::default().batch(file)
}

#[derive(Default)]
struct Batch<'a> {
    statements: Vec<&'a FunctionStatement>,
}

impl<'a> Batch<'a> {
    fn from_statement(s: &'a FunctionStatement) -> Batch {
        Batch {
            statements: vec![s],
        }
    }

    /// Returns true iff this batch consists exclusively of labels and debug directives
    fn is_only_labels_and_directives(&self) -> bool {
        self.statements.iter().all(|s| {
            matches!(
                s,
                FunctionStatement::Label(..) | FunctionStatement::DebugDirective(..)
            )
        })
    }

    /// Returns true iff this batch contains at least one label
    fn contains_labels(&self) -> bool {
        self.statements
            .iter()
            .any(|s| matches!(s, FunctionStatement::Label(..)))
    }

    fn try_absorb(
        &mut self,
        s: &'a FunctionStatement,
    ) -> Result<(), (&'a FunctionStatement, IncompatibleSet)> {
        let batch = Self::from_statement(s);
        self.try_join(batch)
            .map_err(|(b, incompatible)| (b.statements.into_iter().next().unwrap(), incompatible))
    }

    fn try_join(&mut self, other: Self) -> Result<(), (Self, IncompatibleSet)> {
        match (
            self.is_only_labels_and_directives(),
            other.contains_labels(),
        ) {
            // we can join any batch full of labels and debug directives (in particular, an empty batch) with any batch
            (true, _) => {
                self.statements.extend(other.statements);
                Ok(())
            }
            // we cannot join a batch which doesn't only have labels and debug directives with a batch which contains a label
            (false, true) => Err((other, IncompatibleSet([Incompatible::Label].into()))),
            // other types of batching are unimplemented
            (false, false) => Err((other, IncompatibleSet([Incompatible::Unimplemented].into()))),
        }
    }
}

#[derive(Default)]
struct RomBatcher {}

impl RomBatcher {
    // split a list of statements into compatible batches
    fn extract_batches(&self, machine_name: &AbsoluteSymbolPath, machine: &mut Machine) {
        for definition in machine.function_definitions_mut() {
            let batches: Vec<_> = definition
                .function
                .body
                .statements
                .iter()
                .peekable()
                .batching(|it| {
                    let mut batch = Batch::default();
                    loop {
                        // look at the next statement
                        match it.peek() {
                            // try to add it to this batch
                            Some(new_s) => match batch.try_absorb(new_s) {
                                Ok(()) => {
                                    it.next().unwrap();
                                }
                                Err((_, reason)) => {
                                    let res = BatchMetadata {
                                        size: batch.statements.len(),
                                        reason: Some(reason),
                                    };
                                    break Some(res);
                                }
                            },
                            None => {
                                break match batch.statements.len() {
                                    0 => None,
                                    _ => Some(BatchMetadata {
                                        size: batch.statements.len(),
                                        reason: None,
                                    }),
                                }
                            }
                        }
                    }
                })
                .collect();

            let lines_before = batches.iter().map(BatchMetadata::size).sum::<usize>();
            let lines_after = batches.len();

            log::debug!(
                "Batching complete for function {} in machine {} with savings of {}% in execution trace lines",
                definition.name,
                machine_name,
                match lines_before {
                    0 => 0.,
                    lines_before => (1. - lines_after as f32 / lines_before as f32) * 100.,
                }
            );

            definition.function.body.statements.set_batches(batches);
        }
    }

    pub fn batch(&mut self, mut asm_file: AnalysisASMFile) -> AnalysisASMFile {
        for (name, machine) in asm_file.machines_mut() {
            self.extract_batches(&name, machine);
        }

        asm_file
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, path::PathBuf};

    use powdr_ast::asm_analysis::AnalysisASMFile;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use crate::vm::test_utils::batch_str;

    fn test_batching(path: &str) {
        let base_path = PathBuf::from(format!(
            "{}/../test_data/asm/batching",
            env!("CARGO_MANIFEST_DIR")
        ));
        let file_name = base_path.join(path);
        let expected = fs::read_to_string(file_name).unwrap();

        // remove the batch comments from the expected output before compiling
        let input = expected
            .split('\n')
            .filter(|line| !line.contains("//"))
            .collect::<Vec<_>>()
            .join("\n");

        let batched: AnalysisASMFile = batch_str(&input);

        let result = format!("{batched}").replace("\n\n", "\n");
        let expected = expected.replace("\n\n", "\n");
        // The stdlib is added by the importer, so we only check for the initial part.
        assert!(result.len() >= expected.len());
        assert!(expected.len() >= 50);
        assert_eq!(result[..expected.len()], expected);
    }

    #[test]
    fn labels() {
        test_batching("labels.asm")
    }
}
