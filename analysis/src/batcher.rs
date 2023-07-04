use std::marker::PhantomData;

use ast::asm_analysis::{
    AnalysisASMFile, BatchMetadata, Incompatible, IncompatibleSet, ProgramStatement,
};
use itertools::Itertools;
use number::FieldElement;

pub fn batch<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    ProgramBatcher::default().batch(file)
}

#[derive(Default)]
struct Batch<'a, T> {
    statements: Vec<&'a ProgramStatement<T>>,
}

impl<'a, T: FieldElement> Batch<'a, T> {
    fn from_statement(s: &'a ProgramStatement<T>) -> Batch<T> {
        Batch {
            statements: vec![s],
        }
    }

    /// Returns true iff this batch consists exclusively of labels
    fn is_only_labels(&self) -> bool {
        self.statements
            .iter()
            .all(|s| matches!(s, ProgramStatement::Label(..)))
    }

    /// Returns true iff this batch contains at least one label
    fn contains_labels(&self) -> bool {
        self.statements
            .iter()
            .any(|s| matches!(s, ProgramStatement::Label(..)))
    }

    fn try_absorb(
        &mut self,
        s: &'a ProgramStatement<T>,
    ) -> Result<(), (&'a ProgramStatement<T>, IncompatibleSet)> {
        let batch = Self::from_statement(s);
        self.try_join(batch)
            .map_err(|(b, incompatible)| (b.statements.into_iter().next().unwrap(), incompatible))
    }

    fn try_join(&mut self, other: Self) -> Result<(), (Self, IncompatibleSet)> {
        match (self.is_only_labels(), other.contains_labels()) {
            // we can join any batch full of labels (in particular, an empty batch) with any batch
            (true, _) => {
                self.statements.extend(other.statements);
                Ok(())
            }
            // we cannot join a batch which doesn't only have labels with a batch which contains a label
            (false, true) => Err((other, IncompatibleSet([Incompatible::Label].into()))),
            // other types of batching are unimplemented
            (false, false) => Err((other, IncompatibleSet([Incompatible::Unimplemented].into()))),
        }
    }
}

#[derive(Default)]
struct ProgramBatcher<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> ProgramBatcher<T> {
    /// split a list of statements into compatible batches
    fn extract_batches<'a>(
        &self,
        statements: impl IntoIterator<Item = &'a ProgramStatement<T>>,
    ) -> Vec<BatchMetadata> {
        statements
            .into_iter()
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
            .collect()
    }

    pub fn batch(&mut self, mut asm_file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
        let batches = self.extract_batches(&asm_file.program);

        let lines_before = batches.iter().map(BatchMetadata::size).sum::<usize>() as f32;
        let lines_after = batches.len() as f32;

        log::debug!(
            "Batching complete with savings of {}% in execution trace lines",
            (1. - lines_after / lines_before) * 100.
        );

        asm_file.batches = Some(batches);

        asm_file
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, path::PathBuf};

    use number::GoldilocksField;
    use pretty_assertions::assert_eq;
    use test_log::test;

    use crate::{batcher, macro_expansion, type_check};

    fn test_batching(path: &str) {
        let base_path = PathBuf::from("../test_data/asm/batching");
        let file_name = base_path.join(path);
        let contents = fs::read_to_string(&file_name).unwrap();
        let parsed = parser::parse_asm::<GoldilocksField>(
            Some(file_name.as_os_str().to_str().unwrap()),
            &contents,
        )
        .unwrap();
        let expanded = macro_expansion::expand(parsed);
        let checked = type_check::check(expanded).unwrap();
        let batched = batcher::batch(checked);
        let mut expected_file_name = file_name;
        expected_file_name.set_file_name(format!(
            "{}_batched.asm",
            expected_file_name.file_stem().unwrap().to_str().unwrap()
        ));
        let expected = fs::read_to_string(expected_file_name).unwrap();

        assert_eq!(
            format!("{batched}").replace("\n\n", "\n"),
            expected.replace("\n\n", "\n")
        );
    }

    #[test]
    fn labels() {
        test_batching("labels.asm")
    }
}
