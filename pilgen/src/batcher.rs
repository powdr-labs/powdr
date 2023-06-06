use std::marker::PhantomData;

use itertools::Itertools;
use number::FieldElement;
use parser::asm_ast::{
    batched::{ASMStatementBatch, BatchedASMFile, Incompatible, IncompatibleSet},
    ASMFile, ASMStatement,
};

#[derive(Default)]
struct Batch<T> {
    statements: Vec<ASMStatement<T>>,
}

impl<T: FieldElement> Batch<T> {
    fn from_statement(s: ASMStatement<T>) -> Batch<T> {
        Batch {
            statements: vec![s],
        }
    }

    /// Returns true iff this batch consists exclusively of labels
    fn is_only_labels(&self) -> bool {
        self.statements
            .iter()
            .all(|s| matches!(s, ASMStatement::Label(..)))
    }

    /// Returns true iff this batch contains at least one label
    fn contains_labels(&self) -> bool {
        self.statements
            .iter()
            .any(|s| matches!(s, ASMStatement::Label(..)))
    }

    fn try_absorb(&mut self, s: ASMStatement<T>) -> Result<(), (ASMStatement<T>, IncompatibleSet)> {
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
pub struct ASMBatcher<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> ASMBatcher<T> {
    /// split a list of statements into compatible batches
    fn to_compatible_batches(
        &self,
        statements: impl IntoIterator<Item = ASMStatement<T>>,
    ) -> Vec<ASMStatementBatch<T>> {
        statements
            .into_iter()
            .peekable()
            .batching(|it| {
                let mut batch = Batch::default();
                loop {
                    // look at the next statement
                    match it.peek() {
                        // try to add it to this batch
                        Some(new_s) => match batch.try_absorb(new_s.clone()) {
                            Ok(()) => {
                                it.next().unwrap();
                            }
                            Err((_, reason)) => {
                                let res = ASMStatementBatch {
                                    statements: batch.statements,
                                    reason: Some(reason),
                                };
                                break Some(res);
                            }
                        },
                        None => {
                            break match batch.statements.len() {
                                0 => None,
                                _ => Some(ASMStatementBatch {
                                    statements: batch.statements,
                                    reason: None,
                                }),
                            }
                        }
                    }
                }
            })
            .collect()
    }

    pub fn convert(&mut self, asm_file: ASMFile<T>) -> BatchedASMFile<T> {
        let statements = asm_file.0.into_iter().peekable();

        let (declarations, statements) = statements.into_iter().partition(|s| match s {
            ASMStatement::Degree(..)
            | ASMStatement::RegisterDeclaration(..)
            | ASMStatement::InstructionDeclaration(..)
            | ASMStatement::InlinePil(..) => true,
            ASMStatement::Assignment(_, _, _, _)
            | ASMStatement::Instruction(_, _, _)
            | ASMStatement::Label(_, _) => false,
        });

        let batches = self.to_compatible_batches(statements);

        let lines_before = batches.iter().map(ASMStatementBatch::size).sum::<usize>() as f32;
        let lines_after = batches.len() as f32;

        log::debug!(
            "Batching complete with savings of {}% in execution trace lines",
            (1. - lines_after / lines_before) * 100.
        );

        BatchedASMFile {
            declarations,
            batches,
        }
    }
}

#[cfg(test)]
mod tests {

    use std::{fs, path::PathBuf};

    use number::GoldilocksField;
    use pretty_assertions::assert_eq;

    use super::*;

    fn test_batching(path: &str) {
        let base_path = PathBuf::from("../test_data/asm/batching");
        let file_name = base_path.join(path);
        let contents = fs::read_to_string(&file_name).unwrap();
        let batched_asm = parser::parse_asm::<GoldilocksField>(
            Some(file_name.as_os_str().to_str().unwrap()),
            &contents,
        )
        .map(|ast| ASMBatcher::default().convert(ast))
        .unwrap();
        let mut expected_file_name = file_name;
        expected_file_name.set_file_name(format!(
            "{}_batched.asm",
            expected_file_name.file_stem().unwrap().to_str().unwrap()
        ));
        let expected = fs::read_to_string(expected_file_name).unwrap();

        assert_eq!(
            format!("{batched_asm}").replace("\n\n", "\n"),
            expected.replace("\n\n", "\n")
        );
    }

    #[test]
    fn labels() {
        test_batching("labels.asm")
    }
}
