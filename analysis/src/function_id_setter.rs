use std::{iter::repeat, marker::PhantomData};

use ast::asm_analysis::{
    AnalysisASMFile, BatchMetadata, FunctionDefinitionStatement, FunctionStatement, Rom,
};
use number::FieldElement;

pub fn set<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    FunctionIdSetter::default().set(file)
}

#[derive(Default)]
struct FunctionIdSetter<T> {
    marker: PhantomData<T>,
}

impl<T: FieldElement> FunctionIdSetter<T> {
    pub fn set(&mut self, mut asm_file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
        for machine in asm_file.machines.values_mut() {
            // now that the rom is final, we can set the function_id for all exposed functions
            self.set_function_ids(&mut machine.functions, &machine.rom);
        }

        asm_file
    }

    fn set_function_ids(
        &self,
        functions: &mut [FunctionDefinitionStatement<T>],
        rom: &Option<Rom<T>>,
    ) {
        if let Some(rom) = rom {
            let mut optimised_batches;
            let mut default_batches;

            let batches: &mut dyn Iterator<Item = &BatchMetadata> = match &rom.batches {
                Some(batches) => {
                    optimised_batches = batches.iter();
                    &mut optimised_batches
                }
                None => {
                    default_batches = repeat(&BatchMetadata {
                        size: 1,
                        reason: None,
                    })
                    .take(rom.statements.len());
                    &mut default_batches
                }
            };

            let mut statements = rom.statements.iter();

            for (index, batch) in batches.enumerate() {
                let batch_statements = (&mut statements).take(batch.size);

                for statement in batch_statements {
                    if let FunctionStatement::Label(l) = statement {
                        if l.name.starts_with('_') {
                            for f in functions.iter_mut() {
                                if f.name == l.name[1..] {
                                    f.id = Some(T::from(index as u64));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {}
