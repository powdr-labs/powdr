use ast::analyzed::Identity;
use number::FieldElement;

use super::{
    identity_processor::IdentityProcessor,
    rows::{Row, RowFactory, RowPair, RowUpdater, UnknownStrategy},
    sequence_iterator::{
        DefaultSequenceIterator, IdentityInSequence, ProcessingSequenceIterator, SequenceStep,
    },
    EvalError, FixedData,
};

/// A basic processor that knows how to determine a unique satisfying witness
/// for a given list of identities.
/// This current implementation is very rudimentary and only used in the block machine
/// to "fix" the last row. However, in the future we can generalize it to be used
/// for general block machine or VM witness computation.
pub struct Processor<'a, 'b, T: FieldElement> {
    /// The global index of the first row of [Processor::data].
    row_offset: u64,
    /// The rows that are being processed.
    data: Vec<Row<'a, T>>,
    /// The list of identities
    identities: &'b [&'a Identity<T>],
    /// The identity processor
    identity_processor: IdentityProcessor<'a, 'b, T>,
    /// The fixed data (containing information about all columns)
    fixed_data: &'a FixedData<'a, T>,
    /// The row factory
    row_factory: RowFactory<'a, T>,
}

impl<'a, 'b, T: FieldElement> Processor<'a, 'b, T> {
    pub fn new(
        row_offset: u64,
        data: Vec<Row<'a, T>>,
        identity_processor: IdentityProcessor<'a, 'a, T>,
        identities: &'b [&'a Identity<T>],
        fixed_data: &'a FixedData<'a, T>,
        row_factory: RowFactory<'a, T>,
    ) -> Self {
        Self {
            row_offset,
            data,
            identity_processor,
            identities,
            fixed_data,
            row_factory,
        }
    }

    /// Evaluate all identities on all *non-wrapping* row pairs, assuming zero for unknown values.
    /// If any identity was unsatisfied, returns an error.
    pub fn check_constraints(&mut self) -> Result<(), EvalError<T>> {
        for i in 0..(self.data.len() - 1) {
            let row_pair = RowPair::new(
                &self.data[i],
                &self.data[i + 1],
                self.row_offset + i as u64,
                self.fixed_data,
                UnknownStrategy::Zero,
            );
            for identity in self.identities {
                self.identity_processor
                    .process_identity(identity, &row_pair)?;
            }
        }
        Ok(())
    }

    /// Reset the row at the given index to a fresh row.
    pub fn clear_row(&mut self, index: usize) {
        self.data[index] = self.row_factory.fresh_row();
    }

    /// Figures out unknown values, using the default sequence iterator.
    /// Since the default sequence iterator looks at the row before and after
    /// the current block, we assume that these lines are already part of [Self::data]
    /// and set the block size to `self.data.len() - 2`.
    pub fn solve_with_default_sequence_iterator(&mut self) -> Result<(), EvalError<T>> {
        assert!(self.data.len() > 2);
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(self.data.len() - 2, self.identities.len(), None),
        );
        self.solve(&mut sequence_iterator)
    }

    /// Figures out unknown values.
    pub fn solve(
        &mut self,
        sequence_iterator: &mut ProcessingSequenceIterator,
    ) -> Result<(), EvalError<T>> {
        while let Some(SequenceStep {
            row_delta,
            identity,
        }) = sequence_iterator.next()
        {
            match identity {
                IdentityInSequence::Internal(identity_index) => {
                    let row_index = (1 + row_delta) as usize;
                    let progress = self.process_identity(row_index, identity_index)?;
                    sequence_iterator.report_progress(progress);
                }
                IdentityInSequence::OuterQuery => {
                    unimplemented!("Implement outer query")
                }
            }
        }
        Ok(())
    }

    /// Destroys itself, returns the data.
    pub fn finish(self) -> Vec<Row<'a, T>> {
        self.data
    }

    /// Given a row and identity index, computes any updates, applies them and returns
    /// whether any progress was made.
    fn process_identity(
        &mut self,
        row_index: usize,
        identity_index: usize,
    ) -> Result<bool, EvalError<T>> {
        let identity = &self.identities[identity_index];

        // Create row pair
        let global_row_index = self.row_offset + row_index as u64;
        let row_pair = RowPair::new(
            &self.data[row_index],
            &self.data[row_index + 1],
            self.row_offset + row_index as u64,
            self.fixed_data,
            UnknownStrategy::Unknown,
        );

        // Compute updates
        let updates = self
            .identity_processor
            .process_identity(identity, &row_pair)
            .map_err(|e| {
                log::warn!("Error in identity: {identity}");
                log::warn!(
                    "Known values in current row (local: {row_index}, global {global_row_index}):\n{}",
                    self.data[row_index].render_values(false),
                );
                if identity.contains_next_ref() {
                    log::warn!(
                        "Known values in next row (local: {}, global {}):\n{}",
                        row_index + 1,
                        global_row_index + 1,
                        self.data[row_index + 1].render_values(false),
                    );
                }
                e
            })?;

        // Build RowUpdater
        // (a bit complicated, because we need two mutable
        // references to elements of the same vector)
        let (before, after) = self.data.split_at_mut(row_index + 1);
        let current = before.last_mut().unwrap();
        let next = after.first_mut().unwrap();
        let mut row_updater = RowUpdater::new(current, next, self.row_offset + row_index as u64);

        // Apply the updates, return progress
        Ok(row_updater.apply_updates(&updates, || identity.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use ast::analyzed::PolyID;
    use number::{FieldElement, GoldilocksField};
    use pil_analyzer::analyze_string;

    use crate::{
        constant_evaluator::generate,
        witgen::{
            identity_processor::IdentityProcessor, machines::FixedLookup, rows::RowFactory,
            FixedData,
        },
    };

    use super::Processor;

    fn name_to_poly_id<T: FieldElement>(fixed_data: &FixedData<T>) -> BTreeMap<String, PolyID> {
        let mut name_to_poly_id = BTreeMap::new();
        for (poly_id, col) in fixed_data.witness_cols.iter() {
            name_to_poly_id.insert(col.name.clone(), poly_id);
        }
        for (poly_id, col) in fixed_data.fixed_cols.iter() {
            name_to_poly_id.insert(col.name.clone(), poly_id);
        }
        name_to_poly_id
    }

    /// Constructs a processor for a given PIL, then calls a function on it.
    fn do_with_processor<T: FieldElement, R>(
        src: &str,
        f: impl Fn(&mut Processor<T>, BTreeMap<String, PolyID>) -> R,
    ) -> R {
        let analyzed = analyze_string(src);
        let (constants, degree) = generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, degree, &constants);

        // No submachines
        let mut fixed_lookup = FixedLookup::default();
        let mut machines = vec![];

        // No global range constraints
        let global_range_constraints = fixed_data.witness_map_with(None);

        let row_factory = RowFactory::new(&fixed_data, global_range_constraints);
        let data = vec![row_factory.fresh_row(); fixed_data.degree as usize];
        let identity_processor =
            IdentityProcessor::new(&fixed_data, &mut fixed_lookup, &mut machines);
        let row_offset = 0;
        let identities = analyzed.identities.iter().collect::<Vec<_>>();

        let mut processor = Processor::new(
            row_offset,
            data,
            identity_processor,
            &identities,
            &fixed_data,
            row_factory,
        );

        f(&mut processor, name_to_poly_id(&fixed_data))
    }

    fn solve_and_assert<T: FieldElement>(src: &str, asserted_values: &[(usize, &str, u64)]) {
        do_with_processor(src, |processor, poly_ids| {
            processor.solve_with_default_sequence_iterator().unwrap();

            // Can't use processor.finish(), because we don't own it...
            let data = processor.data.clone();

            // In case of any error, this will be useful
            for (i, row) in data.iter().enumerate() {
                println!("{}", row.render(&format!("Row {i}"), true));
            }

            for &(i, name, expected) in asserted_values.iter() {
                let poly_id = poly_ids[name];
                let row = &data[i];
                let actual: T = row[&poly_id].value.unwrap_or_default();
                assert_eq!(actual, T::from(expected));
            }
        })
    }

    #[test]
    fn test_fibonacci() {
        let src = r#"
            constant %N = 8;
            
            namespace Fibonacci(%N);
                col fixed ISFIRST = [1] + [0]*;
                col fixed ISLAST = [0]* + [1];
                col witness x, y;
            
                // Start with 1, 1
                ISFIRST * (y - 1) = 0;
                ISFIRST * (x - 1) = 0;
            
                (1-ISLAST) * (x' - y) = 0;
                (1-ISLAST) * (y' - (x + y)) = 0;
        "#;

        solve_and_assert::<GoldilocksField>(src, &[(7, "Fibonacci.y", 34)]);
    }
}
