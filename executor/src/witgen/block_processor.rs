use std::collections::HashSet;

use ast::analyzed::{AlgebraicExpression as Expression, AlgebraicReference, Identity, PolyID};
use number::FieldElement;

use super::{
    data_structures::finalizable_data::FinalizableData,
    processor::{OuterQuery, Processor},
    rows::RowFactory,
    sequence_iterator::{Action, ProcessingSequenceIterator, SequenceStep},
    EvalError, EvalValue, FixedData, IncompleteCause, MutableState, QueryCallback,
};

/// A basic processor that knows how to determine a unique satisfying witness
/// for a given list of identities.
/// The lifetimes mean the following:
/// - `'a`: The duration of the entire witness generation (e.g. references to identities)
/// - `'b`: The duration of this machine's call (e.g. the mutable references of the other machines)
/// - `'c`: The duration of this Processor's lifetime (e.g. the reference to the identity processor)
pub struct BlockProcessor<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> {
    processor: Processor<'a, 'b, 'c, T, Q>,
}

impl<'a, 'b, 'c, T: FieldElement, Q: QueryCallback<T>> BlockProcessor<'a, 'b, 'c, T, Q> {
    pub fn new(
        row_offset: u64,
        data: FinalizableData<'a, T>,
        mutable_state: &'c mut MutableState<'a, 'b, T, Q>,
        identities: &'c [&'a Identity<Expression<T>>],
        fixed_data: &'a FixedData<'a, T>,
        row_factory: RowFactory<'a, T>,
        witness_cols: &'c HashSet<PolyID>,
    ) -> Self {
        let processor = Processor::new(
            row_offset,
            data,
            mutable_state,
            identities,
            fixed_data,
            row_factory,
            witness_cols,
        );
        Self { processor }
    }

    pub fn with_outer_query(
        self,
        outer_query: OuterQuery<'a, T>,
    ) -> BlockProcessor<'a, 'b, 'c, T, Q> {
        let processor = self.processor.with_outer_query(outer_query);
        Self { processor }
    }

    /// Figures out unknown values.
    /// Returns the assignments to outer query columns.
    pub fn solve(
        &mut self,
        sequence_iterator: &mut ProcessingSequenceIterator,
    ) -> Result<EvalValue<&'a AlgebraicReference, T>, EvalError<T>> {
        let mut outer_assignments = vec![];

        while let Some(SequenceStep { row_delta, action }) = sequence_iterator.next() {
            let row_index = (1 + row_delta) as usize;
            let progress = match action {
                Action::InternalIdentity(identity_index) => {
                    self.processor.process_identity(row_index, identity_index)?
                }
                Action::OuterQuery => {
                    let (progress, new_outer_assignments) =
                        self.processor.process_outer_query(row_index)?;
                    outer_assignments.extend(new_outer_assignments);
                    progress
                }
                Action::ProverQueries => self.processor.process_queries(row_index),
            };
            sequence_iterator.report_progress(progress);
        }

        match self.processor.finshed_outer_query() {
            true => Ok(EvalValue::complete(outer_assignments)),
            false => Ok(EvalValue::incomplete_with_constraints(
                outer_assignments,
                IncompleteCause::BlockMachineLookupIncomplete,
            )),
        }
    }

    pub fn finish(self) -> FinalizableData<'a, T> {
        self.processor.finish()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use ast::analyzed::{PolyID, PolynomialType};
    use number::{FieldElement, GoldilocksField};
    use pil_analyzer::analyze_string;

    use crate::{
        constant_evaluator::generate,
        witgen::{
            data_structures::column_map::FixedColumnMap,
            data_structures::finalizable_data::FinalizableData,
            global_constraints::GlobalConstraints,
            identity_processor::Machines,
            machines::FixedLookup,
            rows::RowFactory,
            sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator},
            FixedData, MutableState, QueryCallback,
        },
    };

    use super::BlockProcessor;

    fn name_to_poly_id<T: FieldElement>(fixed_data: &FixedData<T>) -> BTreeMap<String, PolyID> {
        let mut name_to_poly_id = BTreeMap::new();
        for (poly_id, col) in fixed_data.witness_cols.iter() {
            name_to_poly_id.insert(col.poly.name.clone(), poly_id);
        }
        for (poly_id, col) in fixed_data.fixed_cols.iter() {
            name_to_poly_id.insert(col.name.clone(), poly_id);
        }
        name_to_poly_id
    }

    /// Constructs a processor for a given PIL, then calls a function on it.
    fn do_with_processor<T: FieldElement, Q: QueryCallback<T>, R>(
        src: &str,
        mut query_callback: Q,
        f: impl Fn(BlockProcessor<T, Q>, BTreeMap<String, PolyID>, u64, usize) -> R,
    ) -> R {
        let analyzed = analyze_string(src);
        let constants = generate(&analyzed);
        let fixed_data = FixedData::new(&analyzed, &constants, vec![]);

        // No global range constraints
        let global_range_constraints = GlobalConstraints {
            witness_constraints: fixed_data.witness_map_with(None),
            fixed_constraints: FixedColumnMap::new(None, fixed_data.fixed_cols.len()),
        };

        // No submachines
        let mut fixed_lookup = FixedLookup::new(global_range_constraints.clone());
        let mut machines = vec![];

        let row_factory = RowFactory::new(&fixed_data, global_range_constraints);
        let columns = (0..fixed_data.witness_cols.len())
            .map(move |i| PolyID {
                id: i as u64,
                ptype: PolynomialType::Committed,
            })
            .collect();
        let data = FinalizableData::with_initial_rows_in_progress(
            &columns,
            (0..fixed_data.degree).map(|i| row_factory.fresh_row(i)),
        );

        let mut mutable_state = MutableState {
            fixed_lookup: &mut fixed_lookup,
            machines: Machines::from(machines.iter_mut()),
            query_callback: &mut query_callback,
        };
        let row_offset = 0;
        let identities = analyzed.identities.iter().collect::<Vec<_>>();
        let witness_cols = fixed_data.witness_cols.keys().collect();

        let processor = BlockProcessor::new(
            row_offset,
            data,
            &mut mutable_state,
            &identities,
            &fixed_data,
            row_factory,
            &witness_cols,
        );

        f(
            processor,
            name_to_poly_id(&fixed_data),
            analyzed.degree(),
            identities.len(),
        )
    }

    fn solve_and_assert<T: FieldElement>(src: &str, asserted_values: &[(usize, &str, u64)]) {
        let query_callback = |_: &str| -> Option<T> { None };
        do_with_processor(
            src,
            query_callback,
            |mut processor, poly_ids, degree, num_identities| {
                let mut sequence_iterator = ProcessingSequenceIterator::Default(
                    DefaultSequenceIterator::new(degree as usize - 2, num_identities, None),
                );
                let outer_updates = processor.solve(&mut sequence_iterator).unwrap();
                assert!(outer_updates.is_complete());
                assert!(outer_updates.is_empty());

                let data = processor.finish();

                for &(i, name, expected) in asserted_values.iter() {
                    let poly_id = poly_ids[name];
                    let row = &data[i];
                    let actual: T = row[&poly_id].value.unwrap_or_default();
                    assert_eq!(actual, T::from(expected));
                }
            },
        )
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
