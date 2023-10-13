use ast::analyzed::{Expression, Identity, IdentityKind, PolyID, PolynomialReference};
use ast::parsed::SelectedExpressions;
use number::{DegreeType, FieldElement};
use std::collections::{HashMap, HashSet};

use crate::witgen::rows::CellValue;
use crate::witgen::{EvalStatus, EvalValue, IncompleteCause};

use super::affine_expression::AffineExpression;
use super::column_map::WitnessColumnMap;
use super::identity_processor::{IdentityProcessor, Machines};
use super::machines::Machine;
use super::processor::{OuterQuery, Processor};
use super::range_constraints::RangeConstraint;

use super::machines::FixedLookup;
use super::rows::{transpose_rows, Row, RowFactory};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{Constraints, EvalResult, FixedData, MutableState, QueryCallback};

pub struct Generator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<Expression<T>>>,
    witnesses: HashSet<PolyID>,
    global_range_constraints: WitnessColumnMap<Option<RangeConstraint<T>>>,
    data: Vec<Row<'a, T>>,
    latch: Option<Expression<T>>,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn process_plookup(
        &mut self,
        fixed_lookup: &mut FixedLookup<T>,
        _kind: IdentityKind,
        left: &[AffineExpression<&'a PolynomialReference, T>],
        right: &'a SelectedExpressions<Expression<T>>,
        _machines: Machines<'a, '_, T>,
    ) -> Option<EvalResult<'a, T>> {
        if right.selector == self.latch {
            log::trace!("Start processing secondary VM '{}'", self.name());
            log::trace!("Left values of lookup:");
            for l in left {
                log::trace!("  {}", l);
            }

            // TODO: Pass machines and query callback
            let mut mutable_state: MutableState<_, fn(&str) -> Option<_>> = MutableState {
                fixed_lookup,
                machines: vec![],
                query_callback: None,
            };

            let first_row = self
                .data
                .last()
                .cloned()
                .unwrap_or_else(|| self.compute_partial_first_row(&mut mutable_state));

            let outer_query = OuterQuery {
                left: left.to_vec(),
                right,
            };
            let (outer_assignments, (mut block, success)) =
                self.process(vec![first_row], 0, &mut mutable_state, Some(outer_query));

            if success {
                self.data.pop();
                self.data.append(&mut block);
                let eval_value = EvalValue {
                    constraints: outer_assignments,
                    status: EvalStatus::Complete,
                };
                log::trace!("End processing VM '{}' (successfully)", self.name());
                Some(Ok(eval_value))
            } else {
                let eval_value = EvalValue {
                    constraints: outer_assignments,
                    status: EvalStatus::Incomplete(IncompleteCause::BlockMachineLookupIncomplete),
                };
                log::trace!("End processing VM '{}' (incomplete)", self.name());
                Some(Ok(eval_value))
            }
        } else {
            None
        }
    }

    fn finalize<'b>(
        &mut self,
        fixed_lookup: &'b mut FixedLookup<T>,
        _machines: Machines<'a, 'b, T>,
    ) {
        // TODO: Pass machines and query callback
        let mut mutable_state: MutableState<_, fn(&str) -> Option<_>> = MutableState {
            fixed_lookup,
            machines: vec![],
            query_callback: None,
        };
        self.fill_remaining_rows(&mut mutable_state);

        self.fix_first_row();
    }

    fn take_witness_col_values(&mut self) -> HashMap<String, Vec<T>> {
        transpose_rows(std::mem::take(&mut self.data), &self.witnesses)
            .into_iter()
            .map(|(id, values)| {
                (
                    self.fixed_data.column_name(&id).to_string(),
                    values.into_iter().map(|v| v.unwrap_or_default()).collect(),
                )
            })
            .collect()
    }
}

impl<'a, T: FieldElement> Generator<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        identities: &[&'a Identity<Expression<T>>],
        witnesses: HashSet<PolyID>,
        global_range_constraints: &WitnessColumnMap<Option<RangeConstraint<T>>>,
        latch: Option<Expression<T>>,
    ) -> Self {
        Self {
            fixed_data,
            identities: identities.to_vec(),
            witnesses,
            global_range_constraints: global_range_constraints.clone(),
            data: vec![],
            latch,
        }
    }

    fn name(&self) -> &str {
        let first_witness = self.witnesses.iter().next().unwrap();
        let first_witness_name = self.fixed_data.column_name(first_witness);
        let namespace = first_witness_name
            .rfind('.')
            .map(|idx| &first_witness_name[..idx]);
        if let Some(namespace) = namespace {
            namespace
        } else {
            // For machines compiled using Powdr ASM we'll always have a namespace, but as a last
            // resort we'll use the first witness name.
            first_witness_name
        }
    }

    pub fn run<Q: QueryCallback<T>>(&mut self, mutable_state: &mut MutableState<'a, '_, T, Q>) {
        let first_row = self.compute_partial_first_row(mutable_state);
        self.data = self.process(vec![first_row], 0, mutable_state, None).1 .0;

        log::debug!("Finalizing main Machine");
        let machines = mutable_state.machines.iter_mut().into();
        self.finalize(mutable_state.fixed_lookup, machines);

        let mut machines: Machines<T> = mutable_state.machines.iter_mut().into();
        for i in 0..machines.len() {
            log::debug!(
                "Finalizing secondary machine {} / {}",
                i + 1,
                machines.len()
            );
            let (current, others) = machines.split(i);
            current.finalize(mutable_state.fixed_lookup, others)
        }
    }

    fn fill_remaining_rows<Q>(&mut self, mutable_state: &mut MutableState<'a, '_, T, Q>)
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        if self.data.len() < self.fixed_data.degree as usize + 1 {
            assert!(self.latch.is_some());

            let first_row = self.data.pop().unwrap();

            self.data.append(
                &mut self
                    .process(
                        vec![first_row],
                        self.data.len() as DegreeType,
                        mutable_state,
                        None,
                    )
                    .1
                     .0,
            );
        }
    }

    /// Runs the solver on the row pair (degree - 1, 0) in order to partially compute the first
    /// row from identities like `pc' = (1 - first_step') * <...>`.
    fn compute_partial_first_row<Q: QueryCallback<T>>(
        &self,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
    ) -> Row<'a, T> {
        // Use `Processor` + `DefaultSequenceIterator` using a "block size" of 0. Because `Processor`
        // expects `data` to include the row before and after the block, this means we'll run the
        // solver on exactly one row pair.
        // Note that using `Processor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            mutable_state.fixed_lookup,
            mutable_state.machines.iter_mut().into(),
        );
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let data = vec![row_factory.fresh_row(); 2];
        let mut processor = Processor::new(
            self.fixed_data.degree - 1,
            data,
            &mut identity_processor,
            &self.identities,
            self.fixed_data,
            row_factory,
            &self.witnesses,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, self.identities.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();
        let first_row = processor.finish().remove(1);

        first_row
    }

    fn process<Q: QueryCallback<T>>(
        &self,
        data: Vec<Row<'a, T>>,
        row_offset: DegreeType,
        mutable_state: &mut MutableState<'a, '_, T, Q>,
        outer_query: Option<OuterQuery<'a, T>>,
    ) -> (
        Constraints<&'a PolynomialReference, T>,
        (Vec<Row<'a, T>>, bool),
    ) {
        log::trace!(
            "Running machine {} from row {row_offset} with the following initial values:",
            self.name(),
        );
        for (i, row) in data.iter().enumerate() {
            log::trace!("  Row {i}:\n{}", row.render_values(false, None));
        }
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let mut processor = VmProcessor::new(
            row_offset,
            self.fixed_data,
            &self.identities,
            self.witnesses.clone(),
            data,
            row_factory,
            self.latch.clone(),
            outer_query,
        );
        let outer_assignments = processor.run(mutable_state);
        (outer_assignments, processor.finish())
    }

    /// At the end of the solving algorithm, we'll have computed the first row twice
    /// (as row 0 and as row <degree>). This function merges the two versions.
    fn fix_first_row(&mut self) {
        assert_eq!(self.data.len() as DegreeType, self.fixed_data.degree + 1);

        let last_row = self.data.pop().unwrap();
        self.data[0] = WitnessColumnMap::from(self.data[0].values().zip(last_row.values()).map(
            |(cell1, cell2)| match (&cell1.value, &cell2.value) {
                (CellValue::Known(v1), CellValue::Known(v2)) => {
                    assert_eq!(v1, v2);
                    cell1.clone()
                }
                (CellValue::Known(_), _) => cell1.clone(),
                _ => cell2.clone(),
            },
        ));
    }
}
