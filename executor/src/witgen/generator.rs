use ast::analyzed::{
    Expression, Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions,
};
use number::{DegreeType, FieldElement};
use std::collections::{HashMap, HashSet};

use crate::witgen::rows::CellValue;

use super::affine_expression::AffineExpression;
use super::column_map::WitnessColumnMap;
use super::identity_processor::{IdentityProcessor, Machines};
use super::machines::Machine;
use super::processor::Processor;
use super::range_constraints::RangeConstraint;

use super::machines::FixedLookup;
use super::rows::{transpose_rows, Row, RowFactory};
use super::sequence_iterator::{DefaultSequenceIterator, ProcessingSequenceIterator};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState};

pub struct Generator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<T>>,
    witnesses: HashSet<PolyID>,
    global_range_constraints: WitnessColumnMap<Option<RangeConstraint<T>>>,
    data: Vec<Row<'a, T>>,
    latch: Option<Expression<T>>,
}

impl<'a, T: FieldElement> Machine<'a, T> for Generator<'a, T> {
    fn process_plookup(
        &mut self,
        _fixed_data: &'a FixedData<T>,
        _fixed_lookup: &mut FixedLookup<T>,
        _kind: IdentityKind,
        _left: &[AffineExpression<&'a PolynomialReference, T>],
        _right: &'a SelectedExpressions<T>,
        _machines: Machines<'a, '_, T>,
    ) -> Option<EvalResult<'a, T>> {
        unimplemented!()
    }

    fn take_witness_col_values(&mut self, _fixed_data: &FixedData<T>) -> HashMap<String, Vec<T>> {
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
        identities: &[&'a Identity<T>],
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

    pub fn run<Q>(&mut self, mutable_state: &mut MutableState<'a, T, Q>)
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        let first_row = self.compute_partial_first_row(mutable_state);

        self.data = self.process(first_row, 0, mutable_state);

        if self.data.len() < self.fixed_data.degree as usize + 1 {
            assert!(self.latch.is_some());

            let mut first_row = self.data.pop().unwrap();
            self.set_default_operation_id(&mut first_row);

            self.data.append(&mut self.process(
                first_row,
                self.data.len() as DegreeType,
                mutable_state,
            ));
        }

        self.fix_first_row();
    }

    /// Runs the solver on the row pair (degree - 1, 0) in order to partially compute the first
    /// row from identities like `pc' = (1 - first_step') * <...>`.
    fn compute_partial_first_row<Q>(&self, mutable_state: &mut MutableState<'a, T, Q>) -> Row<'a, T>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        // Use `Processor` + `DefaultSequenceIterator` using a "block size" of 0. Because `Processor`
        // expects `data` to include the row before and after the block, this means we'll run the
        // solver on exactly one row pair.
        // Note that using `Processor` instead of `VmProcessor` is more convenient here because
        // it does not assert that the row is "complete" afterwards (i.e., that all identities
        // are satisfied assuming 0 for unknown values).
        let mut identity_processor = IdentityProcessor::new(
            self.fixed_data,
            &mut mutable_state.fixed_lookup,
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

    /// Sets the "default" operation ID in the given row.
    /// This assumes that there is a fixed column `main.p_line` and a witness column
    /// `main._operation_id`, which will be the case if the PIL has been compiled from
    /// Powdr ASM. By convention, the operation ID is last line of the ROM, i.e., the
    /// largest value in `main.p_line`.
    fn set_default_operation_id(&self, row: &mut Row<'a, T>) {
        let rom_line_column = self.fixed_data.fixed_column_by_name("main.p_line").unwrap();
        let default_operation_id = rom_line_column.values.iter().max().unwrap();
        let operation_id_column = self
            .fixed_data
            .witness_column_by_name("main._operation_id")
            .unwrap();
        assert!(self.witnesses.contains(&operation_id_column.id));

        row[&operation_id_column.id].value = CellValue::Known(*default_operation_id);
    }

    fn process<Q>(
        &self,
        first_row: Row<'a, T>,
        row_offset: DegreeType,
        mutable_state: &mut MutableState<'a, T, Q>,
    ) -> Vec<Row<'a, T>>
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        log::trace!(
            "Running main machine from row {row_offset} with the following initial values: {}",
            first_row.render_values(false, None)
        );
        let data = vec![first_row];
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let mut processor = VmProcessor::new(
            row_offset,
            self.fixed_data,
            &self.identities,
            self.witnesses.clone(),
            data,
            row_factory,
            self.latch.clone(),
        );
        processor.run(mutable_state);
        processor.finish()
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
