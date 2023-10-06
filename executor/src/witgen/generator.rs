use ast::analyzed::{Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions};
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
    ) -> Self {
        Self {
            fixed_data,
            identities: identities.to_vec(),
            witnesses,
            global_range_constraints: global_range_constraints.clone(),
            data: vec![],
        }
    }

    pub fn run<Q>(&mut self, mutable_state: &mut MutableState<'a, T, Q>)
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        // For identities like `pc' = (1 - first_step') * <...>`, we need to process the last
        // row before processing the first row.
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
            &self.witnesses,
        );
        let mut sequence_iterator = ProcessingSequenceIterator::Default(
            DefaultSequenceIterator::new(0, self.identities.len(), None),
        );
        processor.solve(&mut sequence_iterator).unwrap();
        let first_row = processor.finish().remove(1);

        log::trace!("{}", first_row.render("first row", false, &self.witnesses));

        let data = vec![first_row];

        let mut processor = VmProcessor::new(
            self.fixed_data,
            &self.identities,
            self.witnesses.clone(),
            data,
            row_factory,
        );
        processor.run(mutable_state);

        let mut data = processor.finish();

        if data.len() as DegreeType == self.fixed_data.degree + 1 {
            let last_row = data.pop().unwrap();
            data[0] = WitnessColumnMap::from(data[0].values().zip(last_row.values()).map(
                |(cell1, cell2)| match cell1.value {
                    CellValue::Known(_) => cell1.clone(),
                    _ => cell2.clone(),
                },
            ))
        }

        assert_eq!(data.len(), self.fixed_data.degree as usize);

        self.data = data;
    }
}
