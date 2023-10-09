use ast::analyzed::{Expression, Identity, IdentityKind, PolyID, PolynomialReference};
use ast::parsed::SelectedExpressions;
use number::FieldElement;
use std::collections::{HashMap, HashSet};

use super::affine_expression::AffineExpression;
use super::column_map::WitnessColumnMap;
use super::identity_processor::Machines;
use super::machines::Machine;
use super::range_constraints::RangeConstraint;

use super::machines::FixedLookup;
use super::rows::{transpose_rows, Row, RowFactory};
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState};

pub struct Generator<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    identities: Vec<&'a Identity<Expression<T>>>,
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
        _right: &'a SelectedExpressions<Expression<T>>,
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
        identities: &[&'a Identity<Expression<T>>],
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
        // For now, run the VM Processor on an empty block of the size of the degree
        // In the future, we'll instantate a processor for each block and then stitch them together here.
        let row_factory = RowFactory::new(self.fixed_data, self.global_range_constraints.clone());
        let default_row = row_factory.fresh_row();
        let data = vec![default_row; self.fixed_data.degree as usize];

        let mut processor = VmProcessor::new(
            self.fixed_data,
            &self.identities,
            self.witnesses.clone(),
            data,
        );
        processor.run(mutable_state);

        self.data = processor.finish();
    }
}
