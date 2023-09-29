use ast::analyzed::{Identity, IdentityKind, PolyID, PolynomialReference, SelectedExpressions};
use number::FieldElement;
use std::collections::{HashMap, HashSet};

use super::affine_expression::AffineExpression;
use super::column_map::WitnessColumnMap;
use super::identity_processor::Machines;
use super::machines::Machine;
use super::range_constraints::RangeConstraint;

use super::machines::FixedLookup;
use super::rows::transpose_rows;
use super::vm_processor::VmProcessor;
use super::{EvalResult, FixedData, MutableState};

pub struct Generator<'a, T: FieldElement> {
    processor: VmProcessor<'a, T>,
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
        let data = transpose_rows(self.processor.finish(), &self.processor.witnesses);
        data.into_iter()
            .map(|(id, values)| {
                (
                    self.processor.fixed_data.column_name(&id).to_string(),
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
        Generator {
            processor: VmProcessor::new(
                fixed_data,
                identities,
                witnesses,
                global_range_constraints,
                None,
            ),
        }
    }

    pub fn run<Q>(&mut self, mutable_state: &mut MutableState<'a, T, Q>)
    where
        Q: FnMut(&str) -> Option<T> + Send + Sync,
    {
        self.processor.run(mutable_state);
    }
}
