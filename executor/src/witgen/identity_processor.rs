use ast::analyzed::{Expression, Identity, IdentityKind, PolynomialReference};
use number::FieldElement;

use super::{
    machines::{FixedLookup, KnownMachine, Machine},
    rows::RowPair,
    EvalResult, EvalValue, FixedData, IncompleteCause,
};

/// Computes (value or range constraint) updates given a [RowPair] and [Identity].
pub struct IdentityProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    fixed_lookup: &'a mut FixedLookup<T>,
    pub machines: Vec<KnownMachine<T>>,
}

impl<'a, T: FieldElement> IdentityProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_lookup: &'a mut FixedLookup<T>,
        machines: Vec<KnownMachine<T>>,
    ) -> Self {
        Self {
            fixed_data,
            fixed_lookup,
            machines,
        }
    }

    /// Given an identity and a row pair, tries to figure out additional values / range constraints
    /// for the given cells.
    /// Fails if any constraint was not satisfiable.
    /// Returns the updates.
    pub fn process_identity<'b>(
        &mut self,
        identity: &'b Identity<T>,
        rows: &RowPair<T>,
    ) -> EvalResult<'b, T> {
        match identity.kind {
            IdentityKind::Polynomial => self.process_polynomial_identity(identity, rows),
            IdentityKind::Plookup | IdentityKind::Permutation => {
                self.process_plookup(identity, rows)
            }
            kind => {
                unimplemented!(
                    "Identity of kind {kind:?} is not supported by the identity processor."
                )
            }
        }
    }

    fn process_polynomial_identity<'b>(
        &self,
        identity: &'b Identity<T>,
        rows: &RowPair<T>,
    ) -> EvalResult<'b, T> {
        let expression = identity.expression_for_poly_id();
        let evaluated = match rows.evaluate(expression) {
            Err(inclomplete_cause) => return Ok(EvalValue::incomplete(inclomplete_cause)),
            Ok(evaluated) => evaluated,
        };

        evaluated.solve_with_range_constraints(rows)
    }

    fn process_plookup<'b>(
        &mut self,
        identity: &'b Identity<T>,
        rows: &RowPair<T>,
    ) -> EvalResult<'b, T> {
        if let Some(left_selector) = &identity.left.selector {
            if let Some(status) = self.handle_left_selector(left_selector, rows) {
                return Ok(status);
            }
        }

        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| rows.evaluate(e))
            .collect::<Vec<_>>();

        // Now query the machines.
        // Note that we should always query all machines that match, because they might
        // update their internal data, even if all values are already known.
        // TODO could it be that multiple machines match?

        // query the fixed lookup "machine"
        if let Some(result) = self.fixed_lookup.process_plookup(
            self.fixed_data,
            identity.kind,
            &left,
            &identity.right,
        ) {
            return result;
        }

        for m in &mut self.machines {
            // TODO also consider the reasons above.
            if let Some(result) = m.process_plookup(
                self.fixed_data,
                self.fixed_lookup,
                identity.kind,
                &left,
                &identity.right,
            ) {
                return result;
            }
        }

        unimplemented!("No executor machine matched identity `{identity}`")
    }

    /// Returns updates of the left selector cannot be evaluated to 1, otherwise None.
    fn handle_left_selector<'b>(
        &mut self,
        left_selector: &'b Expression<T>,
        rows: &RowPair<T>,
    ) -> Option<EvalValue<&'b PolynomialReference, T>> {
        let value = match rows.evaluate(left_selector) {
            Err(inclomplete_cause) => return Some(EvalValue::incomplete(inclomplete_cause)),
            Ok(value) => value,
        };
        match value.constant_value() {
            Some(v) if v.is_zero() => Some(EvalValue::complete(vec![])),
            Some(v) if v.is_one() => None,
            _ => Some(EvalValue::incomplete(
                IncompleteCause::NonConstantLeftSelector,
            )),
        }
    }
}
