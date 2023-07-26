use ast::analyzed::{Expression, Identity, IdentityKind, PolynomialReference};
use number::FieldElement;

use super::{
    affine_expression::AffineResult,
    expression_evaluator::ExpressionEvaluator,
    machines::{FixedLookup, Machine},
    rows::{RowPair, UpdateStatus},
    symbolic_witness_evaluator::SymoblicWitnessEvaluator,
    EvalError, EvalStatus, FixedData, IncompleteCause,
};

pub struct IdentityProcessor<'a, T: FieldElement> {
    pub fixed_data: &'a FixedData<'a, T>,
    fixed_lookup: &'a mut FixedLookup<T>,
    pub machines: Vec<Box<dyn Machine<T>>>,
}

impl<'a, T: FieldElement> IdentityProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_lookup: &'a mut FixedLookup<T>,
        machines: Vec<Box<dyn Machine<T>>>,
    ) -> Self {
        Self {
            fixed_data,
            fixed_lookup,
            machines,
        }
    }

    /// Given an identity and a row pair, tries to figure out additional values / range constraints
    /// for the given cells.
    /// Fails if any constraint was not satisfied.
    /// Returns whether any progress was made and the new status of the identity.
    pub fn process_identity(
        &mut self,
        identity: &'a Identity<T>,
        rows: &mut RowPair<T>,
    ) -> Result<UpdateStatus, EvalError<T>> {
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

    fn process_polynomial_identity(
        &self,
        identity: &'a Identity<T>,
        rows: &mut RowPair<T>,
    ) -> Result<UpdateStatus<'a>, EvalError<T>> {
        let expression = identity.expression_for_poly_id();
        let evaluated = match self.evaluate(expression, rows) {
            Err(inclomplete_cause) => return Ok(inclomplete_cause.into()),
            Ok(evaluated) => evaluated,
        };

        let evaluation_value = evaluated.solve_with_range_constraints(rows)?;

        Ok(rows.process_eval_value(evaluation_value, || format!("{}", identity)))
    }

    fn process_plookup(
        &mut self,
        identity: &'a Identity<T>,
        rows: &mut RowPair<T>,
    ) -> Result<UpdateStatus<'a>, EvalError<T>> {
        if let Some(left_selector) = &identity.left.selector {
            if let Some(status) = self.handle_left_selector(left_selector, rows) {
                return Ok(status);
            }
        }

        let left = identity
            .left
            .expressions
            .iter()
            .map(|e| self.evaluate(e, rows))
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
            return Ok(rows.process_eval_value(result?, || format!("{}", identity)));
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
                return Ok(rows.process_eval_value(result?, || format!("{}", identity)));
            }
        }

        unimplemented!("No executor machine matched identity `{identity}`")
    }

    fn handle_left_selector(
        &mut self,
        left_selector: &'a Expression<T>,
        rows: &mut RowPair<T>,
    ) -> Option<UpdateStatus<'a>> {
        let value = match self.evaluate(left_selector, rows) {
            Err(inclomplete_cause) => return Some(inclomplete_cause.into()),
            Ok(value) => value,
        };
        match value.constant_value() {
            Some(v) if v.is_zero() => Some(UpdateStatus {
                progress: false,
                eval_status: EvalStatus::Complete,
            }),
            Some(v) if v.is_one() => None,
            _ => Some(IncompleteCause::NonConstantLeftSelector.into()),
        }
    }

    /// Tries to evaluate the expression to an expression affine in the witness polynomials,
    /// taking current values of polynomials into account.
    /// @returns an expression affine in the witness polynomials
    fn evaluate<'b>(
        &self,
        expr: &'b Expression<T>,
        rows: &RowPair<T>,
    ) -> AffineResult<&'b PolynomialReference, T> {
        ExpressionEvaluator::new(SymoblicWitnessEvaluator::new(
            self.fixed_data,
            rows.current_row_index,
            rows,
        ))
        .evaluate(expr)
    }
}
