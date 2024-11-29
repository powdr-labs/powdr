use std::{collections::BTreeMap, fmt};

use powdr_ast::{
    analyzed::{Identity, PolynomialIdentity},
    parsed::visitor::AllChildren,
};
use powdr_executor::witgen::{AffineExpression, AlgebraicVariable, ExpressionEvaluator};
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::mock::evaluator::Variables;

use super::machine::Machine;

pub struct PolynomialConstraintChecker<'a, F> {
    machine: &'a Machine<'a, F>,
    challenges: &'a BTreeMap<u64, F>,
}

impl<'a, F: FieldElement> PolynomialConstraintChecker<'a, F> {
    pub fn new(machine: &'a Machine<'a, F>, challenges: &'a BTreeMap<u64, F>) -> Self {
        Self {
            machine,
            challenges,
        }
    }

    pub fn check(&self) -> MachineResult<'a, F> {
        // We'd only expect to see polynomial identities here, because we're only validating one machine.
        // But if they do appear (because of a lookup / permutation within a namespace), they are handled
        // by the ConnectionConstraintChecker.
        let polynomial_identities = self
            .machine
            .pil
            .identities
            .iter()
            .filter(|identity| matches!(identity, Identity::Polynomial(_)))
            .collect::<Vec<_>>();

        let errors = (0..self.machine.size)
            .into_par_iter()
            .flat_map(|row| self.check_row(row, &polynomial_identities))
            .collect();

        let result = MachineResult {
            machine_name: self.machine.machine_name.clone(),
            errors,
        };
        result.log();
        result
    }

    fn check_row(
        &self,
        row: usize,
        identities: &[&'a Identity<F>],
    ) -> Vec<FailingPolynomialConstraint<'a, F>> {
        let variables = Variables {
            machine: self.machine,
            row,
            challenges: self.challenges,
        };
        let mut evaluator =
            ExpressionEvaluator::new(&variables, &self.machine.intermediate_definitions);
        identities
            .iter()
            .filter_map(|identity| {
                let identity = match identity {
                    Identity::Polynomial(polynomial_identity) => polynomial_identity,
                    _ => unreachable!("Unexpected identity: {}", identity),
                };
                let result = evaluator.evaluate(&identity.expression).unwrap();
                let result = match result {
                    AffineExpression::Constant(c) => c,
                    _ => unreachable!("Unexpected result: {:?}", result),
                };

                if result != F::zero() {
                    let used_variables = identity
                        .all_children()
                        .filter_map(|child| child.try_into().ok());
                    Some(FailingPolynomialConstraint {
                        row,
                        identity,
                        assignments: used_variables
                            .map(|variable| (variable, variables.constant_value(variable)))
                            .collect(),
                    })
                } else {
                    None
                }
            })
            .collect()
    }
}

struct FailingPolynomialConstraint<'a, F> {
    row: usize,
    identity: &'a PolynomialIdentity<F>,
    assignments: BTreeMap<AlgebraicVariable<'a>, F>,
}

impl<F: fmt::Display> fmt::Display for FailingPolynomialConstraint<'_, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Identity fails on row {}: {}", self.row, self.identity)?;
        for (variable, value) in &self.assignments {
            write!(f, "\n  {variable} = {value}")?;
        }
        Ok(())
    }
}

pub struct MachineResult<'a, F> {
    machine_name: String,
    errors: Vec<FailingPolynomialConstraint<'a, F>>,
}

const MAX_ERRORS: usize = 5;

impl<F: fmt::Display> MachineResult<'_, F> {
    pub fn log(&self) {
        let num_errors = self.errors.len();

        if num_errors == 0 {
            return;
        }

        log::error!("Machine {} has {num_errors} errors", self.machine_name);

        for error in self.errors.iter().take(MAX_ERRORS) {
            let error_indented = error.to_string().replace("\n", "\n  ");
            log::error!("  Error: {}", error_indented);
        }
        if num_errors > MAX_ERRORS {
            log::error!("  ... and {} more errors", num_errors - MAX_ERRORS);
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
