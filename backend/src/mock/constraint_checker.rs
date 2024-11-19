use std::{collections::BTreeMap, fmt};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, PolyID, PolynomialIdentity},
    parsed::visitor::AllChildren,
};
use powdr_executor::witgen::{AffineExpression, AlgebraicVariable, ExpressionEvaluator};
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::mock::evaluator::Variables;

pub struct ConstraintChecker<'a, F> {
    machine_name: String,
    size: usize,
    columns: BTreeMap<PolyID, &'a [F]>,
    pil: &'a Analyzed<F>,
    intermediate_definitions: BTreeMap<PolyID, &'a AlgebraicExpression<F>>,
}

impl<'a, F: FieldElement> ConstraintChecker<'a, F> {
    pub fn new(
        machine_name: String,
        witness: &'a [(String, Vec<F>)],
        fixed: &'a [(String, &'a [F])],
        pil: &'a Analyzed<F>,
    ) -> Self {
        let size = witness
            .iter()
            .map(|(_, v)| v.len())
            .chain(fixed.iter().map(|(_, v)| v.len()))
            .unique()
            .exactly_one()
            .unwrap();

        let intermediate_definitions = pil
            .intermediate_polys_in_source_order()
            .flat_map(|(symbol, definitions)| {
                symbol
                    .array_elements()
                    .zip_eq(definitions)
                    .map(|((_, poly_id), def)| (poly_id, def))
            })
            .collect();

        let columns_by_name = witness
            .iter()
            .map(|(name, col)| (name, col.as_slice()))
            .chain(fixed.iter().map(|(name, col)| (name, *col)))
            .collect::<BTreeMap<_, _>>();

        let columns = pil
            .committed_polys_in_source_order()
            .chain(pil.constant_polys_in_source_order())
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, poly_id)| {
                let column = columns_by_name
                    .get(&name)
                    .unwrap_or_else(|| panic!("Missing column: {name}"));
                (poly_id, *column)
            })
            .collect();

        Self {
            machine_name,
            size,
            columns,
            pil,
            intermediate_definitions,
        }
    }

    pub fn check(&self) -> MachineResult<'a, F> {
        // We'd only expect to see polynomial identities here, because we're only validating one machine.
        let mut warnings = Vec::new();
        let polynomial_identities = self
            .pil
            .identities
            .iter()
            .filter_map(|identity| match identity {
                Identity::Polynomial(_) => Some(identity),
                _ => {
                    warnings.push(format!("Unexpected identity: {}", identity));
                    None
                }
            })
            .collect::<Vec<_>>();

        let errors = (0..self.size)
            .into_par_iter()
            .flat_map(|row| self.check_row(row, &polynomial_identities))
            .collect();

        MachineResult {
            machine_name: self.machine_name.clone(),
            warnings,
            errors,
        }
    }

    fn check_row(
        &self,
        row: usize,
        identities: &[&'a Identity<F>],
    ) -> Vec<FailingPolynomialConstraint<'a, F>> {
        let variables = Variables {
            columns: &self.columns,
            row,
        };
        let mut evaluator = ExpressionEvaluator::new(&variables, &self.intermediate_definitions);
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
            write!(f, "\n  {} = {}", variable, value)?;
        }
        Ok(())
    }
}

pub struct MachineResult<'a, F> {
    machine_name: String,
    warnings: Vec<String>,
    errors: Vec<FailingPolynomialConstraint<'a, F>>,
}

const MAX_ERRORS: usize = 5;

impl<F: fmt::Display> MachineResult<'_, F> {
    pub fn log(&self) {
        let num_warnings = self.warnings.len();
        let num_errors = self.errors.len();

        if num_errors == 0 && num_warnings == 0 {
            return;
        }

        let log_level = if num_errors > 0 {
            log::Level::Error
        } else {
            log::Level::Warn
        };

        log::log!(
            log_level,
            "Machine {} has {} errors and {} warnings",
            self.machine_name,
            num_errors,
            num_warnings
        );

        for warning in &self.warnings {
            log::warn!("  Warning: {}", warning);
        }

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

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }
}
