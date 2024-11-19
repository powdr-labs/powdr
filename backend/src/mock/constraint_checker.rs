use std::collections::BTreeMap;

use itertools::Itertools;
use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, PolyID, PolynomialType},
    parsed::visitor::AllChildren,
};
use powdr_executor::witgen::{
    AffineExpression, AffineResult, AlgebraicVariable, ExpressionEvaluator, SymbolicVariables,
};
use powdr_number::FieldElement;

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

        let column_names = pil
            .committed_polys_in_source_order()
            .chain(pil.constant_polys_in_source_order())
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, poly_id)| (poly_id, name))
            .collect::<BTreeMap<_, _>>();

        let columns = column_names
            .iter()
            .map(|(poly_id, name)| {
                let column = columns_by_name
                    .get(&name)
                    .unwrap_or_else(|| panic!("Missing column: {name}"));
                (*poly_id, *column)
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

    pub fn check(&self) {
        log::info!("Checking machine: {}", self.machine_name);

        for row in 0..self.size {
            let variables = Variables {
                columns: &self.columns,
                row,
            };
            let mut evaluator =
                ExpressionEvaluator::new(&variables, &self.intermediate_definitions);
            for identity in self.pil.identities.iter() {
                match identity {
                    Identity::Polynomial(polynomial_identity) => {
                        let result = evaluator.evaluate(&polynomial_identity.expression).unwrap();
                        let result = match result {
                            AffineExpression::Constant(c) => c,
                            _ => unreachable!("Unexpected result: {:?}", result),
                        };

                        if result != F::zero() {
                            log::error!("Identity failed at row {}: {}", row, identity);
                            let used_variables =
                                identity.all_children().filter_map(|child| match child {
                                    AlgebraicExpression::Reference(algebraic_ref) => {
                                        Some(AlgebraicVariable::Column(algebraic_ref))
                                    }
                                    AlgebraicExpression::PublicReference(public) => {
                                        Some(AlgebraicVariable::Public(public))
                                    }
                                    _ => None,
                                });
                            for variable in used_variables {
                                let value = variables.constant_value(variable);
                                log::error!("  {} = {}", variable, value);
                            }
                        }
                    }
                    _ => unreachable!("Unexpected identity: {}", identity),
                }
            }
        }
    }
}

struct Variables<'a, F> {
    columns: &'a BTreeMap<PolyID, &'a [F]>,
    row: usize,
}

impl<'a, F: FieldElement> Variables<'a, F> {
    fn constant_value(&self, var: AlgebraicVariable) -> F {
        match var {
            AlgebraicVariable::Column(column) => match column.poly_id.ptype {
                PolynomialType::Committed | PolynomialType::Constant => {
                    let column_values = self.columns.get(&column.poly_id).unwrap();
                    let row = (self.row + column.next as usize) % column_values.len();
                    column_values[row]
                }
                PolynomialType::Intermediate => unreachable!(
                    "Intermediate polynomials should have been handled by ExpressionEvaluator"
                ),
            },
            AlgebraicVariable::Public(_) => todo!(),
        }
    }
}

impl<'a, F: FieldElement> SymbolicVariables<F> for &Variables<'a, F> {
    fn value<'b>(&self, var: AlgebraicVariable<'b>) -> AffineResult<AlgebraicVariable<'b>, F> {
        Ok(self.constant_value(var).into())
    }
}
