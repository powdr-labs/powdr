use std::{
    collections::{BTreeMap, HashMap},
    iter::once,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{AlgebraicExpression, PolyID, PolynomialType, SelectedExpressions},
    parsed::visitor::AllChildren,
};
use powdr_executor_utils::expression_evaluator::{ExpressionEvaluator, OwnedTerminalValues};
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::witgen::{
    data_structures::identity::{convert_identities, Identity},
    machines::profiling::{record_end, record_start},
};

use super::{util::try_to_simple_poly, FixedData};

static MULTIPLICITY_WITGEN_NAME: &str = "multiplicity witgen";

pub struct MultiplicityColumnGenerator<'a, T: FieldElement> {
    fixed: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> MultiplicityColumnGenerator<'a, T> {
    pub fn new(fixed: &'a FixedData<'a, T>) -> Self {
        Self { fixed }
    }

    /// Takes a map of witness columns and extends it with the multiplicity columns
    /// referenced by bus sends with a non-binary multiplicity.
    pub fn generate(
        &self,
        witness_columns: HashMap<String, Vec<T>>,
        publics: BTreeMap<String, Option<T>>,
    ) -> HashMap<String, Vec<T>> {
        record_start(MULTIPLICITY_WITGEN_NAME);

        let (identities, _) = convert_identities(self.fixed.analyzed);

        let all_columns = witness_columns
            .into_iter()
            .map(|(name, col)| {
                let poly_id = self.fixed.try_column_by_name(&name).unwrap();
                (poly_id, col)
            })
            .chain(
                self.fixed
                    .fixed_cols
                    .iter()
                    // TODO: Avoid clone
                    // TODO: Find the actual size
                    .map(|(poly_id, fixed_col)| (poly_id, fixed_col.values_max_size().to_vec())),
            )
            .collect::<BTreeMap<_, _>>();
        let terminal_values = OwnedTerminalValues {
            trace: all_columns,
            public_values: publics
                .into_iter()
                // Publics might be unavailable if they are later-stage publics.
                .filter_map(|(k, v)| v.map(|v| (k, v)))
                .collect(),
            challenge_values: self.fixed.challenges.clone(),
        };

        // Index all bus receives with arbitrary multiplicity.
        let receive_infos = self
            .fixed
            .bus_receives
            .iter()
            .filter(|(_, bus_receive)| {
                bus_receive.has_arbitrary_multiplicity() && bus_receive.multiplicity.is_some()
            })
            .map(|(bus_id, bus_receive)| {
                let SelectedExpressions {
                    selector,
                    expressions,
                } = &bus_receive.selected_payload;
                let (size, rhs_tuples) = self.get_tuples(
                    &terminal_values,
                    selector,
                    &expressions.iter().collect::<Vec<_>>(),
                );

                let values_to_index = rhs_tuples
                    .into_iter()
                    .map(|(i, tuple)| {
                        // There might be multiple identical rows, but it's fine, we can pick any.
                        (tuple, i)
                    })
                    .collect::<HashMap<_, _>>();

                let multiplicity = bus_receive.multiplicity.as_ref().unwrap();
                (
                    *bus_id,
                    ReceiveInfo {
                        multiplicity_column: try_to_simple_poly(multiplicity)
                            .unwrap_or_else(|| {
                                panic!("Expected simple reference, got: {multiplicity}")
                            })
                            .poly_id,
                        size,
                        values_to_index,
                    },
                )
            })
            .collect::<BTreeMap<_, _>>();

        // A map from multiplicity column ID to the vector of multiplicities.
        let mut multiplicity_columns = receive_infos
            .values()
            .map(|info| (info.multiplicity_column, vec![0; info.size]))
            .collect::<BTreeMap<_, _>>();

        // Increment multiplicities for all bus sends.
        let bus_sends = identities.iter().filter_map(|i| match i {
            Identity::BusSend(bus_send) => match bus_send.bus_id() {
                // As a performance optimization, already filter out sends with a static
                // bus ID for which we know we don't need to track multiplicities.
                Some(bus_id) => receive_infos.get(&bus_id).map(|_| bus_send),
                // For dynamic sends, this optimization is not possible.
                None => Some(bus_send),
            },
            _ => None,
        });

        for bus_send in bus_sends {
            let SelectedExpressions {
                selector,
                expressions,
            } = &bus_send.selected_payload;

            // We need to evaluate both the bus_id (to know the run-time receive) and the expressions
            let bus_id_and_expressions = once(&bus_send.bus_id)
                .chain(expressions.iter())
                .collect::<Vec<_>>();

            let (_, bus_id_and_expressions) =
                self.get_tuples(&terminal_values, selector, &bus_id_and_expressions);

            // Looking up the index is slow, so we do it in parallel.
            let columns_and_indices = bus_id_and_expressions
                .into_par_iter()
                .filter_map(|(_, bus_id_and_expressions)| {
                    receive_infos
                        .get(&bus_id_and_expressions[0])
                        .map(|receive_info| {
                            (
                                receive_info.multiplicity_column,
                                receive_info.values_to_index[&bus_id_and_expressions[1..]],
                            )
                        })
                })
                .collect::<Vec<_>>();

            for (multiplicity_column, index) in columns_and_indices {
                multiplicity_columns.get_mut(&multiplicity_column).unwrap()[index] += 1;
            }
        }

        let columns = terminal_values
            .into_trace()
            .into_iter()
            .chain(
                multiplicity_columns
                    .into_iter()
                    .map(|(col, values)| (col, values.into_iter().map(T::from).collect())),
            )
            .map(|(poly_id, values)| (self.fixed.column_name(&poly_id).to_string(), values))
            .collect();

        record_end(MULTIPLICITY_WITGEN_NAME);
        columns
    }

    /// Given the trace, global values and a selected expression, evaluates the expression
    /// on all rows and returns the number of rows and the selected tuples with their original
    /// row index.
    fn get_tuples(
        &self,
        terminal_values: &OwnedTerminalValues<T>,
        selector: &AlgebraicExpression<T>,
        expressions: &[&AlgebraicExpression<T>],
    ) -> (usize, Vec<(usize, Vec<T>)>) {
        let machine_size = expressions
            .iter()
            .flat_map(|e| e.all_children())
            .chain(selector.all_children())
            .filter_map(|expr| match expr {
                AlgebraicExpression::Reference(ref r) => match r.poly_id.ptype {
                    PolynomialType::Committed | PolynomialType::Constant => {
                        Some(terminal_values.column_length(&r.poly_id))
                    }
                    _ => None,
                },
                _ => None,
            })
            // We add fixed columns in their max size, so they might not be equal...
            // But in practice, either the machine has a (smaller) witness column, or
            // it's a fixed lookup, so there is only one size.
            .min()
            .unwrap_or_else(|| {
                panic!(
                    "No column references found: {selector} $ [{}]",
                    expressions.iter().map(ToString::to_string).join(", ")
                )
            });

        let tuples = (0..machine_size)
            .into_par_iter()
            .filter_map(|row| {
                let mut evaluator = ExpressionEvaluator::new(
                    terminal_values.row(row),
                    &self.fixed.intermediate_definitions,
                );
                let selector = evaluator.evaluate(selector);

                assert!(
                    selector.is_zero() || selector.is_one(),
                    "Non-binary selector"
                );
                selector.is_one().then(|| {
                    (
                        row,
                        expressions
                            .iter()
                            .map(|expression| evaluator.evaluate(expression))
                            .collect::<Vec<_>>(),
                    )
                })
            })
            .collect::<Vec<_>>();

        (machine_size, tuples)
    }
}

struct ReceiveInfo<T> {
    multiplicity_column: PolyID,
    size: usize,
    /// Maps a tuple of values to its index in the trace.
    values_to_index: HashMap<Vec<T>, usize>,
}
