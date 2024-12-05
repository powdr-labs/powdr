use std::collections::{BTreeMap, BTreeSet};

use fp2::Fp2;
use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_ast::analyzed::{Analyzed, Identity, PhantomBusInteractionIdentity};
use powdr_executor_utils::VariablySizedColumn;
use powdr_number::{DegreeType, FieldElement};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::witgen::evaluators::expression_evaluator::ExpressionEvaluator;

use super::evaluators::expression_evaluator::OwnedTraceValues;

mod fp2;

/// Witness generator for the second-stage bus accumulator.
pub struct BusAccumulatorGenerator<'a, T> {
    pil: &'a Analyzed<T>,
    bus_interactions: Vec<&'a PhantomBusInteractionIdentity<T>>,
    trace_values: OwnedTraceValues<T>,
    powers_of_alpha: Vec<Fp2<T>>,
    beta: Fp2<T>,
}

impl<'a, T: FieldElement> BusAccumulatorGenerator<'a, T> {
    pub fn new(
        pil: &'a Analyzed<T>,
        witness_columns: &'a [(String, Vec<T>)],
        fixed_columns: &'a [(String, VariablySizedColumn<T>)],
        challenges: BTreeMap<u64, T>,
    ) -> Self {
        let size = witness_columns.iter().next().unwrap().1.len() as DegreeType;

        // The provided PIL might only contain a subset of all fixed columns.
        let fixed_column_names = pil
            .constant_polys_in_source_order()
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, _)| name.clone())
            .collect::<BTreeSet<_>>();

        // Select the columns in the current PIL and select the right size.
        let fixed_columns = fixed_columns
            .iter()
            .filter(|(n, _)| fixed_column_names.contains(n))
            .map(|(n, v)| (n.clone(), v.get_by_size(size).unwrap()));

        let trace_values = OwnedTraceValues::new(
            pil,
            witness_columns.to_vec(),
            fixed_columns
                .map(|(name, values)| (name, values.to_vec()))
                .collect(),
        );

        let bus_interactions = pil
            .identities
            .iter()
            .filter_map(|identity| match identity {
                Identity::PhantomBusInteraction(i) => Some(i),
                _ => None,
            })
            .collect::<Vec<_>>();

        let max_tuple_size = bus_interactions
            .iter()
            .map(|i| i.tuple.0.len())
            .max()
            .unwrap();

        let alpha = Fp2::new(challenges[&1], challenges[&2]);
        let beta = Fp2::new(challenges[&3], challenges[&4]);
        let powers_of_alpha = powers_of_alpha(alpha, max_tuple_size);

        Self {
            pil,
            bus_interactions,
            trace_values,
            powers_of_alpha,
            beta,
        }
    }

    pub fn generate(&self) -> Vec<(String, Vec<T>)> {
        let accumulators = self
            .bus_interactions
            .par_iter()
            .flat_map(|bus_interaction| {
                let (acc1, acc2) = self.interaction_columns(bus_interaction);
                let next1 = next(&acc1);
                let next2 = next(&acc2);

                // We assume that the second-stage witness columns are in this order,
                // for each bus interaction.
                [acc1, acc2, next1, next2]
            })
            .collect::<Vec<_>>();

        self.pil
            .committed_polys_in_source_order()
            .filter(|(symbol, _)| symbol.stage == Some(1))
            .flat_map(|(symbol, _)| symbol.array_elements().map(|(name, _)| name))
            .zip_eq(accumulators)
            .collect()
    }

    fn interaction_columns(
        &self,
        bus_interaction: &PhantomBusInteractionIdentity<T>,
    ) -> (Vec<T>, Vec<T>) {
        let intermediate_definitions = self.pil.intermediate_definitions();
        let empty_challenges = BTreeMap::new();

        let size = self.trace_values.height();
        let mut acc1 = vec![T::zero(); size];
        let mut acc2 = vec![T::zero(); size];

        for i in 0..size {
            let mut evaluator = ExpressionEvaluator::new(
                self.trace_values.row(i),
                &intermediate_definitions,
                &empty_challenges,
            );
            let current_acc = if i == 0 {
                Fp2::zero()
            } else {
                Fp2::new(acc1[i - 1], acc2[i - 1])
            };
            let multiplicity = evaluator.evaluate(&bus_interaction.multiplicity);

            let new_acc = match multiplicity.is_zero() {
                true => current_acc,
                false => {
                    let tuple = bus_interaction
                        .tuple
                        .0
                        .iter()
                        .map(|r| evaluator.evaluate(r))
                        .collect::<Vec<_>>();

                    let fingerprint = self.beta - self.fingerprint(&tuple);
                    current_acc + fingerprint.inverse() * multiplicity
                }
            };

            acc1[i] = new_acc.0;
            acc2[i] = new_acc.1;
        }

        (acc1, acc2)
    }

    /// Fingerprints a tuples of field elements, using the pre-computed powers of alpha.
    fn fingerprint(&self, tuple: &[T]) -> Fp2<T> {
        tuple
            .iter()
            .zip_eq(self.powers_of_alpha.iter().take(tuple.len()).rev())
            .map(|(a, b)| (*b) * (*a))
            .sum()
    }
}

/// Given `alpha`, computes [1, alpha, alpha^2, ..., alpha^(n-1)].
fn powers_of_alpha<T: FieldElement>(alpha: Fp2<T>, n: usize) -> Vec<Fp2<T>> {
    (0..n)
        .scan(Fp2::one(), |state, _| {
            let result = *state;
            *state = *state * alpha;
            Some(result)
        })
        .collect::<Vec<_>>()
}

/// Rotates a column to the left.
fn next<T: Clone>(column: &[T]) -> Vec<T> {
    let mut result = column.to_vec();
    result.rotate_left(1);
    result
}
