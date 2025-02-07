use std::{
    collections::{BTreeMap, BTreeSet},
    iter::once,
};

use extension_field::ExtensionField;
use fp2::Fp2;
use fp4::Fp4;
use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicExpression, Analyzed, Identity, PhantomBusInteractionIdentity, PolyID,
};
use powdr_executor_utils::{
    expression_evaluator::{ExpressionEvaluator, OwnedTerminalValues},
    VariablySizedColumn,
};
use powdr_number::{DegreeType, FieldElement, KnownField};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

mod extension_field;
mod fp2;
mod fp4;

pub type InteractionColumns<T> = (Vec<Vec<T>>, Vec<Vec<T>>, Vec<Vec<T>>);

/// Generates the second-stage columns for the bus accumulator.
pub fn generate_bus_accumulator_columns<'a, T>(
    pil: &'a Analyzed<T>,
    witness_columns: &'a [(String, Vec<T>)],
    fixed_columns: &'a [(String, VariablySizedColumn<T>)],
    challenges: BTreeMap<u64, T>,
) -> Vec<(String, Vec<T>)>
where
    T: FieldElement,
{
    match T::known_field().unwrap() {
        KnownField::GoldilocksField => BusAccumulatorGenerator::<T, Fp2<T>>::new(
            pil,
            witness_columns,
            fixed_columns,
            challenges,
        )
        .generate(),
        KnownField::BabyBearField | KnownField::KoalaBearField | KnownField::Mersenne31Field => {
            BusAccumulatorGenerator::<T, Fp4<T>>::new(
                pil,
                witness_columns,
                fixed_columns,
                challenges,
            )
            .generate()
        }
        KnownField::Bn254Field => unimplemented!(),
    }
}

/// Witness generator for the second-stage bus accumulator.
struct BusAccumulatorGenerator<'a, T, Ext> {
    pil: &'a Analyzed<T>,
    bus_interactions: Vec<&'a PhantomBusInteractionIdentity<T>>,
    values: OwnedTerminalValues<T>,
    powers_of_alpha: Vec<Ext>,
    beta: Ext,
}

impl<'a, T: FieldElement, Ext: ExtensionField<T> + Sync> BusAccumulatorGenerator<'a, T, Ext> {
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
            // Maximum length of the payload + bus ID
            .map(|i| i.payload.0.len() + 1)
            .max()
            .unwrap();

        let alpha = Ext::get_challenge(&challenges, 0);
        let beta = Ext::get_challenge(&challenges, 1);
        let powers_of_alpha = powers_of_alpha(alpha, max_tuple_size);

        let values = OwnedTerminalValues::new(
            pil,
            witness_columns.to_vec(),
            fixed_columns
                .map(|(name, values)| (name, values.to_vec()))
                .collect(),
        )
        .with_challenges(challenges);

        Self {
            pil,
            bus_interactions,
            values,
            powers_of_alpha,
            beta,
        }
    }

    pub fn generate(&self) -> Vec<(String, Vec<T>)> {
        // First, collect all (PolyID, Vec<T>) pairs from all bus interactions.
        let mut columns: BTreeMap<PolyID, Vec<T>> = self
            .bus_interactions
            .par_iter()
            .flat_map(|bus_interaction| {
                let (folded, helper, acc) = self.interaction_columns(bus_interaction);
                collect_folded_columns(bus_interaction, folded)
                    .chain(collect_helper_columns(bus_interaction, helper))
                    .chain(collect_acc_columns(bus_interaction, acc))
                    .collect::<Vec<_>>()
            })
            // Each thread builds its own BTreeMap.
            .fold(BTreeMap::new, |mut acc, (poly_id, column)| {
                acc.entry(poly_id)
                    .and_modify(|existing: &mut Vec<T>| {
                        // Element-wise addition. We assume both vectors have the same length.
                        for (a, b) in existing.iter_mut().zip_eq(&column) {
                            *a += *b;
                        }
                    })
                    .or_insert(column);
                acc
            })
            // Merge the thread-local BTreeMaps.
            .reduce(BTreeMap::new, |mut map1, map2| {
                for (poly_id, column) in map2 {
                    map1.entry(poly_id)
                        .and_modify(|existing| {
                            for (a, b) in existing.iter_mut().zip_eq(&column) {
                                *a += *b;
                            }
                        })
                        .or_insert(column);
                }
                map1
            });

        // Finally, for each committed poly from the PIL in stage 1, remove its column from the map.
        let result = self
            .pil
            .committed_polys_in_source_order()
            .filter(|(symbol, _)| symbol.stage == Some(1))
            .flat_map(|(symbol, _)| symbol.array_elements())
            .map(|(name, poly_id)| {
                let column = columns
                    .remove(&poly_id)
                    .unwrap_or_else(|| panic!("Unexpected column: {name}"));
                (name, column)
            })
            .collect();
        assert!(
            columns.is_empty(),
            "Some expected columns not found in the PIL."
        );

        result
    }

    /// Given a bus interaction and existing witness values,
    /// calculates and returns a triple tuple of:
    /// - the folded columns (one per bus interaction)
    /// - one helper column per pair of bus interactions
    /// - the accumulator column (shared by all interactions)
    fn interaction_columns(
        &self,
        bus_interaction: &PhantomBusInteractionIdentity<T>,
    ) -> InteractionColumns<T> {
        let intermediate_definitions = self.pil.intermediate_definitions();

        let size = self.values.height();
        let mut folded_list = vec![Ext::zero(); size];
        let mut helper_list = vec![Ext::zero(); size];
        let mut acc_list = vec![Ext::zero(); size];

        for i in 0..size {
            let mut evaluator =
                ExpressionEvaluator::new(self.values.row(i), &intermediate_definitions);
            let current_acc = if i == 0 { Ext::zero() } else { acc_list[i - 1] };
            let multiplicity = evaluator.evaluate(&bus_interaction.multiplicity);

            let tuple = once(evaluator.evaluate(&bus_interaction.bus_id))
                .chain(
                    bus_interaction
                        .payload
                        .0
                        .iter()
                        .map(|r| evaluator.evaluate(r)),
                )
                .collect::<Vec<_>>();
            let folded = self.beta - self.fingerprint(&tuple);

            let helper = folded.inverse() * multiplicity;

            let new_acc = match multiplicity.is_zero() {
                true => current_acc,
                false => current_acc + helper,
            };

            folded_list[i] = folded;
            helper_list[i] = helper;
            acc_list[i] = new_acc;
        }

        // Transpose from row-major to column-major & flatten.
        let mut folded = vec![Vec::with_capacity(size); Ext::size()];
        let mut helper = vec![Vec::with_capacity(size); Ext::size()];
        let mut acc = vec![Vec::with_capacity(size); Ext::size()];
        for row_index in 0..size {
            for (col_index, x) in folded_list[row_index].to_vec().into_iter().enumerate() {
                folded[col_index].push(x);
            }
            for (col_index, x) in helper_list[row_index].to_vec().into_iter().enumerate() {
                helper[col_index].push(x);
            }
            for (col_index, x) in acc_list[row_index].to_vec().into_iter().enumerate() {
                acc[col_index].push(x);
            }
        }

        (folded, helper, acc)
    }

    /// Fingerprints a tuples of field elements, using the pre-computed powers of alpha.
    fn fingerprint(&self, tuple: &[T]) -> Ext {
        tuple
            .iter()
            .zip_eq(self.powers_of_alpha.iter().take(tuple.len()))
            .map(|(a, b)| (*b) * (*a))
            .sum()
    }
}

/// Given `alpha`, computes [1, alpha, alpha^2, ..., alpha^(n-1)].
fn powers_of_alpha<T, Ext: ExtensionField<T>>(alpha: Ext, n: usize) -> Vec<Ext> {
    (0..n)
        .scan(Ext::one(), |state, _| {
            let result = *state;
            *state = *state * alpha;
            Some(result)
        })
        .collect::<Vec<_>>()
}

fn collect_folded_columns<T>(
    bus_interaction: &PhantomBusInteractionIdentity<T>,
    folded: Vec<Vec<T>>,
) -> impl Iterator<Item = (PolyID, Vec<T>)> + '_ {
    bus_interaction
        .folded_expressions
        .0
        .iter()
        .zip_eq(folded)
        .filter_map(|(expr, column)| match expr {
            AlgebraicExpression::Reference(col_reference) if col_reference.is_witness() => {
                Some((col_reference.poly_id, column))
            }
            // If the folded payload is not persisted as witness columns, we skip it.
            _ => None,
        })
}

fn collect_acc_columns<T>(
    bus_interaction: &PhantomBusInteractionIdentity<T>,
    acc: Vec<Vec<T>>,
) -> impl Iterator<Item = (PolyID, Vec<T>)> + '_ {
    bus_interaction
        .accumulator_columns
        .iter()
        .zip_eq(acc)
        .map(|(column_reference, column)| (column_reference.poly_id, column))
}

fn collect_helper_columns<T>(
    bus_interaction: &PhantomBusInteractionIdentity<T>,
    helper: Vec<Vec<T>>,
) -> impl Iterator<Item = (PolyID, Vec<T>)> + '_ {
    bus_interaction
        .helper_columns
        .iter()
        .zip_eq(helper)
        .map(|(column_reference, column)| (column_reference.poly_id, column))
}
