use std::collections::BTreeMap;

use powdr_ast::analyzed::{Analyzed, Identity};
use powdr_number::FieldElement;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::witgen::evaluators::expression_evaluator::ExpressionEvaluator;

use super::evaluators::expression_evaluator::OwnedTraceValues;

pub fn generate_bus_accumulators<T: FieldElement>(
    pil: &Analyzed<T>,
    witness_columns: &[(String, Vec<T>)],
    fixed_columns: Vec<(String, &[T])>,
    challenges: BTreeMap<u64, T>,
) -> Vec<(String, Vec<T>)> {
    let bus_interactions = pil
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::PhantomBusInteraction(i) => Some(i),
            _ => None,
        })
        .collect::<Vec<_>>();

    let trace_values = OwnedTraceValues::new(
        pil,
        witness_columns.to_vec(),
        fixed_columns
            .into_iter()
            .map(|(name, values)| (name, values.to_vec()))
            .collect(),
    );
    let accumulators = (0..bus_interactions.len())
        .into_par_iter()
        .map(|i| interaction_columns(pil, i, &trace_values, &challenges))
        .collect::<Vec<_>>();

    witness_columns
        .iter()
        .cloned()
        .chain(accumulators.into_iter().flatten())
        .collect()
}

fn interaction_columns<T: FieldElement>(
    pil: &Analyzed<T>,
    connection_index: usize,
    trace_values: &OwnedTraceValues<T>,
    challenges: &BTreeMap<u64, T>,
) -> Vec<(String, Vec<T>)> {
    let bus_interactions = pil
        .identities
        .iter()
        .filter_map(|identity| match identity {
            Identity::PhantomBusInteraction(i) => Some(i),
            _ => None,
        })
        .collect::<Vec<_>>();
    let bus_interaction = bus_interactions[connection_index];

    let namespace = pil
        .committed_polys_in_source_order()
        .next()
        .unwrap()
        .0
        .absolute_name
        .split("::")
        .next()
        .unwrap();
    let intermediate_definitions = pil.intermediate_definitions();

    let size = trace_values.height();
    let mut acc1 = vec![T::zero(); size];
    let mut acc2 = vec![T::zero(); size];
    let mut acc1_next = vec![T::zero(); size];
    let mut acc2_next = vec![T::zero(); size];

    let alpha = (challenges[&1], challenges[&2]);
    let beta = (challenges[&3], challenges[&4]);

    for i in 0..size {
        let mut evaluator =
            ExpressionEvaluator::new(trace_values.row(i), &intermediate_definitions, challenges);
        let current_acc = if i == 0 {
            (T::zero(), T::zero())
        } else {
            (acc1[i - 1], acc2[i - 1])
        };

        let tuple = bus_interaction
            .tuple
            .0
            .iter()
            .map(|r| evaluator.evaluate(r))
            .collect::<Vec<_>>();

        let fingerprint = sub_ext(beta, fingerprint(&tuple, alpha));
        let multiplicity = evaluator.evaluate(&bus_interaction.multiplicity);

        /*
        add_ext(
            current_acc,
            mul_ext(m_ext_next, inv_ext(folded_next))
        )
         */
        let update = add_ext(
            current_acc,
            mul_ext((multiplicity, T::from(0)), inv_ext(fingerprint)),
        );

        acc1[i] = update.0;
        acc2[i] = update.1;
        acc1_next[(i + size - 1) % size] = update.0;
        acc2_next[(i + size - 1) % size] = update.1;
    }

    vec![
        (name(namespace, "acc", connection_index * 2), acc1),
        (name(namespace, "acc", connection_index * 2 + 1), acc2),
        (name(namespace, "acc_next", connection_index * 2), acc1_next),
        (
            name(namespace, "acc_next", connection_index * 2 + 1),
            acc2_next,
        ),
    ]
}

fn name(namespace: &str, base: &str, i: usize) -> String {
    if i == 0 {
        return format!("{namespace}::{base}");
    }
    format!("{namespace}::{base}_{i}")
}

/*
let<T: Add + FromLiteral + Mul> mul_ext: Fp2<T>, Fp2<T> -> Fp2<T> = |a, b| match (a, b) {
    (Fp2::Fp2(a0, a1), Fp2::Fp2(b0, b1)) => Fp2::Fp2(
        // Multiplication modulo the polynomial x^2 - 11. We'll use the fact
        // that x^2 == 11 (mod x^2 - 11), so:
        // (a0 + a1 * x) * (b0 + b1 * x) = a0 * b0 + 11 * a1 * b1 + (a1 * b0 + a0 * b1) * x (mod x^2 - 11)
        a0 * b0 + 11 * a1 * b1,
        a1 * b0 + a0 * b1
    )
};
*/

fn mul_ext<T: FieldElement>(a: (T, T), b: (T, T)) -> (T, T) {
    (a.0 * b.0 + a.1 * b.1 * T::from(11), a.1 * b.0 + a.0 * b.1)
}

fn add_ext<T: FieldElement>(a: (T, T), b: (T, T)) -> (T, T) {
    (a.0 + b.0, a.1 + b.1)
}

fn sub_ext<T: FieldElement>(a: (T, T), b: (T, T)) -> (T, T) {
    (a.0 - b.0, a.1 - b.1)
}

/*
/// Maps [x_1, x_2, ..., x_n] to its Read-Solomon fingerprint, using a challenge alpha: $\sum_{i=1}^n alpha**{(n - i)} * x_i$
/// To generate an expression that computes the fingerprint, use `fingerprint_inter` instead.
/// Note that alpha is passed as an expressions, so that it is only evaluated if needed (i.e., if len(expr_array) > 1).
let fingerprint: fe[], Fp2<expr> -> Fp2<fe> = query |expr_array, alpha| {
    fingerprint_impl(expr_array, eval_ext(alpha), len(expr_array))
};

let fingerprint_impl: fe[], Fp2<fe>, int -> Fp2<fe> = query |expr_array, alpha, l| if l == 1 {
    // Base case
    from_base(expr_array[0])
} else {

    // Recursively compute the fingerprint as fingerprint(expr_array[:-1], alpha) * alpha + expr_array[-1]
    let intermediate_fingerprint = fingerprint_impl(expr_array, alpha, l - 1);
    add_ext(mul_ext(alpha, intermediate_fingerprint), from_base(expr_array[l - 1]))
};
*/

fn fingerprint<T: FieldElement>(expr_array: &[T], alpha: (T, T)) -> (T, T) {
    fingerprint_impl(expr_array, alpha, expr_array.len())
}

fn fingerprint_impl<T: FieldElement>(expr_array: &[T], alpha: (T, T), l: usize) -> (T, T) {
    if l == 1 {
        return (expr_array[0], T::zero());
    }

    let intermediate_fingerprint = fingerprint_impl(expr_array, alpha, l - 1);
    add_ext(
        mul_ext(alpha, intermediate_fingerprint),
        (expr_array[l - 1], T::zero()),
    )
}

/*

/// Extension field inversion
let inv_ext: Fp2<fe> -> Fp2<fe> = |a| match a {
    // The inverse of (a0, a1) is a point (b0, b1) such that:
    // (a0 + a1 * x) (b0 + b1 * x) = 1 (mod x^2 - 11)
    // Multiplying out and plugging in x^2 = 11 yields the following system of linear equations:
    // a0 * b0 + 11 * a1 * b1 = 1
    // a1 * b0 + a0 * b1 = 0
    // Solving for (b0, b1) yields:
    Fp2::Fp2(a0, a1) => {
        let factor = inv_field(11 * a1 * a1 - a0 * a0);
        Fp2::Fp2(-a0 * factor, a1 * factor)
    }
};

*/
fn inv_ext<T: FieldElement>(a: (T, T)) -> (T, T) {
    let factor = T::from(1) / (T::from(11) * a.1 * a.1 - a.0 * a.0);
    (-a.0 * factor, a.1 * factor)
}
