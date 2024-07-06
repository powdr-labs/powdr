use itertools::Itertools;
use p3_air::TwoRowMatrixView;
use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, PolynomialSpace};
use p3_field::{AbstractExtensionField, AbstractField, Field};
use p3_uni_stark::{StarkGenericConfig, Val, VerificationError};
use p3_util::log2_ceil_usize;

use crate::circuit_builder::PowdrAir;

use super::{
    folder::VerifierConstraintFolder,
    params::{Proof, StarkVerifyingKey},
};

pub fn verify<SC, A>(
    config: &SC,
    verifying_key: Option<&StarkVerifyingKey<SC>>,
    air: &A,
    challenger: &mut SC::Challenger,
    proof: &Proof<SC>,
    public_values: &Vec<Val<SC>>,
) -> Result<(), VerificationError>
where
    SC: StarkGenericConfig,
    A: for<'a> PowdrAir<VerifierConstraintFolder<'a, SC>>,
{
    let Proof {
        commitments,
        opened_values,
        opening_proof,
        degree_bits,
    } = proof;

    let degree = 1 << degree_bits;
    let constraint_degree = 2; // TODO: enforce that
    let log_quotient_degree = log2_ceil_usize(constraint_degree - 1);
    let quotient_degree = 1 << log_quotient_degree;

    let pcs = config.pcs();
    let trace_domain = pcs.natural_domain_for_degree(degree);
    let quotient_domain =
        trace_domain.create_disjoint_domain(1 << (degree_bits + log_quotient_degree));
    let quotient_chunks_domains = quotient_domain.split_domains(quotient_degree);

    let air_width = air.width();
    let air_fixed_width = air.fixed_width();
    let valid_shape = (air_fixed_width == 0 || verifying_key.is_some()) // if we have fixed columns, we have a verifying key
        && opened_values.fixed_local.as_ref().map(|v| v.len()).unwrap_or_default() == air_fixed_width
        && opened_values.fixed_next.as_ref().map(|v| v.len()).unwrap_or_default() == air_fixed_width
        && opened_values.trace_local.len() == air_width
        && opened_values.trace_next.len() == air_width
        && opened_values.quotient_chunks.len() == quotient_degree
        && opened_values
            .quotient_chunks
            .iter()
            .all(|qc| qc.len() == <SC::Challenge as AbstractExtensionField<Val<SC>>>::D);
    if !valid_shape {
        return Err(VerificationError::InvalidProofShape);
    }

    challenger.observe(commitments.trace.clone());
    let alpha: SC::Challenge = challenger.sample_ext_element();
    challenger.observe(commitments.quotient_chunks.clone());

    let zeta: SC::Challenge = challenger.sample();
    let zeta_next = trace_domain.next_point(zeta).unwrap();

    pcs.verify(
        verifying_key
            .map(|verifying_key| {
                (
                    verifying_key.fixed_commit.clone(),
                    (vec![(
                        trace_domain,
                        vec![
                            (zeta, opened_values.fixed_local.as_ref().unwrap().clone()),
                            (
                                zeta_next,
                                opened_values.fixed_next.as_ref().unwrap().clone(),
                            ),
                        ],
                    )]),
                )
            })
            .into_iter()
            .chain([
                (
                    commitments.trace.clone(),
                    vec![(
                        trace_domain,
                        vec![
                            (zeta, opened_values.trace_local.clone()),
                            (zeta_next, opened_values.trace_next.clone()),
                        ],
                    )],
                ),
                (
                    commitments.quotient_chunks.clone(),
                    quotient_chunks_domains
                        .iter()
                        .zip(&opened_values.quotient_chunks)
                        .map(|(domain, values)| (*domain, vec![(zeta, values.clone())]))
                        .collect_vec(),
                ),
            ])
            .collect(),
        opening_proof,
        challenger,
    )
    .map_err(|_| VerificationError::InvalidOpeningArgument)?;

    let zps = quotient_chunks_domains
        .iter()
        .enumerate()
        .map(|(i, domain)| {
            quotient_chunks_domains
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, other_domain)| {
                    other_domain.zp_at_point(zeta)
                        * other_domain.zp_at_point(domain.first_point()).inverse()
                })
                .product::<SC::Challenge>()
        })
        .collect_vec();

    let quotient = opened_values
        .quotient_chunks
        .iter()
        .enumerate()
        .map(|(ch_i, ch)| {
            ch.iter()
                .enumerate()
                .map(|(e_i, &c)| zps[ch_i] * SC::Challenge::monomial(e_i) * c)
                .sum::<SC::Challenge>()
        })
        .sum::<SC::Challenge>();

    let sels = trace_domain.selectors_at_point(zeta);

    let mut folder = VerifierConstraintFolder {
        main: TwoRowMatrixView {
            local: &opened_values.trace_local,
            next: &opened_values.trace_next,
        },
        fixed: verifying_key.is_some().then(|| TwoRowMatrixView {
            local: opened_values.fixed_local.as_ref().unwrap(),
            next: opened_values.fixed_next.as_ref().unwrap(),
        }),
        public_values,
        is_first_row: sels.is_first_row,
        is_last_row: sels.is_last_row,
        is_transition: sels.is_transition,
        alpha,
        accumulator: SC::Challenge::zero(),
    };
    air.eval(&mut folder);
    let folded_constraints = folder.accumulator;

    // Finally, check that
    //     folded_constraints(zeta) / Z_H(zeta) = quotient(zeta)
    if folded_constraints * sels.inv_zeroifier != quotient {
        return Err(VerificationError::OodEvaluationMismatch);
    }

    Ok(())
}
