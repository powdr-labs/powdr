use alloc::vec;
use alloc::vec::Vec;
use core::iter;

use itertools::{izip, Itertools};
use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, PolynomialSpace};
use p3_field::{AbstractExtensionField, AbstractField, Field};
use p3_matrix::dense::RowMajorMatrixView;
use p3_matrix::stack::VerticalPair;
use tracing::instrument;

use crate::symbolic_builder::{get_log_quotient_degree, SymbolicAirBuilder};
use crate::{ChipOpenedValues, MultiStageAir, Proof, StarkVerifyingKey, VerifierConstraintFolder};
use p3_uni_stark::{PcsError, StarkGenericConfig, Val};

#[instrument(skip_all)]
pub fn verify<SC, A>(
    config: &SC,
    air: &A,
    challenger: &mut SC::Challenger,
    proof: &Proof<SC>,
    public_values: &Vec<Val<SC>>,
) -> Result<(), VerificationError<PcsError<SC>>>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'a> MultiStageAir<VerifierConstraintFolder<'a, SC>>,
{
    verify_with_key(
        config,
        None,
        vec![Table {
            air,
            public_values_by_stage: vec![public_values],
        }],
        challenger,
        proof,
        vec![vec![public_values]],
    )
}

/// A sub-table to be proven, in the form of an air and values for the public inputs
struct Table<
    'a,
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<VerifierConstraintFolder<'b, SC>>,
> {
    air: &'a A,
    public_values_by_stage: Vec<&'a Vec<Val<SC>>>,
}

#[instrument(skip_all)]
pub fn verify_with_key<SC, A>(
    config: &SC,
    verifying_key: Option<&StarkVerifyingKey<SC>>,
    inputs: Vec<Table<SC, A>>,
    challenger: &mut SC::Challenger,
    proof: &Proof<SC>,
    public_values_by_stage: Vec<Vec<&Vec<Val<SC>>>>,
) -> Result<(), VerificationError<PcsError<SC>>>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'a> MultiStageAir<VerifierConstraintFolder<'a, SC>>,
{
    if proof.opened_values.len() != inputs.len() {
        return Err(VerificationError::InvalidProofShape);
    }

    let pcs = config.pcs();

    let Proof {
        commitments,
        opened_values,
        opening_proof,
    } = proof;

    if let Some(vk) = verifying_key {
        challenger.observe(vk.preprocessed_commit.clone());
    }

    // loop through stages observing the trace and issuing challenges

    // Observe the instances.
    for opened_values in opened_values {
        challenger.observe(Val::<SC>::from_canonical_usize(opened_values.log_degree));
    }
    // TODO: Might be best practice to include other instance data here in the transcript, like some
    // encoding of the AIR. This protects against transcript collisions between distinct instances.
    // Practically speaking though, the only related known attack is from failing to include public
    // values. It's not clear if failing to include other instance data could enable a transcript
    // collision, since most such changes would completely change the set of satisfying witnesses.

    let stage_count = inputs
        .iter()
        .map(|i| i.air)
        .map(|air| <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(air))
        .max()
        .unwrap() as u32;

    let challenge_count_by_stage: Vec<usize> = (0..stage_count)
        .map(|stage_id| {
            inputs
                .iter()
                .map(|table| {
                    <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(
                        table.air, stage_id,
                    )
                })
                .max()
                .unwrap()
        })
        .collect();

    let challenges = commitments
        .traces_by_stage
        .iter()
        .zip_eq(&public_values_by_stage)
        .zip_eq(challenge_count_by_stage)
        .map(|((commitment, public_values), challenge_count)| {
            challenger.observe(commitment.clone());
            for public_values in public_values {
                challenger.observe_slice(public_values);
            }
            (0..challenge_count).map(|_| challenger.sample()).collect()
        })
        .collect_vec();

    let alpha: SC::Challenge = challenger.sample_ext_element();
    challenger.observe(commitments.quotient_chunks.clone());

    let zeta: SC::Challenge = challenger.sample();

    // maybe these need to be in the vk, right now the prover can mess with them
    let trace_domains = proof
        .opened_values
        .iter()
        .map(|opened_values| {
            let degree = 1 << opened_values.log_degree;
            pcs.natural_domain_for_degree(degree)
        })
        .collect::<Vec<_>>();

    let quotient_domains = inputs
        .iter()
        .zip(&proof.opened_values)
        .map(|(input, opened_values)| {
            let degree = 1 << opened_values.log_degree;
            let trace_domain = pcs.natural_domain_for_degree(degree);
            trace_domain
                .create_disjoint_domain(
                    1 << (opened_values.log_degree
                        + get_log_quotient_degree::<Val<SC>, A>(
                            input.air,
                            &input
                                .public_values_by_stage
                                .iter()
                                .map(|values| values.len())
                                .collect::<Vec<_>>(),
                        )),
                )
                .split_domains(
                    1 << get_log_quotient_degree::<Val<SC>, A>(
                        input.air,
                        &input
                            .public_values_by_stage
                            .iter()
                            .map(|values| values.len())
                            .collect::<Vec<_>>(),
                    ),
                )
        })
        .collect::<Vec<_>>();

    let verify_input: Vec<_> = proof
        .opened_values
        .iter()
        .zip_eq(trace_domains.iter().zip(quotient_domains.iter()))
        .flat_map(|(opened_values, (trace_domain, quotient_chunks_domains))| {
            let zeta_next = trace_domain.next_point(zeta).unwrap();

            iter::empty()
                .chain(
                    verifying_key
                        .map(|verifying_key| {
                            vec![(
                                verifying_key.preprocessed_commit.clone(),
                                vec![(
                                    *trace_domain,
                                    vec![
                                        (zeta, opened_values.preprocessed_local.clone()),
                                        (zeta_next, opened_values.preprocessed_next.clone()),
                                    ],
                                )],
                            )]
                        })
                        .into_iter()
                        .flatten(),
                )
                .chain(
                    izip!(
                        commitments.traces_by_stage.iter(),
                        opened_values.traces_by_stage_local.iter(),
                        opened_values.traces_by_stage_next.iter()
                    )
                    .map(move |(trace_commit, opened_local, opened_next)| {
                        (
                            trace_commit.clone(),
                            vec![(
                                *trace_domain,
                                vec![
                                    (zeta, opened_local.clone()),
                                    (zeta_next, opened_next.clone()),
                                ],
                            )],
                        )
                    }),
                )
                .chain([(
                    commitments.quotient_chunks.clone(),
                    quotient_chunks_domains
                        .iter()
                        .zip(&opened_values.quotient_chunks)
                        .map(|(domain, values)| (*domain, vec![(zeta, values.clone())]))
                        .collect_vec(),
                )])
        })
        .collect();

    pcs.verify(verify_input, opening_proof, challenger)
        .map_err(VerificationError::InvalidOpeningArgument)?;

    let public_values: Vec<_> = (0..inputs.len())
        .map(|i| {
            public_values_by_stage
                .iter()
                .map(|values| values[i])
                .collect::<Vec<_>>()
        })
        .collect();

    // Verify the constraint evaluations.
    for (table, trace_domain, quotient_chunks_domains, opened_values, public_values_by_stage) in izip!(
        inputs.iter(),
        trace_domains,
        quotient_domains,
        opened_values.iter(),
        public_values,
    ) {
        // Verify the shape of the opening arguments matches the expected values.
        verify_opening_shape(table, opened_values)?;
        // Verify the constraint evaluation.
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

        let preprocessed = VerticalPair::new(
            RowMajorMatrixView::new_row(&opened_values.preprocessed_local),
            RowMajorMatrixView::new_row(&opened_values.preprocessed_next),
        );

        let traces_by_stage = opened_values
            .traces_by_stage_local
            .iter()
            .zip(opened_values.traces_by_stage_next.iter())
            .map(|(trace_local, trace_next)| {
                VerticalPair::new(
                    RowMajorMatrixView::new_row(trace_local),
                    RowMajorMatrixView::new_row(trace_next),
                )
            })
            .collect::<Vec<VerticalPair<_, _>>>();

        let mut folder = VerifierConstraintFolder {
            challenges: &challenges,
            preprocessed,
            traces_by_stage,
            public_values_by_stage,
            is_first_row: sels.is_first_row,
            is_last_row: sels.is_last_row,
            is_transition: sels.is_transition,
            alpha,
            accumulator: SC::Challenge::zero(),
        };
        table.air.eval(&mut folder);
        let folded_constraints = folder.accumulator;

        // Finally, check that
        //     folded_constraints(zeta) / Z_H(zeta) = quotient(zeta)
        if folded_constraints * sels.inv_zeroifier != quotient {
            return Err(VerificationError::OodEvaluationMismatch);
        }
    }

    Ok(())
}

fn verify_opening_shape<
    'a,
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<VerifierConstraintFolder<'b, SC>>,
>(
    table: &Table<'a, SC, A>,
    opened_values: &ChipOpenedValues<SC::Challenge>,
) -> Result<(), VerificationError<PcsError<SC>>> {
    let log_quotient_degree = get_log_quotient_degree::<Val<SC>, A>(
        table.air,
        &table
            .public_values_by_stage
            .iter()
            .map(|values| values.len())
            .collect::<Vec<_>>(),
    );
    let quotient_degree = 1 << log_quotient_degree;
    let stage_count = <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(table.air);
    let challenge_counts: Vec<usize> = (0..stage_count)
        .map(|i| {
            <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(table.air, i as u32)
        })
        .collect();

    let air_widths = (0..stage_count)
        .map(|stage| {
            <A as MultiStageAir<SymbolicAirBuilder<Val<SC>>>>::stage_trace_width(
                table.air,
                stage as u32,
            )
        })
        .collect::<Vec<usize>>();
    let air_fixed_width =
        <A as MultiStageAir<SymbolicAirBuilder<Val<SC>>>>::preprocessed_width(table.air);
    let res = opened_values.preprocessed_local.len() == air_fixed_width
        && opened_values.preprocessed_next.len() == air_fixed_width
        && opened_values
            .traces_by_stage_local
            .iter()
            .zip(&air_widths)
            .all(|(stage, air_width)| stage.len() == *air_width)
        && opened_values
            .traces_by_stage_next
            .iter()
            .zip(&air_widths)
            .all(|(stage, air_width)| stage.len() == *air_width)
        && opened_values.quotient_chunks.len() == quotient_degree
        && opened_values
            .quotient_chunks
            .iter()
            .all(|qc| qc.len() == <SC::Challenge as AbstractExtensionField<Val<SC>>>::D)
        && table.public_values_by_stage.len() == stage_count
        && challenge_counts.len() == stage_count;

    res.then_some(())
        .ok_or(VerificationError::InvalidProofShape)
}

#[derive(Debug)]
pub enum VerificationError<PcsErr> {
    InvalidProofShape,
    /// An error occurred while verifying the claimed openings.
    InvalidOpeningArgument(PcsErr),
    /// Out-of-domain evaluation mismatch, i.e. `constraints(zeta)` did not match
    /// `quotient(zeta) Z_H(zeta)`.
    OodEvaluationMismatch,
}
