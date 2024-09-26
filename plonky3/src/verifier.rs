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
use crate::{MultiStageAir, Proof, StarkVerifyingKey, VerifierConstraintFolder};
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
    verify_with_key(config, None, air, challenger, proof, vec![public_values])
}

#[instrument(skip_all)]
pub fn verify_with_key<SC, A>(
    config: &SC,
    verifying_key: Option<&StarkVerifyingKey<SC>>,
    air: &A,
    challenger: &mut SC::Challenger,
    proof: &Proof<SC>,
    public_values_by_stage: Vec<&Vec<Val<SC>>>,
) -> Result<(), VerificationError<PcsError<SC>>>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'a> MultiStageAir<VerifierConstraintFolder<'a, SC>>,
{
    let Proof {
        commitments,
        opened_values,
        opening_proof,
        degree_bits,
    } = proof;

    let degree = 1 << degree_bits;
    let log_quotient_degree = get_log_quotient_degree::<Val<SC>, A>(
        air,
        &public_values_by_stage
            .iter()
            .map(|values| values.len())
            .collect::<Vec<_>>(),
    );
    let quotient_degree = 1 << log_quotient_degree;
    let stage_count = proof.commitments.traces_by_stage.len();
    let challenge_counts: Vec<usize> = (0..stage_count)
        .map(|i| <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(air, i as u32))
        .collect();

    let pcs = config.pcs();
    let trace_domain = pcs.natural_domain_for_degree(degree);
    let quotient_domain =
        trace_domain.create_disjoint_domain(1 << (degree_bits + log_quotient_degree));
    let quotient_chunks_domains = quotient_domain.split_domains(quotient_degree);

    let air_widths = (0..stage_count)
        .map(|stage| {
            <A as MultiStageAir<SymbolicAirBuilder<Val<SC>>>>::stage_trace_width(air, stage as u32)
        })
        .collect::<Vec<usize>>();
    let air_fixed_width =
        <A as MultiStageAir<SymbolicAirBuilder<Val<SC>>>>::preprocessed_width(air);
    let valid_shape = opened_values.preprocessed_local.len() == air_fixed_width
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
        && public_values_by_stage.len() == stage_count
        && challenge_counts.len() == stage_count;

    if !valid_shape {
        return Err(VerificationError::InvalidProofShape);
    }

    // Observe the instance.
    challenger.observe(Val::<SC>::from_canonical_usize(proof.degree_bits));
    // TODO: Might be best practice to include other instance data here in the transcript, like some
    // encoding of the AIR. This protects against transcript collisions between distinct instances.
    // Practically speaking though, the only related known attack is from failing to include public
    // values. It's not clear if failing to include other instance data could enable a transcript
    // collision, since most such changes would completely change the set of satisfying witnesses.

    if let Some(verifying_key) = verifying_key {
        challenger.observe(verifying_key.preprocessed_commit.clone())
    };

    let mut challenges = vec![];

    commitments
        .traces_by_stage
        .iter()
        .zip(&public_values_by_stage)
        .zip(challenge_counts)
        .for_each(|((commitment, public_values), challenge_count)| {
            challenger.observe(commitment.clone());
            challenger.observe_slice(public_values);
            challenges.push((0..challenge_count).map(|_| challenger.sample()).collect());
        });
    let alpha: SC::Challenge = challenger.sample_ext_element();
    challenger.observe(commitments.quotient_chunks.clone());

    let zeta: SC::Challenge = challenger.sample();
    let zeta_next = trace_domain.next_point(zeta).unwrap();

    pcs.verify(
        iter::empty()
            .chain(
                verifying_key
                    .map(|verifying_key| {
                        (
                            verifying_key.preprocessed_commit.clone(),
                            (vec![(
                                trace_domain,
                                vec![
                                    (zeta, opened_values.preprocessed_local.clone()),
                                    (zeta_next, opened_values.preprocessed_next.clone()),
                                ],
                            )]),
                        )
                    })
                    .into_iter(),
            )
            .chain(
                izip!(
                    commitments.traces_by_stage.iter(),
                    opened_values.traces_by_stage_local.iter(),
                    opened_values.traces_by_stage_next.iter()
                )
                .map(|(trace_commit, opened_local, opened_next)| {
                    (
                        trace_commit.clone(),
                        vec![(
                            trace_domain,
                            vec![
                                (zeta, opened_local.clone()),
                                (zeta_next, opened_next.clone()),
                            ],
                        )],
                    )
                })
                .collect_vec(),
            )
            .chain([(
                commitments.quotient_chunks.clone(),
                quotient_chunks_domains
                    .iter()
                    .zip(&opened_values.quotient_chunks)
                    .map(|(domain, values)| (*domain, vec![(zeta, values.clone())]))
                    .collect_vec(),
            )])
            .collect_vec(),
        opening_proof,
        challenger,
    )
    .map_err(VerificationError::InvalidOpeningArgument)?;

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
        challenges,
        preprocessed,
        traces_by_stage,
        public_values_by_stage,
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

#[derive(Debug)]
pub enum VerificationError<PcsErr> {
    InvalidProofShape,
    /// An error occurred while verifying the claimed openings.
    InvalidOpeningArgument(PcsErr),
    /// Out-of-domain evaluation mismatch, i.e. `constraints(zeta)` did not match
    /// `quotient(zeta) Z_H(zeta)`.
    OodEvaluationMismatch,
}
