use alloc::borrow::ToOwned;
use alloc::vec;
use alloc::vec::Vec;
use core::iter::{self, once};

use itertools::{izip, Itertools};
use p3_air::Air;
use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, PolynomialSpace};
use p3_field::{AbstractExtensionField, AbstractField, PackedValue};
use p3_matrix::dense::RowMajorMatrix;
use p3_matrix::Matrix;
use p3_maybe_rayon::prelude::*;
use p3_util::log2_strict_usize;
use tracing::{info_span, instrument};

use crate::symbolic_builder::{get_log_quotient_degree, SymbolicAirBuilder};
use crate::traits::MultiStageAir;
use crate::{
    Commitments, OpenedValues, ProcessedStage, Proof, ProverConstraintFolder, StarkProvingKey,
};
use p3_uni_stark::{Domain, PackedChallenge, PackedVal, StarkGenericConfig, Val};

#[instrument(skip_all)]
#[allow(clippy::multiple_bound_locations)] // cfg not supported in where clauses?
pub fn prove_with_key<
    SC,
    #[cfg(debug_assertions)] A: for<'a> Air<crate::check_constraints::DebugConstraintBuilder<'a, Val<SC>>>,
    #[cfg(not(debug_assertions))] A,
    C,
>(
    config: &SC,
    proving_key: Option<&StarkProvingKey<SC>>,
    air: &A,
    challenger: &mut SC::Challenger,
    stage_0_trace: RowMajorMatrix<Val<SC>>,
    next_stage_trace_callback: &C,
    #[allow(clippy::ptr_arg)]
    // we do not use `&[Val<SC>]` in order to keep the same API
    stage_0_public_values: &Vec<Val<SC>>,
) -> Proof<SC>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'a> MultiStageAir<ProverConstraintFolder<'a, SC>>,
    C: NextStageTraceCallback<SC>,
{
    let degree = stage_0_trace.height();
    let log_degree = log2_strict_usize(degree);

    let stage_count = <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(air);

    let pcs = config.pcs();
    let trace_domain = pcs.natural_domain_for_degree(degree);

    // Observe the instance.
    challenger.observe(Val::<SC>::from_canonical_usize(log_degree));
    // TODO: Might be best practice to include other instance data here; see verifier comment.

    if let Some(proving_key) = proving_key {
        challenger.observe(proving_key.preprocessed_commit.clone())
    };

    let mut state: ProverState<SC> = ProverState::new(pcs, trace_domain, challenger);
    let mut stage = Stage {
        trace: stage_0_trace,
        challenge_count: <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(air, 0),
        public_values: stage_0_public_values.to_owned(),
    };

    assert!(stage_count >= 1);
    // generate all stages starting from the second one based on the witgen callback
    for stage_id in 1..stage_count {
        state = state.run_stage(stage);
        // get the challenges drawn at the end of the previous stage
        let local_challenges = &state.processed_stages.last().unwrap().challenge_values;
        let CallbackResult {
            trace,
            public_values,
            challenges,
        } = next_stage_trace_callback.compute_stage(stage_id as u32, local_challenges);
        // replace the challenges of the last stage with the ones received
        state.processed_stages.last_mut().unwrap().challenge_values = challenges;
        // go to the next stage
        stage = Stage {
            trace,
            challenge_count: <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(
                air,
                stage_id as u32,
            ),
            public_values,
        };
    }

    // run the last stage
    state = state.run_stage(stage);

    // sanity check that the last stage did not create any challenges
    assert!(state
        .processed_stages
        .last()
        .unwrap()
        .challenge_values
        .is_empty());
    // sanity check that we processed as many stages as expected
    assert_eq!(state.processed_stages.len(), stage_count);

    // with the witness complete, check the constraints
    #[cfg(debug_assertions)]
    crate::check_constraints::check_constraints(
        air,
        &air.preprocessed_trace()
            .unwrap_or(RowMajorMatrix::new(Default::default(), 0)),
        state.processed_stages.iter().map(|s| &s.trace).collect(),
        &state
            .processed_stages
            .iter()
            .map(|s| &s.public_values)
            .collect(),
        state
            .processed_stages
            .iter()
            .map(|s| &s.challenge_values)
            .collect(),
    );

    let log_quotient_degree = get_log_quotient_degree::<Val<SC>, A>(
        air,
        &state
            .processed_stages
            .iter()
            .map(|s| s.public_values.len())
            .collect::<Vec<_>>(),
    );
    let quotient_degree = 1 << log_quotient_degree;

    let challenger = &mut state.challenger;

    let alpha: SC::Challenge = challenger.sample_ext_element();

    let quotient_domain =
        trace_domain.create_disjoint_domain(1 << (log_degree + log_quotient_degree));

    let preprocessed_on_quotient_domain = proving_key.map(|proving_key| {
        pcs.get_evaluations_on_domain(&proving_key.preprocessed_data, 0, quotient_domain)
    });

    let traces_on_quotient_domain = state
        .processed_stages
        .iter()
        .map(|s| pcs.get_evaluations_on_domain(&s.prover_data, 0, quotient_domain))
        .collect();

    let challenges = state
        .processed_stages
        .iter()
        .map(|stage| stage.challenge_values.clone())
        .collect();

    let public_values_by_stage = state
        .processed_stages
        .iter()
        .map(|stage| stage.public_values.clone())
        .collect();

    let quotient_values = quotient_values(
        air,
        &public_values_by_stage,
        trace_domain,
        quotient_domain,
        preprocessed_on_quotient_domain,
        traces_on_quotient_domain,
        challenges,
        alpha,
    );
    let quotient_flat = RowMajorMatrix::new_col(quotient_values).flatten_to_base();
    let quotient_chunks = quotient_domain.split_evals(quotient_degree, quotient_flat);
    let qc_domains = quotient_domain.split_domains(quotient_degree);

    let (quotient_commit, quotient_data) = info_span!("commit to quotient poly chunks")
        .in_scope(|| pcs.commit(izip!(qc_domains, quotient_chunks).collect_vec()));
    challenger.observe(quotient_commit.clone());

    let commitments = Commitments {
        traces_by_stage: state
            .processed_stages
            .iter()
            .map(|s| s.commitment.clone())
            .collect(),
        quotient_chunks: quotient_commit,
    };

    let zeta: SC::Challenge = challenger.sample();
    let zeta_next = trace_domain.next_point(zeta).unwrap();

    let (opened_values, opening_proof) = pcs.open(
        iter::empty()
            .chain(
                proving_key
                    .map(|proving_key| {
                        (&proving_key.preprocessed_data, vec![vec![zeta, zeta_next]])
                    })
                    .into_iter(),
            )
            .chain(
                state
                    .processed_stages
                    .iter()
                    .map(|processed_stage| {
                        (&processed_stage.prover_data, vec![vec![zeta, zeta_next]])
                    })
                    .collect_vec(),
            )
            .chain(once((
                &quotient_data,
                // open every chunk at zeta
                (0..quotient_degree).map(|_| vec![zeta]).collect_vec(),
            )))
            .collect_vec(),
        challenger,
    );
    let mut opened_values = opened_values.iter();

    // maybe get values for the preprocessed columns
    let (preprocessed_local, preprocessed_next) = if proving_key.is_some() {
        let value = opened_values.next().unwrap();
        assert_eq!(value.len(), 1);
        assert_eq!(value[0].len(), 2);
        (value[0][0].clone(), value[0][1].clone())
    } else {
        (vec![], vec![])
    };

    // get values for the traces
    let (traces_by_stage_local, traces_by_stage_next): (Vec<_>, Vec<_>) = state
        .processed_stages
        .iter()
        .map(|_| {
            let value = opened_values.next().unwrap();
            assert_eq!(value.len(), 1);
            assert_eq!(value[0].len(), 2);
            (value[0][0].clone(), value[0][1].clone())
        })
        .unzip();

    // get values for the quotient
    let value = opened_values.next().unwrap();
    assert_eq!(value.len(), quotient_degree);
    let quotient_chunks = value.iter().map(|v| v[0].clone()).collect_vec();

    let opened_values = OpenedValues {
        traces_by_stage_local,
        traces_by_stage_next,
        preprocessed_local,
        preprocessed_next,
        quotient_chunks,
    };
    Proof {
        commitments,
        opened_values,
        opening_proof,
        degree_bits: log_degree,
    }
}

#[allow(clippy::too_many_arguments)]
#[instrument(name = "compute quotient polynomial", skip_all)]
fn quotient_values<'a, SC, A, Mat>(
    air: &A,
    public_values_by_stage: &'a Vec<Vec<Val<SC>>>,
    trace_domain: Domain<SC>,
    quotient_domain: Domain<SC>,
    preprocessed_on_quotient_domain: Option<Mat>,
    traces_on_quotient_domain: Vec<Mat>,
    challenges: Vec<Vec<Val<SC>>>,
    alpha: SC::Challenge,
) -> Vec<SC::Challenge>
where
    SC: StarkGenericConfig,
    A: Air<ProverConstraintFolder<'a, SC>>,
    Mat: Matrix<Val<SC>> + Sync,
{
    let quotient_size = quotient_domain.size();
    let preprocessed_width = preprocessed_on_quotient_domain
        .as_ref()
        .map(Matrix::width)
        .unwrap_or_default();
    let mut sels = trace_domain.selectors_on_coset(quotient_domain);

    let qdb = log2_strict_usize(quotient_domain.size()) - log2_strict_usize(trace_domain.size());
    let next_step = 1 << qdb;

    // We take PackedVal::<SC>::WIDTH worth of values at a time from a quotient_size slice, so we need to
    // pad with default values in the case where quotient_size is smaller than PackedVal::<SC>::WIDTH.
    for _ in quotient_size..PackedVal::<SC>::WIDTH {
        sels.is_first_row.push(Val::<SC>::default());
        sels.is_last_row.push(Val::<SC>::default());
        sels.is_transition.push(Val::<SC>::default());
        sels.inv_zeroifier.push(Val::<SC>::default());
    }

    (0..quotient_size)
        .into_par_iter()
        .step_by(PackedVal::<SC>::WIDTH)
        .flat_map_iter(|i_start| {
            let i_range = i_start..i_start + PackedVal::<SC>::WIDTH;

            let is_first_row = *PackedVal::<SC>::from_slice(&sels.is_first_row[i_range.clone()]);
            let is_last_row = *PackedVal::<SC>::from_slice(&sels.is_last_row[i_range.clone()]);
            let is_transition = *PackedVal::<SC>::from_slice(&sels.is_transition[i_range.clone()]);
            let inv_zeroifier = *PackedVal::<SC>::from_slice(&sels.inv_zeroifier[i_range.clone()]);

            let preprocessed = RowMajorMatrix::new(
                preprocessed_on_quotient_domain
                    .as_ref()
                    .map(|on_quotient_domain| {
                        iter::empty()
                            .chain(on_quotient_domain.vertically_packed_row(i_start))
                            .chain(on_quotient_domain.vertically_packed_row(i_start + next_step))
                            .collect_vec()
                    })
                    .unwrap_or_default(),
                preprocessed_width,
            );

            let traces_by_stage = traces_on_quotient_domain
                .iter()
                .map(|trace_on_quotient_domain| {
                    RowMajorMatrix::new(
                        iter::empty()
                            .chain(trace_on_quotient_domain.vertically_packed_row(i_start))
                            .chain(
                                trace_on_quotient_domain.vertically_packed_row(i_start + next_step),
                            )
                            .collect_vec(),
                        trace_on_quotient_domain.width(),
                    )
                })
                .collect();

            let accumulator = PackedChallenge::<SC>::zero();
            let mut folder = ProverConstraintFolder {
                challenges: challenges.clone(),
                traces_by_stage,
                preprocessed,
                public_values_by_stage,
                is_first_row,
                is_last_row,
                is_transition,
                alpha,
                accumulator,
            };
            air.eval(&mut folder);

            // quotient(x) = constraints(x) / Z_H(x)
            let quotient = folder.accumulator * inv_zeroifier;

            // "Transpose" D packed base coefficients into WIDTH scalar extension coefficients.
            (0..core::cmp::min(quotient_size, PackedVal::<SC>::WIDTH)).map(move |idx_in_packing| {
                let quotient_value = (0..<SC::Challenge as AbstractExtensionField<Val<SC>>>::D)
                    .map(|coeff_idx| quotient.as_base_slice()[coeff_idx].as_slice()[idx_in_packing])
                    .collect::<Vec<_>>();
                SC::Challenge::from_base_slice(&quotient_value)
            })
        })
        .collect()
}

pub struct ProverState<'a, SC: StarkGenericConfig> {
    pub(crate) processed_stages: Vec<ProcessedStage<SC>>,
    pub(crate) challenger: &'a mut SC::Challenger,
    pub(crate) pcs: &'a <SC>::Pcs,
    pub(crate) trace_domain: Domain<SC>,
}

impl<'a, SC: StarkGenericConfig> ProverState<'a, SC> {
    pub(crate) fn new(
        pcs: &'a <SC as StarkGenericConfig>::Pcs,
        trace_domain: Domain<SC>,
        challenger: &'a mut <SC as StarkGenericConfig>::Challenger,
    ) -> Self {
        Self {
            processed_stages: Default::default(),
            challenger,
            pcs,
            trace_domain,
        }
    }

    pub(crate) fn run_stage(mut self, stage: Stage<SC>) -> Self {
        #[cfg(debug_assertions)]
        let trace = stage.trace.clone();

        // commit to the trace for this stage
        let (commitment, prover_data) = info_span!("commit to stage {stage} data")
            .in_scope(|| self.pcs.commit(vec![(self.trace_domain, stage.trace)]));

        self.challenger.observe(commitment.clone());
        // observe the public inputs for this stage
        self.challenger.observe_slice(&stage.public_values);

        let challenge_values = (0..stage.challenge_count)
            .map(|_| self.challenger.sample())
            .collect();

        self.processed_stages.push(ProcessedStage {
            public_values: stage.public_values,
            prover_data,
            commitment,
            challenge_values,
            #[cfg(debug_assertions)]
            trace,
        });
        self
    }
}

pub struct Stage<SC: StarkGenericConfig> {
    /// the witness for this stage
    pub(crate) trace: RowMajorMatrix<Val<SC>>,
    /// the number of challenges to be drawn at the end of this stage
    pub(crate) challenge_count: usize,
    /// the public values for this stage
    pub(crate) public_values: Vec<Val<SC>>,
}

pub struct CallbackResult<T> {
    /// the trace for this stage
    pub(crate) trace: RowMajorMatrix<T>,
    /// the values of the public inputs of this stage
    pub(crate) public_values: Vec<T>,
    /// the values of the challenges drawn at the previous stage
    pub(crate) challenges: Vec<T>,
}

impl<T> CallbackResult<T> {
    pub fn new(trace: RowMajorMatrix<T>, public_values: Vec<T>, challenges: Vec<T>) -> Self {
        Self {
            trace,
            public_values,
            challenges,
        }
    }
}

pub trait NextStageTraceCallback<SC: StarkGenericConfig> {
    /// Computes the stage number `trace_stage` based on `challenges` drawn at the end of stage `trace_stage - 1`
    fn compute_stage(&self, stage: u32, challenges: &[Val<SC>]) -> CallbackResult<Val<SC>>;
}
