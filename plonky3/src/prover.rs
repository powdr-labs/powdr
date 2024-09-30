use alloc::vec;
use alloc::vec::Vec;
use core::iter::{self, once};
use core::marker::PhantomData;

use itertools::Itertools;
use p3_air::Air;
use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs, PolynomialSpace};
use p3_field::{AbstractExtensionField, AbstractField, PackedValue};
use p3_matrix::dense::{DenseMatrix, RowMajorMatrix};
use p3_matrix::Matrix;
use p3_maybe_rayon::prelude::*;
use p3_util::log2_strict_usize;
use tracing::{info_span, instrument};

use crate::symbolic_builder::{get_log_quotient_degree, SymbolicAirBuilder};
use crate::traits::MultiStageAir;
use crate::{
    ChipOpenedValues, Com, Commitments, PcsProof, PcsProverData, ProcessedStage, Proof,
    ProverConstraintFolder, StarkProvingKey,
};
use p3_uni_stark::{Domain, PackedChallenge, PackedVal, StarkGenericConfig, Val};

/// A sub-table to be proven, in the form of an air, a proving key, a stage 0 trace and stage 0 publics
struct MultiTable<'a, SC, A> {
    tables: Vec<Table<'a, SC, A>>,
}

impl<
        'a,
        SC,
        #[cfg(debug_assertions)] A: for<'b> Air<crate::check_constraints::DebugConstraintBuilder<'b, Val<SC>>>,
        #[cfg(not(debug_assertions))] A,
    > MultiTable<'a, SC, A>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<ProverConstraintFolder<'b, SC>>,
{
    fn table_count(&self) -> usize {
        self.tables.len()
    }

    /// Returns the number of stages in the table with the most stages.
    ///
    /// # Panics
    ///
    /// Panics if there are no tables.
    fn stage_count(&self) -> u32 {
        self.tables
            .iter()
            .map(|i| i.air)
            .map(|air| <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(air))
            .max()
            .unwrap() as u32
    }

    /// Observe the instance for each table.
    fn observe_instances(&self, challenger: &mut SC::Challenger) {
        for input in &self.tables {
            input.observe_instance(challenger);
        }
    }

    fn quotient_chunks_count(&self) -> usize {
        self.tables
            .iter()
            .map(|table| 1 << table.log_quotient_degree())
            .sum()
    }

    /// Commit to the quotient polynomial across all tables.
    ///
    /// Returns a single commitment and the prover data.
    fn commit_to_quotient(
        &self,
        state: &mut ProverState<'a, SC, A>,
        proving_key: Option<&StarkProvingKey<SC>>,
    ) -> (Com<SC>, PcsProverData<SC>) {
        let alpha: SC::Challenge = state.challenger.sample_ext_element();

        // get the quotient domains and chunks for each table
        let quotient_domains_and_chunks: Vec<_> = self
            .tables
            .iter()
            .enumerate()
            .flat_map(|(index, i)| i.quotient_domains_and_chunks(index, state, proving_key, alpha))
            .collect();

        assert_eq!(
            quotient_domains_and_chunks.len(),
            self.quotient_chunks_count()
        );

        // commit to the chunks
        let (quotient_commit, quotient_data) = info_span!("commit to quotient poly chunks")
            .in_scope(|| state.pcs.commit(quotient_domains_and_chunks));
        // observe the commitment
        state.challenger.observe(quotient_commit.clone());

        (quotient_commit, quotient_data)
    }

    /// Opens the commitments to the preprocessed trace, the traces, and the quotient polynomial.
    ///
    /// # Panics
    ///
    /// Panics if .
    fn open(
        &self,
        state: &mut ProverState<SC, A>,
        proving_key: Option<&StarkProvingKey<SC>>,
        quotient_data: PcsProverData<SC>,
    ) -> (Vec<ChipOpenedValues<SC::Challenge>>, PcsProof<SC>) {
        let zeta: SC::Challenge = state.challenger.sample();

        let preprocessed_data_and_opening_points = proving_key.as_ref().map(|key| {
            (
                &key.preprocessed_data,
                self.tables
                    .iter()
                    .map(|input| {
                        vec![
                            zeta,
                            input.trace_domain(state.pcs).next_point(zeta).unwrap(),
                        ]
                    })
                    .collect(),
            )
        });

        let trace_data_and_points_per_stage: Vec<(_, Vec<Vec<_>>)> = state
            .processed_stages
            .iter()
            .map(|processed_stage| {
                let points = self
                    .tables
                    .iter()
                    .map(|input| {
                        vec![
                            zeta,
                            input.trace_domain(state.pcs).next_point(zeta).unwrap(),
                        ]
                    })
                    .collect();
                (&processed_stage.prover_data, points)
            })
            .collect();

        let quotient_opening_points: Vec<_> = (0..self.quotient_chunks_count())
            .map(|_| vec![zeta])
            .collect();

        let (opened_values, proof) = state.pcs.open(
            preprocessed_data_and_opening_points
                .into_iter()
                .chain(trace_data_and_points_per_stage)
                .chain(once((&quotient_data, quotient_opening_points)))
                .collect(),
            state.challenger,
        );

        let mut opened_values = opened_values.into_iter();

        // maybe get values for the preprocessed columns
        let (preprocessed_local, preprocessed_next) = if proving_key.is_some() {
            let value = opened_values.next().unwrap();
            assert_eq!(value.len(), state.program.table_count());
            value
                .into_iter()
                .map(|v| (v[0].clone(), v[1].clone()))
                .unzip()
        } else {
            (
                vec![vec![]; state.program.table_count()],
                vec![vec![]; state.program.table_count()],
            )
        };

        // for each stage, for each table

        // output for each table, for each stage

        // get values for the traces
        let (traces_by_stage_local, traces_by_stage_next): (Vec<Vec<Vec<_>>>, Vec<Vec<Vec<_>>>) =
            state
                .processed_stages
                .iter()
                .fold(
                    vec![(vec![], vec![]); state.program.table_count()],
                    |mut traces_by_table, _| {
                        let mut values = opened_values.next().unwrap();
                        for ((local, next), v) in
                            traces_by_table.iter_mut().zip_eq(values.iter_mut())
                        {
                            assert_eq!(v.len(), 2);
                            next.push(v.pop().unwrap());
                            local.push(v.pop().unwrap());
                        }
                        traces_by_table
                    },
                )
                .into_iter()
                .unzip();

        // get values for the quotient
        let mut value = opened_values.next().unwrap().into_iter();
        let quotient_chunks: Vec<Vec<Vec<SC::Challenge>>> = self
            .tables
            .iter()
            .map(|i| {
                let log_quotient_degree = i.log_quotient_degree();
                let quotient_degree = 1 << log_quotient_degree;
                (&mut value)
                    .take(quotient_degree)
                    .map(|mut v| {
                        assert_eq!(v.len(), 1);
                        v.pop().unwrap()
                    })
                    .collect()
            })
            .collect();

        let log_degrees: Vec<_> = state
            .program
            .tables
            .iter()
            .map(|table| table.log_degree())
            .collect();

        let opened_values = preprocessed_local
            .into_iter()
            .zip_eq(preprocessed_next)
            .zip_eq(
                traces_by_stage_local
                    .into_iter()
                    .zip_eq(traces_by_stage_next),
            )
            .zip_eq(quotient_chunks)
            .zip_eq(log_degrees)
            .map(
                |(
                    (
                        (
                            (preprocessed_local, preprocessed_next),
                            (traces_by_stage_local, traces_by_stage_next),
                        ),
                        quotient_chunks,
                    ),
                    log_degree,
                )| ChipOpenedValues {
                    preprocessed_local,
                    preprocessed_next,
                    traces_by_stage_local,
                    traces_by_stage_next,
                    quotient_chunks,
                    log_degree,
                },
            )
            .collect();

        (opened_values, proof)
    }

    /// For a given stage, return the number of challenges required by the table with the most challenges.
    ///
    /// # Panics
    ///
    /// Panics if there are no tables.
    fn stage_challenge_count(&self, stage_id: u32) -> usize {
        self.tables
            .iter()
            .map(|table| {
                <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(
                    table.air, stage_id,
                )
            })
            .max()
            .unwrap()
    }
}

/// A sub-table to be proven, in the form of an air, a proving key, a stage 0 trace and stage 0 publics
struct Table<'a, SC, A> {
    air: &'a A,
    degree: usize,
    marker: PhantomData<SC>,
}

impl<
        'a,
        SC,
        #[cfg(debug_assertions)] A: for<'b> Air<crate::check_constraints::DebugConstraintBuilder<'b, Val<SC>>>,
        #[cfg(not(debug_assertions))] A,
    > Table<'a, SC, A>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<ProverConstraintFolder<'b, SC>>,
{
    fn log_degree(&self) -> usize {
        log2_strict_usize(self.degree)
    }

    fn trace_domain(&self, pcs: &SC::Pcs) -> Domain<SC> {
        pcs.natural_domain_for_degree(self.degree)
    }

    fn public_input_count_per_stage(&self) -> Vec<usize> {
        (0..<A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(self.air))
            .map(|stage| {
                <A as MultiStageAir<SymbolicAirBuilder<_>>>::stage_public_count(
                    self.air,
                    stage as u32,
                )
            })
            .collect()
    }

    fn log_quotient_degree(&self) -> usize {
        get_log_quotient_degree(self.air, &self.public_input_count_per_stage())
    }

    fn observe_instance(&self, challenger: &mut SC::Challenger) {
        challenger.observe(Val::<SC>::from_canonical_usize(self.log_degree()));
        // TODO: Might be best practice to include other instance data here; see verifier comment.
    }

    /// Compute the quotient domains and chunks for this table.
    /// * Arguments:
    ///    * `index`: The index of the table in the program. This is used as the index for this table in the mmcs.
    ///    * `state`: The current prover state.
    ///    * `proving_key`: The proving key, if it exists.
    ///    * `alpha`: The challenge value for the quotient polynomial.
    fn quotient_domains_and_chunks(
        &self,
        index: usize,
        state: &ProverState<SC, A>,
        proving_key: Option<&StarkProvingKey<SC>>,
        alpha: SC::Challenge,
    ) -> Vec<(Domain<SC>, DenseMatrix<Val<SC>>)> {
        let quotient_domain = self
            .trace_domain(state.pcs)
            .create_disjoint_domain(1 << (self.log_degree() + self.log_quotient_degree()));

        let preprocessed_on_quotient_domain = proving_key.map(|proving_key| {
            state.pcs.get_evaluations_on_domain(
                &proving_key.preprocessed_data,
                index,
                quotient_domain,
            )
        });

        let traces_on_quotient_domain = state
            .processed_stages
            .iter()
            .map(|s| {
                state
                    .pcs
                    .get_evaluations_on_domain(&s.prover_data, index, quotient_domain)
            })
            .collect();

        let challenges = state
            .processed_stages
            .iter()
            .map(|stage| stage.challenge_values.clone())
            .collect();

        let public_values_by_stage = state
            .processed_stages
            .iter()
            .map(|stage| stage.public_values[index].clone())
            .collect();

        let quotient_values = quotient_values(
            self.air,
            &public_values_by_stage,
            self.trace_domain(state.pcs),
            quotient_domain,
            preprocessed_on_quotient_domain,
            traces_on_quotient_domain,
            challenges,
            alpha,
        );

        let quotient_flat = RowMajorMatrix::new_col(quotient_values).flatten_to_base();

        let quotient_degree = 1 << self.log_quotient_degree();
        let quotient_chunks = quotient_domain.split_evals(quotient_degree, quotient_flat);
        let qc_domains = quotient_domain.split_domains(quotient_degree);
        qc_domains.into_iter().zip_eq(quotient_chunks).collect()
    }
}

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
    program: MultiTable<SC, A>,
    stage_0_traces: Vec<RowMajorMatrix<Val<SC>>>,
    stage_0_publics: Vec<Vec<Val<SC>>>,
    challenger: &mut SC::Challenger,
    next_stage_trace_callback: &C,
) -> Proof<SC>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'a> MultiStageAir<ProverConstraintFolder<'a, SC>>,
    C: NextStageTraceCallback<SC>,
{
    assert_eq!(stage_0_traces.len(), stage_0_publics.len());
    assert_eq!(stage_0_traces.len(), program.table_count());

    let stage_count = program.stage_count();

    let pcs = config.pcs();
    // let trace_domain = pcs.natural_domain_for_degree(degree);

    // Observe the instances.
    program.observe_instances(challenger);
    if let Some(proving_key) = proving_key {
        challenger.observe(proving_key.preprocessed_commit.clone())
    };

    let mut state = ProverState::new(&program, pcs, challenger);
    // assumption: stages are ordered like in airs in `state.tables`. Maybe we can enforce this better.
    let mut stage = Stage {
        id: 0,
        air_stages: stage_0_traces
            .into_iter()
            .zip(stage_0_publics)
            .map(|(trace, public_values)| AirStage {
                trace,
                public_values,
            })
            .collect(),
    };

    assert!(stage_count >= 1);
    // generate all stages starting from the second one based on the witgen callback
    for id in 1..stage_count {
        state = state.run_stage(stage);
        // get the challenges drawn at the end of the previous stage
        let local_challenges = &state.processed_stages.last().unwrap().challenge_values;
        let CallbackResult {
            trace,
            public_values,
            challenges,
        } = next_stage_trace_callback.compute_stage(id, local_challenges);

        // go to the next stage

        stage = Stage {
            id,
            air_stages: program
                .tables
                .iter()
                .map(|_| AirStage {
                    trace: todo!("split trace by air"),
                    public_values: todo!(),
                })
                .collect(),
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
    assert_eq!(state.processed_stages.len() as u32, stage_count);

    // with the witness complete, check the constraints
    // #[cfg(debug_assertions)]
    // crate::check_constraints::check_constraints(
    //     air,
    //     &air.preprocessed_trace()
    //         .unwrap_or(RowMajorMatrix::new(Default::default(), 0)),
    //     state.processed_stages.iter().map(|s| &s.trace).collect(),
    //     &state
    //         .processed_stages
    //         .iter()
    //         .map(|s| &s.public_values)
    //         .collect(),
    //     state
    //         .processed_stages
    //         .iter()
    //         .map(|s| &s.challenge_values)
    //         .collect(),
    // );

    let (quotient_commit, quotient_data) = program.commit_to_quotient(&mut state, proving_key);

    let commitments = Commitments {
        traces_by_stage: state
            .processed_stages
            .iter()
            .map(|s| s.commitment.clone())
            .collect(),
        quotient_chunks: quotient_commit,
    };

    let (opened_values, opening_proof) = program.open(&mut state, proving_key, quotient_data);

    Proof {
        commitments,
        opened_values,
        opening_proof,
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

struct ProverState<
    'a,
    SC,
    #[cfg(debug_assertions)] A: for<'b> Air<crate::check_constraints::DebugConstraintBuilder<'b, Val<SC>>>,
    #[cfg(not(debug_assertions))] A,
> where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<ProverConstraintFolder<'b, SC>>,
{
    pub(crate) program: &'a MultiTable<'a, SC, A>,
    pub(crate) processed_stages: Vec<ProcessedStage<SC>>,
    pub(crate) challenger: &'a mut SC::Challenger,
    pub(crate) pcs: &'a <SC>::Pcs,
}

impl<
        'a,
        SC,
        #[cfg(debug_assertions)] A: for<'b> Air<crate::check_constraints::DebugConstraintBuilder<'b, Val<SC>>>,
        #[cfg(not(debug_assertions))] A,
    > ProverState<'a, SC, A>
where
    SC: StarkGenericConfig,
    A: MultiStageAir<SymbolicAirBuilder<Val<SC>>>
        + for<'b> MultiStageAir<ProverConstraintFolder<'b, SC>>,
{
    pub(crate) fn new(
        program: &'a MultiTable<'a, SC, A>,
        pcs: &'a <SC as StarkGenericConfig>::Pcs,
        challenger: &'a mut <SC as StarkGenericConfig>::Challenger,
    ) -> Self {
        Self {
            program,
            processed_stages: Default::default(),
            challenger,
            pcs,
        }
    }

    pub(crate) fn run_stage(mut self, stage: Stage<Val<SC>>) -> Self {
        // #[cfg(debug_assertions)]
        // let trace = stage.trace.clone();

        let (commit_inputs, public_values): (_, Vec<_>) = stage
            .air_stages
            .into_iter()
            .map(|air_stage| {
                (
                    (
                        self.pcs.natural_domain_for_degree(air_stage.trace.height()),
                        air_stage.trace,
                    ),
                    air_stage.public_values,
                )
            })
            .unzip();

        // commit to the traces
        let (commitment, prover_data) =
            info_span!("commit to stage {stage} data").in_scope(|| self.pcs.commit(commit_inputs));

        self.challenger.observe(commitment.clone());
        // observe the public inputs. Is this fine to do after the trace commitment?
        for public_values in &public_values {
            println!("pi {public_values:?}");
            self.challenger.observe_slice(public_values);
        }

        // draw challenges
        let challenge_values = (0..self.program.stage_challenge_count(stage.id))
            .map(|_| self.challenger.sample())
            .collect();

        // update the state with the output of this stage
        self.processed_stages.push(ProcessedStage {
            public_values,
            prover_data,
            commitment,
            challenge_values,
            // #[cfg(debug_assertions)]
            // trace,
        });

        self
    }
}

pub struct AirStage<F> {
    /// the witness for this stage
    pub(crate) trace: RowMajorMatrix<F>,
    /// the public values for this stage
    pub(crate) public_values: Vec<F>,
}

pub struct Stage<F> {
    /// the id of this stage
    pub(crate) id: u32,
    /// the stage trace for each air
    air_stages: Vec<AirStage<F>>,
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
