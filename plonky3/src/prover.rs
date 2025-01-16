use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use core::iter::{self, once};

use itertools::Itertools;
use p3_air::Air;
use p3_challenger::{CanObserve, CanSample, FieldChallenger};
use p3_commit::{Pcs as _, PolynomialSpace};
use p3_field::{AbstractExtensionField, AbstractField, PackedValue};
use p3_matrix::dense::{DenseMatrix, RowMajorMatrix};
use p3_matrix::Matrix;
use p3_maybe_rayon::prelude::*;
use p3_util::log2_strict_usize;
use tracing::{info_span, instrument};

use crate::circuit_builder::{generate_matrix, PowdrCircuit, PowdrTable};
use crate::params::{Challenge, Challenger, Pcs};
use crate::proof::{OpenedValues, StageOpenedValues};
use crate::symbolic_builder::{
    get_log_quotient_degree, get_max_constraint_degree, SymbolicAirBuilder,
};
use crate::traits::MultiStageAir;
use crate::{
    Com, Commitment, Commitments, FieldElementMap, PcsProof, PcsProverData, ProcessedStage, Proof,
    ProverConstraintFolder, ProverData, StarkProvingKey, TableOpenedValues,
    TableProvingKeyCollection,
};
use p3_uni_stark::{Domain, PackedChallenge, PackedVal, StarkGenericConfig, Val};

pub(crate) struct MultiTable<'a, T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub(crate) tables: BTreeMap<String, Table<'a, T>>,
}

impl<'a, T: FieldElementMap> MultiTable<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn table_count(&self) -> usize {
        self.tables.len()
    }

    fn table_names(&self) -> Vec<&String> {
        self.tables.keys().collect()
    }

    /// Returns the number of stages in the table with the most stages.
    ///
    /// # Panics
    ///
    /// Panics if there are no tables.
    fn stage_count(&self) -> u8 {
        self.tables
            .values()
            .map(|i| &i.air)
            .map(<_ as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count)
            .max()
            .expect("expected at least one table")
    }

    /// Observe the instance for each table.
    fn observe_instances(&self, challenger: &mut Challenger<T>) {
        for input in self.tables.values() {
            input.observe_instance(challenger);
        }
    }

    fn quotient_chunks_count(&self) -> usize {
        self.tables
            .values()
            .map(|table| 1 << table.log_quotient_degree())
            .sum()
    }

    /// Computes the quotient polynomials for each table and commits to them.
    ///
    /// # Side effects
    /// Samples a random challenge and observes the commitment.
    ///
    /// # Returns
    /// The commitment and the prover data.
    fn compute_and_commit_to_quotient(
        &self,
        state: &mut ProverState<'a, T>,
        proving_key: Option<&StarkProvingKey<T::Config>>,
    ) -> (Com<T::Config>, PcsProverData<T::Config>) {
        let alpha: Challenge<T> = state.challenger.sample_ext_element();

        // get the quotient domains and chunks for each table
        let quotient_domains_and_chunks: Vec<_> = self
            .tables
            .iter()
            .enumerate()
            .flat_map(|(index, (name, table))| {
                table.quotient_domains_and_chunks(
                    index,
                    state,
                    proving_key
                        .as_ref()
                        .and_then(|proving_key| proving_key.preprocessed.get(name)),
                    alpha,
                )
            })
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
    fn open(
        &self,
        state: &mut ProverState<T>,
        proving_key: Option<&StarkProvingKey<T::Config>>,
        quotient_data: PcsProverData<T::Config>,
    ) -> (OpenedValues<Challenge<T>>, PcsProof<T::Config>) {
        let zeta: Challenge<T> = state.challenger.sample();

        let preprocessed_data_and_opening_points = proving_key
            .as_ref()
            .map(|key| {
                self.tables.iter().filter_map(|(name, table)| {
                    key.preprocessed.get(name).map(|preprocessed| {
                        (
                            // pick the preprocessed data for this table in the correct size
                            &preprocessed[&(1 << table.log_degree())].prover_data,
                            vec![vec![
                                zeta,
                                table.trace_domain(state.pcs).next_point(zeta).unwrap(),
                            ]],
                        )
                    })
                })
            })
            .into_iter()
            .flatten();

        let trace_data_and_points_per_stage: Vec<(_, Vec<Vec<_>>)> = state
            .processed_stages
            .iter()
            .map(|processed_stage| {
                let points = self
                    .tables
                    .values()
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
                .chain(trace_data_and_points_per_stage)
                .chain(once((&quotient_data, quotient_opening_points)))
                .collect(),
            state.challenger,
        );

        let mut opened_values = opened_values.into_iter();

        // maybe get values for the preprocessed columns
        let preprocessed: Vec<_> = if let Some(proving_key) = proving_key {
            state
                .program
                .tables
                .keys()
                .map(|name| {
                    proving_key.preprocessed.contains_key(name).then(|| {
                        let value = opened_values.next().unwrap();
                        assert_eq!(value.len(), 1);
                        StageOpenedValues {
                            local: value[0][0].clone(),
                            next: value[0][1].clone(),
                        }
                    })
                })
                .collect()
        } else {
            vec![None; state.program.table_count()]
        };

        // get values for the traces
        let traces_by_table_by_stage: Vec<Vec<_>> = state.processed_stages.iter().fold(
            vec![vec![]; state.program.table_count()],
            |mut traces_by_table, _| {
                let values = opened_values.next().unwrap();
                for (values, v) in traces_by_table.iter_mut().zip_eq(values) {
                    let [local, next] = v.try_into().unwrap();

                    values.push(StageOpenedValues { next, local });
                }
                traces_by_table
            },
        );

        // get values for the quotient
        let mut value = opened_values.next().unwrap().into_iter();
        let quotient_chunks: Vec<Vec<Vec<Challenge<T>>>> = self
            .tables
            .values()
            .map(|i| {
                let log_quotient_degree = i.log_quotient_degree();
                let quotient_degree = 1 << log_quotient_degree;
                (&mut value)
                    .take(quotient_degree)
                    .map(|v| {
                        let [v] = v.try_into().unwrap();
                        v
                    })
                    .collect()
            })
            .collect();

        assert!(opened_values.next().is_none());

        let opened_values = state
            .program
            .tables
            .iter()
            .zip_eq(preprocessed)
            .zip_eq(traces_by_table_by_stage)
            .zip_eq(quotient_chunks)
            .map(
                |((((name, table), preprocessed), traces_by_stage), quotient_chunks)| {
                    (
                        name.clone(),
                        TableOpenedValues {
                            preprocessed,
                            traces_by_stage,
                            quotient_chunks,
                            log_degree: table.log_degree(),
                        },
                    )
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
    fn stage_challenge_count(&self, stage_id: u8) -> usize {
        self.tables
            .values()
            .map(|table| {
                <_ as MultiStageAir<SymbolicAirBuilder<_>>>::stage_challenge_count(
                    &table.air, stage_id,
                )
            })
            .max()
            .unwrap()
    }
}

/// A sub-table to be proven, in the form of an air and a degree
pub(crate) struct Table<'a, T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    air: PowdrTable<'a, T>,
    degree: usize,
}

impl<T: FieldElementMap> Table<'_, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn log_degree(&self) -> usize {
        log2_strict_usize(self.degree)
    }

    fn trace_domain(&self, pcs: &Pcs<T>) -> Domain<T::Config> {
        pcs.natural_domain_for_degree(self.degree)
    }

    fn public_input_count_per_stage(&self) -> Vec<usize> {
        (0..<_ as MultiStageAir<SymbolicAirBuilder<_>>>::stage_count(&self.air))
            .map(|stage| {
                <_ as MultiStageAir<SymbolicAirBuilder<_>>>::stage_public_count(&self.air, stage)
            })
            .collect()
    }

    fn log_quotient_degree(&self) -> usize {
        get_log_quotient_degree(&self.air, &self.public_input_count_per_stage())
    }

    fn max_constraint_degree(&self) -> usize {
        get_max_constraint_degree(&self.air, &self.public_input_count_per_stage())
    }

    fn observe_instance(&self, challenger: &mut Challenger<T>) {
        challenger.observe(Val::<T::Config>::from_canonical_usize(self.log_degree()));
        // TODO: Might be best practice to include other instance data here; see verifier comment.
    }

    /// Compute the quotient domains and chunks for this table.
    /// * Arguments:
    ///    * `table_index`: The index of the table in the program. This is used as the index for this table in the mmcs.
    ///    * `state`: The current prover state.
    ///    * `table_preprocessed_data`: The preprocessed data for this table, if it exists.
    ///    * `alpha`: The challenge value for the quotient polynomial.
    fn quotient_domains_and_chunks(
        &self,
        table_index: usize,
        state: &ProverState<T>,
        table_preprocessed_data: Option<&TableProvingKeyCollection<T::Config>>,
        alpha: Challenge<T>,
    ) -> impl Iterator<Item = (Domain<T::Config>, DenseMatrix<Val<T::Config>>)> {
        let quotient_domain = self
            .trace_domain(state.pcs)
            .create_disjoint_domain(1 << (self.log_degree() + self.log_quotient_degree()));

        let preprocessed_on_quotient_domain = table_preprocessed_data.map(|preprocessed| {
            state.pcs.get_evaluations_on_domain(
                &preprocessed[&(1 << self.log_degree())].prover_data,
                // the index is 0 because we committed to each preprocessed matrix alone, see setup
                0,
                quotient_domain,
            )
        });

        let traces_on_quotient_domain = state
            .processed_stages
            .iter()
            .map(|s| {
                state
                    .pcs
                    // the index is `table_index` because we committed to all table for a given stage together, and this is the `table_index`th table
                    .get_evaluations_on_domain(&s.prover_data, table_index, quotient_domain)
            })
            .collect();

        let challenges = state
            .processed_stages
            .iter()
            .map(|stage| stage.challenge_values.clone())
            .collect_vec();

        let public_values_by_stage = state
            .processed_stages
            .iter()
            .map(|stage| stage.public_values[table_index].clone())
            .collect_vec();

        let quotient_values = quotient_values::<T::Config, _, _>(
            &self.air,
            &public_values_by_stage,
            self.trace_domain(state.pcs),
            quotient_domain,
            preprocessed_on_quotient_domain,
            traces_on_quotient_domain,
            &challenges,
            alpha,
        );

        let quotient_flat = RowMajorMatrix::new_col(quotient_values).flatten_to_base();

        let quotient_degree = 1 << self.log_quotient_degree();
        let quotient_chunks = quotient_domain.split_evals(quotient_degree, quotient_flat);
        let qc_domains = quotient_domain.split_domains(quotient_degree);
        qc_domains.into_iter().zip_eq(quotient_chunks)
    }
}

/// Prove a program execution.
/// Note that `witness_by_machine` might not have all the machines, empty ones are expected
/// to be removed already.
#[instrument(skip_all)]
#[allow(clippy::multiple_bound_locations)] // cfg not supported in where clauses?
pub fn prove<T: FieldElementMap>(
    proving_key: Option<&StarkProvingKey<T::Config>>,
    program: &PowdrCircuit<T>,
    witness_by_machine: &mut BTreeMap<String, Vec<(String, Vec<T>)>>,
    challenger: &mut Challenger<T>,
) -> Proof<T::Config>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    let (tables, stage_0): (BTreeMap<_, _>, BTreeMap<_, _>) = witness_by_machine
        .iter()
        .map(|(name, columns)| {
            let constraint_system = &program.split.get(name).unwrap().1;
            let degree = columns[0].1.len();

            let table = Table {
                air: PowdrTable::new(constraint_system),
                degree,
            };

            // Sanity-check that the degree bound is not exceeded
            // If we don't panic here, Plonky3 panics with a bad error message when computing the quotient polynomial
            let degree_bound = T::degree_bound();
            let max_degree = table.max_constraint_degree();
            if max_degree > degree_bound {
                panic!(
                    "Table {} has a constraint of degree {} which exceeds the degree bound of {}.",
                    name, max_degree, degree_bound
                );
            }

            (
                (name.clone(), table),
                (
                    name.clone(),
                    AirStage {
                        trace: generate_matrix(
                            columns.iter().map(|(name, values)| (name, values.as_ref())),
                        ),
                        public_values: constraint_system.publics_by_stage[0]
                            .iter()
                            .map(|(_, column_name, _, row)| {
                                witness_by_machine
                                    .get(name)
                                    .unwrap()
                                    .iter()
                                    .find_map(|(n, v)| (n == column_name).then(|| v[*row]))
                                    .unwrap()
                                    .into_p3_field()
                            })
                            .collect(),
                    },
                ),
            )
        })
        .unzip();

    if tables.is_empty() {
        panic!("No tables to prove");
    }

    let multi_table = MultiTable { tables };

    let config = T::get_config();

    assert_eq!(stage_0.keys().collect_vec(), multi_table.table_names());

    let stage_count = multi_table.stage_count();

    let pcs = config.pcs();

    // observe the parts of the proving key which correspond to the sizes of the tables we are proving
    if let Some(proving_key) = proving_key {
        proving_key
            .preprocessed
            .iter()
            .filter_map(|(name, map)| {
                multi_table
                    .tables
                    .get(name)
                    .map(|table| &map[&table.degree].commitment)
            })
            .for_each(|commitment| challenger.observe(commitment.clone()));
    }

    multi_table.observe_instances(challenger);

    let mut state = ProverState::new(&multi_table, pcs, challenger);

    // run the first stage
    state = state.run_stage(Stage {
        id: 0,
        air_stages: stage_0,
    });

    assert!(stage_count >= 1);
    // generate all stages starting from the second one based on the witgen callback
    for stage_id in 1..stage_count {
        // get the challenges drawn at the end of the previous stage
        let local_challenges = &state.processed_stages.last().unwrap().challenge_values;
        let CallbackResult { air_stages } =
            program.compute_stage(stage_id, local_challenges, witness_by_machine);

        assert_eq!(air_stages.len(), multi_table.table_count());

        // go to the next stage
        state = state.run_stage(Stage {
            id: stage_id,
            air_stages,
        });
    }

    // sanity check that the last stage did not create any challenges
    assert!(state
        .processed_stages
        .last()
        .unwrap()
        .challenge_values
        .is_empty());
    // sanity check that we processed as many stages as expected
    assert_eq!(state.processed_stages.len() as u8, stage_count);

    let (quotient_commit, quotient_data) =
        multi_table.compute_and_commit_to_quotient(&mut state, proving_key);

    let commitments = Commitments {
        traces_by_stage: state
            .processed_stages
            .iter()
            .map(|s| s.commitment.clone())
            .collect(),
        quotient_chunks: quotient_commit,
    };

    let (opened_values, opening_proof) = multi_table.open(&mut state, proving_key, quotient_data);

    Proof {
        commitments,
        opened_values,
        opening_proof,
    }
}

#[allow(clippy::too_many_arguments)]
#[instrument(name = "compute quotient polynomial", skip_all)]
fn quotient_values<SC, A, Mat>(
    air: &A,
    public_values_by_stage: &[Vec<Val<SC>>],
    trace_domain: Domain<SC>,
    quotient_domain: Domain<SC>,
    preprocessed_on_quotient_domain: Option<Mat>,
    traces_on_quotient_domain: Vec<Mat>,
    challenges: &[Vec<Val<SC>>],
    alpha: SC::Challenge,
) -> Vec<SC::Challenge>
where
    SC: StarkGenericConfig,
    A: for<'a> Air<ProverConstraintFolder<'a, SC>>,
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
                    .map(|preprocessed_on_quotient_domain| {
                        iter::empty()
                            .chain(preprocessed_on_quotient_domain.vertically_packed_row(i_start))
                            .chain(
                                preprocessed_on_quotient_domain
                                    .vertically_packed_row(i_start + next_step),
                            )
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
                .collect_vec();

            let accumulator = PackedChallenge::<SC>::zero();
            let mut folder = ProverConstraintFolder {
                challenges,
                traces_by_stage: traces_by_stage
                    .iter()
                    .map(|trace| trace.as_view())
                    .collect(),
                preprocessed: preprocessed.as_view(),
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

struct ProverState<'a, T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub(crate) program: &'a MultiTable<'a, T>,
    pub(crate) processed_stages: Vec<ProcessedStage<T::Config>>,
    pub(crate) challenger: &'a mut Challenger<T>,
    pub(crate) pcs: &'a Pcs<T>,
}

impl<'a, T: FieldElementMap> ProverState<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub(crate) fn new(
        program: &'a MultiTable<'a, T>,
        pcs: &'a <T::Config as StarkGenericConfig>::Pcs,
        challenger: &'a mut <T::Config as StarkGenericConfig>::Challenger,
    ) -> Self {
        Self {
            program,
            processed_stages: Default::default(),
            challenger,
            pcs,
        }
    }

    pub(crate) fn run_stage(mut self, stage: Stage<Val<T::Config>>) -> Self {
        let (commit_inputs, public_values): (_, Vec<_>) = stage
            .air_stages
            .into_values()
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

        // observe the public inputs
        for public_values in &public_values {
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
    pub(crate) id: u8,
    /// the stage trace for each air
    air_stages: BTreeMap<String, AirStage<F>>,
}

pub struct CallbackResult<T> {
    /// the next stage for each air
    pub(crate) air_stages: BTreeMap<String, AirStage<T>>,
}
