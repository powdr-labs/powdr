use std::{
    borrow::Cow,
    collections::{btree_map::Entry, BTreeMap},
    io::{self, Cursor, Read},
    marker::PhantomData,
    path::PathBuf,
    sync::{Arc, Mutex},
    thread,
};

use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};
use serde::{Deserialize, Serialize};
use split::{machine_fixed_columns, machine_witness_columns};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

use self::sub_prover::RunStatus;

mod split;

/// Maps each size to the corresponding verification key.
type VerificationKeyBySize = BTreeMap<usize, Vec<u8>>;

/// A composite verification key that contains a verification key for each machine separately.
#[derive(Serialize, Deserialize)]
struct CompositeVerificationKey {
    /// Verification keys for each machine (if available, otherwise None), sorted by machine name.
    verification_keys: Vec<Option<VerificationKeyBySize>>,
}

/// A proof for a single machine.
#[derive(Serialize, Deserialize)]
struct MachineProof {
    /// The (dynamic) size of the machine.
    size: usize,
    /// The proof for the machine.
    proof: Vec<u8>,
}

/// A composite proof that contains a proof for each machine separately, sorted by machine name.
#[derive(Serialize, Deserialize)]
struct CompositeProof {
    /// Machine proofs, sorted by machine name.
    proofs: Vec<MachineProof>,
}

pub(crate) struct CompositeBackendFactory<F: FieldElement, B: BackendFactory<F>> {
    factory: B,
    _marker: PhantomData<F>,
}

impl<F: FieldElement, B: BackendFactory<F>> CompositeBackendFactory<F, B> {
    pub(crate) const fn new(factory: B) -> Self {
        Self {
            factory,
            _marker: PhantomData,
        }
    }
}

impl<F: FieldElement, B: BackendFactory<F>> BackendFactory<F> for CompositeBackendFactory<F, B> {
    fn create<'a>(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<'a, F> + 'a>, Error> {
        if verification_app_key.is_some() {
            unimplemented!();
        }

        let pils = split::split_pil((*pil).clone());

        // Read the setup once (if any) to pass to all backends.
        let setup_bytes = setup.map(|setup| {
            let mut setup_data = Vec::new();
            setup.read_to_end(&mut setup_data).unwrap();
            setup_data
        });

        // Read all provided verification keys
        let verification_keys = verification_key
            .map(|verification_key| bincode::deserialize_from(verification_key).unwrap())
            .unwrap_or(CompositeVerificationKey {
                verification_keys: vec![None; pils.len()],
            })
            .verification_keys;

        log::info!(
            "Instantiating a composite backend with {} machines:",
            pils.len()
        );
        for (machine_name, pil) in pils.iter() {
            log_machine_stats(machine_name, pil)
        }

        let machine_data = pils
            .into_iter()
            .zip_eq(verification_keys.into_iter())
            .map(|((machine_name, pil), verification_key)| {
                let pil = Arc::new(pil);
                machine_fixed_columns(&fixed, &pil)
                    .into_iter()
                    .map(|(size, fixed)| {
                        let pil = set_size(pil.clone(), size as DegreeType);
                        // Set up readers for the setup and verification key
                        let mut setup_cursor = setup_bytes.as_ref().map(Cursor::new);
                        let setup = setup_cursor.as_mut().map(|cursor| cursor as &mut dyn Read);

                        let mut verification_key_cursor = verification_key
                            .as_ref()
                            .map(|keys| Cursor::new(keys.get(&size).unwrap()));
                        let verification_key = verification_key_cursor
                            .as_mut()
                            .map(|cursor| cursor as &mut dyn Read);

                        let output_dir = output_dir
                            .clone()
                            .map(|output_dir| output_dir.join(&machine_name));
                        if let Some(ref output_dir) = output_dir {
                            std::fs::create_dir_all(output_dir)?;
                        }
                        let fixed = Arc::new(fixed);
                        let backend = self.factory.create(
                            pil.clone(),
                            fixed,
                            output_dir,
                            setup,
                            verification_key,
                            // TODO: Handle verification_app_key
                            None,
                            backend_options.clone(),
                        );
                        backend.map(|backend| {
                            (
                                size,
                                MachineData {
                                    pil,
                                    backend: Mutex::new(backend),
                                },
                            )
                        })
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>()
                    .map(|backends| (machine_name, backends))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;

        Ok(Box::new(CompositeBackend { machine_data }))
    }

    fn generate_setup(&self, size: DegreeType, output: &mut dyn io::Write) -> Result<(), Error> {
        self.factory.generate_setup(size, output)
    }
}

fn log_machine_stats<T: FieldElement>(machine_name: &str, pil: &Analyzed<T>) {
    let num_witness_columns = pil.committed_polys_in_source_order().len();
    let num_fixed_columns = pil.constant_polys_in_source_order().len();
    let max_identity_degree = pil
        .identities_with_inlined_intermediate_polynomials()
        .iter()
        .map(|i| i.degree())
        .max()
        .unwrap_or(0);
    let uses_next_operator = pil.identities.iter().any(|i| i.contains_next_ref());
    // This assumes that we'll always at least once reference the current row
    let number_of_rotations = 1 + if uses_next_operator { 1 } else { 0 };
    let num_identities_by_kind = pil
        .identities
        .iter()
        .map(|i| i.kind)
        .counts()
        .into_iter()
        .collect::<BTreeMap<_, _>>();

    log::info!("* {}:", machine_name);
    log::info!("  * Number of witness columns: {}", num_witness_columns);
    log::info!("  * Number of fixed columns: {}", num_fixed_columns);
    log::info!("  * Maximum identity degree: {}", max_identity_degree);
    log::info!("  * Number of rotations: {}", number_of_rotations);
    log::info!("  * Number of identities:");
    for (kind, count) in num_identities_by_kind {
        log::info!("    * {:?}: {}", kind, count);
    }
}

struct MachineData<'a, F> {
    pil: Arc<Analyzed<F>>,
    // Mutex is needed because Backend is not Sync, so during proof we need to
    // ensure the type system that each backend is only used by one thread at a
    // time.
    backend: Mutex<Box<dyn Backend<'a, F> + 'a>>,
}

pub(crate) struct CompositeBackend<'a, F> {
    /// Maps each machine name to the corresponding machine data
    /// Note that it is essential that we use BTreeMap here to ensure that the machines are
    /// deterministically ordered.
    machine_data: BTreeMap<String, BTreeMap<usize, MachineData<'a, F>>>,
}

/// Makes sure that all columns in the machine PIL have the provided degree, cloning
/// the machine PIL if necessary. This is needed because backends other than `CompositeBackend`
/// typically expect that the degree is static.
///
/// # Panics
/// Panics if the machine PIL contains definitions with different degrees, or if the machine
/// already has a degree set that is different from the provided degree.
fn set_size<F: Clone>(pil: Arc<Analyzed<F>>, degree: DegreeType) -> Arc<Analyzed<F>> {
    let current_degrees = pil.degrees();
    assert!(
        current_degrees.len() <= 1,
        "Expected at most one degree within a machine"
    );

    match current_degrees.iter().next() {
        None => {
            // Clone the PIL and set the degree for all definitions
            let pil = (*pil).clone();
            let definitions = pil
                .definitions
                .into_iter()
                .map(|(name, (mut symbol, def))| {
                    symbol.degree = Some(degree);
                    (name, (symbol, def))
                })
                .collect();
            Arc::new(Analyzed { definitions, ..pil })
        }
        Some(existing_degree) => {
            // Keep the the PIL as is
            assert_eq!(
                existing_degree, &degree,
                "Expected all definitions within a machine to have the same degree"
            );
            pil
        }
    }
}

mod sub_prover {
    use std::{
        collections::BTreeMap,
        sync::{
            mpsc::{Receiver, SyncSender},
            Arc, Mutex,
        },
        thread::{Scope, ScopedJoinHandle},
    };

    use crate::{Backend, Error};
    use powdr_executor::witgen::WitgenCallback;
    use powdr_number::FieldElement;

    /// Runs a prover until it either completes or challenges the caller, in
    /// which case the execution can be resumed from the returned `SubProver`.
    pub fn run<'s, 'env, 'outer, F: FieldElement>(
        scope: &'s Scope<'s, 'env>,
        prover: &'env Mutex<Box<dyn Backend<'outer, F> + 'outer>>,
        witness: Vec<(String, Vec<F>)>,
    ) -> RunStatus<'s, F>
    where
        'outer: 'env,
    {
        let (challenge_sender, challenge_receiver) = std::sync::mpsc::sync_channel(0);
        let (response_sender, response_receiver) = std::sync::mpsc::sync_channel(0);

        let thread = scope.spawn(move || {
            // We must ensure the backend can call the callback from multiple
            // threads (needed by Plonky3, as it requires WitgenCallback to be
            // Sync). So we wrap the data in a Mutex.
            let callback_data = Mutex::new((challenge_sender, response_receiver, 0u8));
            let callback = WitgenCallback::<F>::new(Arc::new(move |_, challenge, stage| {
                // Locking guarantees the sequencing of the stages:
                let mut receiver_guard = callback_data.lock().unwrap();
                let (challenge_sender, response_receiver, expected_stage) = &mut *receiver_guard;

                assert_eq!(stage, *expected_stage);
                *expected_stage += 1;

                challenge_sender.send(challenge).unwrap();
                response_receiver.recv().unwrap()
            }));

            // TODO: this witness will be held in memory until the end of the
            // proof, even if it's not needed anymore. We should probably change
            // this API so the Vec is moved into the prover, and returned in the
            // callback and result.
            prover.lock().unwrap().prove(&witness, None, callback)
        });

        SubProver {
            thread,
            challenge_receiver,
            response_sender,
        }
        .wait()
    }

    pub enum RunStatus<'s, F: FieldElement> {
        Completed(Result<Vec<u8>, Error>),
        Challenged(SubProver<'s, F>, BTreeMap<u64, F>),
    }

    /// Runs a prover in its own thread, storing the state in between
    /// challenges.
    pub struct SubProver<'s, F: FieldElement> {
        pub thread: ScopedJoinHandle<'s, Result<Vec<u8>, Error>>,
        pub challenge_receiver: Receiver<BTreeMap<u64, F>>,
        pub response_sender: SyncSender<Vec<(String, Vec<F>)>>,
    }

    impl<'s, F: FieldElement> SubProver<'s, F> {
        pub fn resume(self, response: Vec<(String, Vec<F>)>) -> RunStatus<'s, F> {
            self.response_sender.send(response).unwrap();
            self.wait()
        }

        fn wait(self) -> RunStatus<'s, F> {
            match self.challenge_receiver.recv() {
                Ok(challenge) => RunStatus::Challenged(self, challenge),
                Err(_) => match self.thread.join() {
                    Ok(proof) => RunStatus::Completed(proof),
                    Err(err) => std::panic::resume_unwind(err),
                },
            }
        }
    }
}

fn accumulate_challenges<F: FieldElement>(into: &mut BTreeMap<u64, F>, from: BTreeMap<u64, F>) {
    for (k, v) in from {
        match into.entry(k) {
            Entry::Vacant(e) => {
                e.insert(v);
            }
            Entry::Occupied(e) => *e.into_mut() += v,
        }
    }
}

fn process_witness_for_machine<F: FieldElement>(
    machine: &str,
    machine_data: &BTreeMap<usize, MachineData<F>>,
    witness: &[(String, Vec<F>)],
) -> (Vec<(String, Vec<F>)>, usize) {
    // Pick any available PIL; they all contain the same witness columns
    let any_pil = &machine_data.values().next().unwrap().pil;
    let witness = machine_witness_columns(witness, any_pil, machine);
    let size = witness
        .iter()
        .map(|(_, witness)| witness.len())
        .unique()
        .exactly_one()
        .expect("All witness columns of a machine must have the same size");

    (witness, size)
}

fn time_stage<'a, F: FieldElement>(
    machine_name: &str,
    size: usize,
    stage: u8,
    stage_run: impl FnOnce() -> RunStatus<'a, F>,
) -> RunStatus<'a, F> {
    log::info!("== Proving machine: {machine_name} (size {size}), stage {stage}");
    let start_time = std::time::Instant::now();
    let status = stage_run();
    let elapsed = start_time.elapsed();
    log::info!("==> Proof stage computed in {elapsed:?}");

    status
}

// TODO: This just forwards to the backend for now. In the future this should:
// - Compute a verification key for each machine separately
// - Compute a proof for each machine separately
// - Verify all the machine proofs
// - Run additional checks on public values of the machine proofs
impl<'a, F: FieldElement> Backend<'a, F> for CompositeBackend<'a, F> {
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            unimplemented!();
        }

        // We use scoped threads to be able to share non 'static references.
        thread::scope(|scope| {
            let mut proofs_status = self
                .machine_data
                .iter()
                .map(|machine_entry| {
                    let (machine, machine_data) = machine_entry;
                    let (witness, size) =
                        process_witness_for_machine(machine, machine_data, witness);

                    let inner_machine_data = machine_data
                        .get(&size)
                        .expect("Machine does not support the given size");

                    let status = time_stage(machine, size, 0, || {
                        sub_prover::run(scope, &inner_machine_data.backend, witness)
                    });

                    (status, machine_entry, size)
                })
                .collect::<Vec<_>>();

            let mut proof_results = Vec::new();
            let mut witness = Cow::Borrowed(witness);
            for stage in 0.. {
                // Filter out proofs that have completed and accumulate the
                // challenges.
                let mut challenges = BTreeMap::new();
                let waiting_provers = std::mem::take(&mut proofs_status)
                    .into_iter()
                    .filter_map(|(status, machine_data, size)| match status {
                        sub_prover::RunStatus::Completed(result) => {
                            proof_results.push((result, size));
                            None
                        }
                        sub_prover::RunStatus::Challenged(sub_prover, c) => {
                            // Accumulate the challenges
                            accumulate_challenges(&mut challenges, c);
                            Some((sub_prover, machine_data))
                        }
                    })
                    .collect::<Vec<_>>();

                if waiting_provers.is_empty() {
                    break;
                }

                witness = witgen_callback
                    .next_stage_witness(&witness, challenges, stage)
                    .into();

                // Resume the waiting provers with the new witness
                proofs_status = waiting_provers
                    .into_iter()
                    .map(|(prover, machine_entry)| {
                        let (machine_name, machine_data) = machine_entry;
                        let (witness, size) =
                            process_witness_for_machine(machine_name, machine_data, &witness);

                        let status = time_stage(machine_name, size, stage + 1, move || {
                            prover.resume(witness)
                        });

                        (status, machine_entry, size)
                    })
                    .collect();
            }

            let proofs = proof_results
                .into_iter()
                .map(|(proof, size)| match proof {
                    Ok(proof) => Ok(MachineProof { size, proof }),
                    Err(e) => {
                        log::error!("==> Machine proof failed: {:?}", e);
                        Err(e)
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;

            let proof = CompositeProof { proofs };
            Ok(bincode::serialize(&proof).unwrap())
        })
    }

    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        let proof: CompositeProof = bincode::deserialize(proof).unwrap();
        for (machine_data, machine_proof) in
            self.machine_data.values().zip_eq(proof.proofs.into_iter())
        {
            machine_data
                .get(&machine_proof.size)
                .unwrap()
                .backend
                .lock()
                .unwrap()
                .verify(&machine_proof.proof, instances)?;
        }
        Ok(())
    }

    fn export_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        // All backend are the same, just pick the first
        self.machine_data
            .values()
            .next()
            .unwrap()
            .values()
            .next()
            .unwrap()
            .backend
            .lock()
            .unwrap()
            .export_setup(output)
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        let verification_key = CompositeVerificationKey {
            verification_keys: self
                .machine_data
                .values()
                .map(|machine_data| {
                    let verification_keys = machine_data
                        .iter()
                        .map(|(size, machine_data)| {
                            machine_data
                                .backend
                                .lock()
                                .unwrap()
                                .verification_key_bytes()
                                .map(|vk_bytes| (*size, vk_bytes))
                        })
                        .collect::<Result<_, _>>();

                    match verification_keys {
                        Ok(verification_keys) => Ok(Some(verification_keys)),
                        Err(Error::NoVerificationAvailable) => Ok(None),
                        Err(e) => Err(e),
                    }
                })
                .collect::<Result<_, _>>()?,
        };
        Ok(bincode::serialize(&verification_key).unwrap())
    }

    fn export_ethereum_verifier(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!();
    }
}
