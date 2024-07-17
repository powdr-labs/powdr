use std::{
    collections::BTreeMap,
    io::{self, Cursor, Read},
    marker::PhantomData,
    path::PathBuf,
    sync::Arc,
};

use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};
use serde::{Deserialize, Serialize};
use split::{machine_fixed_columns, machine_witness_columns};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

mod split;

/// A composite verification key that contains a verification key for each machine separately.
#[derive(Serialize, Deserialize)]
struct CompositeVerificationKey {
    /// Verification key for each machine (if available, otherwise None), sorted by machine name.
    verification_keys: Vec<Option<Vec<u8>>>,
}

/// A composite proof that contains a proof for each machine separately, sorted by machine name.
#[derive(Serialize, Deserialize)]
struct CompositeProof {
    /// Map from machine name to proof
    proofs: Vec<Vec<u8>>,
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
        fixed: Arc<Vec<(String, Vec<F>)>>,
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
                // Set up readers for the setup and verification key
                let mut setup_cursor = setup_bytes.as_ref().map(Cursor::new);
                let setup = setup_cursor.as_mut().map(|cursor| cursor as &mut dyn Read);

                let mut verification_key_cursor = verification_key.as_ref().map(Cursor::new);
                let verification_key = verification_key_cursor
                    .as_mut()
                    .map(|cursor| cursor as &mut dyn Read);

                let pil = Arc::new(pil);
                let output_dir = output_dir
                    .clone()
                    .map(|output_dir| output_dir.join(&machine_name));
                if let Some(ref output_dir) = output_dir {
                    std::fs::create_dir_all(output_dir)?;
                }
                let fixed = Arc::new(machine_fixed_columns(&fixed, &pil));
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
                backend.map(|backend| (machine_name.to_string(), MachineData { pil, backend }))
            })
            .collect::<Result<_, _>>()?;
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
    backend: Box<dyn Backend<'a, F> + 'a>,
}

pub(crate) struct CompositeBackend<'a, F> {
    /// Maps each machine name to the corresponding machine data
    /// Note that it is essential that we use BTreeMap here to ensure that the machines are
    /// deterministically ordered.
    machine_data: BTreeMap<String, MachineData<'a, F>>,
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

        let proof = CompositeProof {
            proofs: self
                .machine_data
                .iter()
                .map(|(machine, MachineData { pil, backend })| {
                    let witgen_callback = witgen_callback.clone().with_pil(pil.clone());

                    log::info!("== Proving machine: {} (size {})", machine, pil.degree());
                    log::debug!("PIL:\n{}", pil);

                    let start = std::time::Instant::now();

                    let witness = machine_witness_columns(witness, pil, machine);

                    let proof = backend.prove(&witness, None, witgen_callback);

                    match &proof {
                        Ok(proof) => {
                            log::info!(
                                "==> Machine proof of {} bytes computed in {:?}",
                                proof.len(),
                                start.elapsed()
                            );
                        }
                        Err(e) => {
                            log::error!("==> Machine proof failed: {:?}", e);
                        }
                    };
                    proof
                })
                .collect::<Result<_, _>>()?,
        };
        Ok(bincode::serialize(&proof).unwrap())
    }

    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        let proof: CompositeProof = bincode::deserialize(proof).unwrap();
        for (machine_data, machine_proof) in self.machine_data.values().zip_eq(proof.proofs) {
            machine_data.backend.verify(&machine_proof, instances)?;
        }
        Ok(())
    }

    fn export_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        // All backend are the same, just pick the first
        self.machine_data
            .values()
            .next()
            .unwrap()
            .backend
            .export_setup(output)
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        let verification_key = CompositeVerificationKey {
            verification_keys: self
                .machine_data
                .values()
                .map(|machine_data| {
                    let backend = machine_data.backend.as_ref();
                    let vk_bytes = backend.verification_key_bytes();
                    match vk_bytes {
                        Ok(vk_bytes) => Ok(Some(vk_bytes)),
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
