use std::{
    collections::BTreeMap,
    io::{self, Cursor, Read},
    marker::PhantomData,
    path::PathBuf,
    sync::Arc,
};

use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};
use serde::{Deserialize, Serialize};
use split::{machine_fixed_columns, machine_witness_columns};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

mod split;

/// A composite verification key that contains a verification key for each machine separately.
#[derive(Serialize, Deserialize)]
struct CompositeVerificationKey {
    /// Verification key for each machine (if available, otherwise None), sorted by machine name.
    verification_keys: Vec<Option<BTreeMap<usize, Vec<u8>>>>,
}

/// A composite proof that contains a proof for each machine separately, sorted by machine name.
#[derive(Serialize, Deserialize)]
struct CompositeProof {
    /// Map from machine name to proof
    proofs: Vec<Vec<u8>>,
    sizes: Vec<usize>,
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

        let machine_data = pils
            .into_iter()
            .zip_eq(verification_keys.into_iter())
            .map(|((machine_name, pil), verification_key)| {
                let pil = Arc::new(pil);
                machine_fixed_columns(&fixed, &pil)
                    .into_iter()
                    .map(|(size, fixed)| {
                        let pil = Arc::new(set_size(&pil, size as DegreeType));
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
                        backend.map(|backend| (size, MachineData { pil, backend }))
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

struct MachineData<'a, F> {
    pil: Arc<Analyzed<F>>,
    backend: Box<dyn Backend<'a, F> + 'a>,
}

pub(crate) struct CompositeBackend<'a, F> {
    /// Maps each machine name to the corresponding machine data
    /// Note that it is essential that we use BTreeMap here to ensure that the machines are
    /// deterministically ordered.
    machine_data: BTreeMap<String, BTreeMap<usize, MachineData<'a, F>>>,
}

fn set_size<F: Clone>(pil: &Analyzed<F>, degree: DegreeType) -> Analyzed<F> {
    let pil = pil.clone();
    let definitions = pil
        .definitions
        .into_iter()
        .map(|(name, (mut symbol, def))| {
            symbol.degree = Some(degree);
            (name, (symbol, def))
        })
        .collect();
    Analyzed { definitions, ..pil }
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

        let (sizes, proofs) = self
            .machine_data
            .iter()
            .map(|(machine, machine_data)| {
                let any_pil = &machine_data.values().next().unwrap().pil;
                let witness = machine_witness_columns(witness, any_pil, machine);
                let size = witness
                    .iter()
                    .map(|(_, witness)| witness.len())
                    .unique()
                    .exactly_one()
                    .expect("All witness columns of a machine must have the same size");
                let machine_data = machine_data
                    .get(&size)
                    .expect("Machine does not support the given size");
                let witgen_callback = witgen_callback.clone().with_pil(machine_data.pil.clone());

                log::info!("== Proving machine: {} (size {})", machine, size);
                log::debug!("PIL:\n{}", machine_data.pil);

                machine_data
                    .backend
                    .prove(&witness, None, witgen_callback)
                    .map(|proof| (size, proof))
            })
            .collect::<Result<Vec<(usize, Vec<u8>)>, _>>()?
            .into_iter()
            .unzip();

        let proof = CompositeProof { sizes, proofs };
        Ok(bincode::serialize(&proof).unwrap())
    }

    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        let proof: CompositeProof = bincode::deserialize(proof).unwrap();
        for (machine_data, (machine_proof, size)) in self
            .machine_data
            .values()
            .zip_eq(proof.proofs.into_iter().zip_eq(proof.sizes))
        {
            machine_data
                .get(&size)
                .unwrap()
                .backend
                .verify(&machine_proof, instances)?;
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
