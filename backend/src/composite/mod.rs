use std::{collections::BTreeMap, io, marker::PhantomData, path::PathBuf, sync::Arc};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement, VariablySizedColumns};
use serde::{Deserialize, Serialize};
use split::select_machine_columns;

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

mod split;

/// A composite proof that contains a proof for each machine separately.
#[derive(Serialize, Deserialize)]
struct CompositeProof {
    /// Map from machine name to proof
    proofs: BTreeMap<String, Vec<u8>>,
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
        fixed: Arc<VariablySizedColumns<F>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<'a, F> + 'a>, Error> {
        if setup.is_some() || verification_key.is_some() || verification_app_key.is_some() {
            unimplemented!();
        }

        // TODO: Handle multiple sizes.
        let fixed = Arc::new(
            fixed
                .get_only_size_cloned()
                .map_err(|_| Error::NoVariableDegreeAvailable)?,
        );

        let per_machine_data = split::split_pil((*pil).clone())
            .into_iter()
            .map(|(machine_name, pil)| {
                let pil = Arc::new(pil);
                let output_dir = output_dir
                    .clone()
                    .map(|output_dir| output_dir.join(&machine_name));
                if let Some(ref output_dir) = output_dir {
                    std::fs::create_dir_all(output_dir)?;
                }
                let fixed = Arc::new(
                    select_machine_columns(
                        &fixed,
                        pil.constant_polys_in_source_order()
                            .into_iter()
                            .map(|(symbol, _)| symbol),
                    )
                    .into_iter()
                    .map(|(column_name, values)| {
                        (column_name, [(values.len(), values)].into_iter().collect())
                    })
                    .into(),
                );
                let backend = self.factory.create(
                    pil.clone(),
                    fixed,
                    output_dir,
                    // TODO: Handle setup, verification_key, verification_app_key
                    None,
                    None,
                    None,
                    backend_options.clone(),
                );
                backend.map(|backend| (machine_name.to_string(), MachineData { pil, backend }))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;
        Ok(Box::new(CompositeBackend {
            machine_data: per_machine_data,
        }))
    }

    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }
}

struct MachineData<'a, F> {
    pil: Arc<Analyzed<F>>,
    backend: Box<dyn Backend<'a, F> + 'a>,
}

pub(crate) struct CompositeBackend<'a, F> {
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

                    log::info!("== Proving machine: {}", machine);
                    log::debug!("PIL:\n{}", pil);

                    let witness = select_machine_columns(
                        witness,
                        pil.committed_polys_in_source_order()
                            .into_iter()
                            .map(|(symbol, _)| symbol),
                    );

                    backend
                        .prove(&witness, None, witgen_callback)
                        .map(|proof| (machine.clone(), proof))
                })
                .collect::<Result<_, _>>()?,
        };
        Ok(serde_json::to_vec(&proof).unwrap())
    }

    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        let proof: CompositeProof = serde_json::from_slice(proof).unwrap();
        for (machine, machine_proof) in proof.proofs {
            let machine_data = self
                .machine_data
                .get(&machine)
                .ok_or_else(|| Error::BackendError(format!("Unknown machine: {machine}")))?;
            machine_data.backend.verify(&machine_proof, instances)?;
        }
        Ok(())
    }

    fn export_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!()
    }

    fn export_verification_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!();
    }

    fn export_ethereum_verifier(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!();
    }
}
