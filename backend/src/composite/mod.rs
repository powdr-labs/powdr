use std::{collections::BTreeMap, io, marker::PhantomData, path::PathBuf};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

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
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<'a, F> + 'a>, Error> {
        if setup.is_some() || verification_key.is_some() || verification_app_key.is_some() {
            unimplemented!();
        }

        let backend_by_machine = ["main"]
            .iter()
            .map(|machine_name| {
                let output_dir = output_dir
                    .clone()
                    .map(|output_dir| output_dir.join(machine_name));
                println!("Output dir: {:?}", output_dir);
                let backend = self.factory.create(
                    pil,
                    fixed,
                    output_dir,
                    // TODO: Handle setup, verification_key, verification_app_key
                    None,
                    None,
                    None,
                    backend_options.clone(),
                );
                backend.map(|backend| (machine_name.to_string(), backend))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;
        Ok(Box::new(CompositeBackend { backend_by_machine }))
    }

    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }
}

pub(crate) struct CompositeBackend<'a, F: FieldElement> {
    backend_by_machine: BTreeMap<String, Box<dyn Backend<'a, F> + 'a>>,
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
        self.backend_by_machine
            .get("main")
            .unwrap()
            .prove(witness, prev_proof, witgen_callback)
    }

    fn verify(&self, _proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        self.backend_by_machine
            .get("main")
            .unwrap()
            .verify(_proof, instances)
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
