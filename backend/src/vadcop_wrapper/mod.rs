use std::{io, marker::PhantomData};

use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

pub(crate) struct VadCopWrapperFactory<F: FieldElement, B: BackendFactory<F>> {
    factory: B,
    _marker: PhantomData<F>,
}

impl<F: FieldElement, B: BackendFactory<F>> VadCopWrapperFactory<F, B> {
    pub(crate) const fn new(factory: B) -> Self {
        Self {
            factory,
            _marker: PhantomData,
        }
    }
}

impl<F: FieldElement, B: BackendFactory<F>> BackendFactory<F> for VadCopWrapperFactory<F, B> {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        output_dir: Option<&'a std::path::Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<'a, F> + 'a>, Error> {
        let backend: Box<dyn Backend<'a, F> + 'a> = self.factory.create(
            pil,
            fixed,
            output_dir,
            setup,
            verification_key,
            verification_app_key,
            backend_options,
        )?;
        Ok(Box::new(VadCopWrapper { backend }))
    }
}

pub(crate) struct VadCopWrapper<'a, F: FieldElement> {
    backend: Box<dyn Backend<'a, F> + 'a>,
}

// TODO: This just forwards to the backend for now. In the future this should:
// - Compute a verification key for each machine separately
// - Compute a proof for each machine separately
// - Verify all the machine proofs
// - Run additional checks on public values of the machine proofs
impl<'a, F: FieldElement> Backend<'a, F> for VadCopWrapper<'a, F> {
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        self.backend.prove(witness, prev_proof, witgen_callback)
    }

    fn verify(&self, _proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        self.backend.verify(_proof, instances)
    }

    fn export_setup(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.backend.export_setup(output)
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.backend.export_verification_key(output)
    }

    fn export_ethereum_verifier(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        self.backend.export_ethereum_verifier(output)
    }
}
