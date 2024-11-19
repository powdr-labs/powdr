use std::{collections::BTreeMap, io, marker::PhantomData, path::PathBuf, sync::Arc};

use constraint_checker::ConstraintChecker;
use halo2_proofs::dev::metadata::Constraint;
use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_backend_utils::{machine_fixed_columns, machine_witness_columns};
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

mod constraint_checker;

pub(crate) struct MockBackendFactory<F: FieldElement> {
    _marker: PhantomData<F>,
}

impl<F: FieldElement> MockBackendFactory<F> {
    pub(crate) const fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<F: FieldElement> BackendFactory<F> for MockBackendFactory<F> {
    fn create(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        _output_dir: Option<PathBuf>,
        _setup: Option<&mut dyn std::io::Read>,
        proving_key: Option<&mut dyn std::io::Read>,
        _verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        _backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<F>>, Error> {
        if proving_key.is_some() {
            unimplemented!();
        }
        if verification_app_key.is_some() {
            unimplemented!();
        }
        let machine_to_pil = powdr_backend_utils::split_pil(&pil);

        Ok(Box::new(MockBackend {
            machine_to_pil,
            fixed,
        }))
    }

    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        unreachable!()
    }
}

pub(crate) struct MockBackend<F> {
    machine_to_pil: BTreeMap<String, Analyzed<F>>,
    fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
}

impl<F: FieldElement> Backend<F> for MockBackend<F> {
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error> {
        if prev_proof.is_some() {
            unimplemented!();
        }

        for (machine, pil) in &self.machine_to_pil {
            log::info!("Checking machine {}", machine);

            let witness = machine_witness_columns(witness, pil, machine);
            let size = witness
                .iter()
                .map(|(_, witness)| witness.len())
                .unique()
                .exactly_one()
                .expect("All witness columns of a machine must have the same size")
                as DegreeType;
            let all_fixed = machine_fixed_columns(&self.fixed, &pil);
            let fixed = all_fixed.get(&size).unwrap();

            ConstraintChecker::new(&witness, fixed);
        }

        Ok(Vec::new())
    }

    fn verify(&self, _proof: &[u8], _instances: &[Vec<F>]) -> Result<(), Error> {
        unreachable!()
    }

    fn export_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unreachable!()
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        unreachable!()
    }

    fn export_ethereum_verifier(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        unimplemented!();
    }
}
