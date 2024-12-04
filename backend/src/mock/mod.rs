use std::{
    collections::BTreeMap,
    hash::{DefaultHasher, Hash, Hasher},
    io,
    marker::PhantomData,
    path::PathBuf,
    sync::Arc,
};

use connection_constraint_checker::{Connection, ConnectionConstraintChecker};
use machine::Machine;
use polynomial_constraint_checker::PolynomialConstraintChecker;
use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed},
    parsed::visitor::AllChildren,
};
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};

use crate::{Backend, BackendFactory, BackendOptions, Error, Proof};

mod connection_constraint_checker;
mod machine;
mod polynomial_constraint_checker;

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
        let connections = Connection::get_all(&pil, &machine_to_pil);

        Ok(Box::new(MockBackend {
            machine_to_pil,
            fixed,
            connections,
        }))
    }

    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        unreachable!()
    }
}

pub(crate) struct MockBackend<F> {
    machine_to_pil: BTreeMap<String, Analyzed<F>>,
    fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
    connections: Vec<Connection<F>>,
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

        let challenges = self
            .machine_to_pil
            .values()
            .flat_map(|pil| pil.identities.iter())
            .flat_map(|identity| identity.all_children())
            .filter_map(|expr| match expr {
                AlgebraicExpression::Challenge(challenge) => {
                    // Use the hash of the ID as the challenge.
                    // This way, if the same challenge is used by different machines, they will
                    // have the same value.
                    let mut hasher = DefaultHasher::new();
                    challenge.id.hash(&mut hasher);
                    Some((challenge.id, F::from(hasher.finish())))
                }
                _ => None,
            })
            .collect::<BTreeMap<_, _>>();

        let machines = self
            .machine_to_pil
            // TODO: We should probably iterate in parallel, because Machine::try_new might generate
            // later-stage witnesses, which is expensive.
            // However, for now, doing it sequentially simplifies debugging.
            .iter()
            .filter_map(|(machine_name, pil)| {
                Machine::try_new(
                    machine_name.clone(),
                    witness,
                    &self.fixed,
                    pil,
                    &witgen_callback,
                    &challenges,
                )
            })
            .map(|machine| (machine.machine_name.clone(), machine))
            .collect::<BTreeMap<_, _>>();

        let is_ok = machines.values().all(|machine| {
            !PolynomialConstraintChecker::new(machine, &challenges)
                .check()
                .has_errors()
        }) && ConnectionConstraintChecker {
            connections: &self.connections,
            machines,
            challenges: &challenges,
        }
        .check()
        .is_ok();

        match is_ok {
            true => Ok(Vec::new()),
            false => Err(Error::BackendError("Constraint check failed".to_string())),
        }
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
