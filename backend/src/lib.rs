use ast::{analyzed::Analyzed, asm_analysis::Machine};
use number::FieldElement;
use std::io;
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum Backend {
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2")]
    Halo2,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-aggr")]
    Halo2Aggr,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock")]
    Halo2Mock,
    // At the moment, this enum is empty without halo2, but it is always built
    // as part of the infrastructure to eventually support other backends.
}

/// Create a proof for a given PIL, fixed column values and witness column values
/// using the chosen backend.

pub type Proof = Vec<u8>;
pub type Params = Vec<u8>;

pub trait ProverWithParams {
    fn prove<T: FieldElement, R: io::Read>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
        params: R,
    ) -> Option<Proof>;

    fn generate_params<T: FieldElement>(size: usize) -> Params;
}

pub trait ProverWithoutParams {
    fn prove<T: FieldElement>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
    ) -> Option<Proof>;
}

pub trait ProverAggregationWithParams {
    fn prove<T: FieldElement, R1: io::Read, R2: io::Read>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
        proof: R1,
        params: R2,
    ) -> Proof;
}

#[cfg(feature = "halo2")]
pub struct Halo2Backend;

#[cfg(feature = "halo2")]
pub struct Halo2MockBackend;

#[cfg(feature = "halo2")]
pub struct Halo2AggregationBackend;

#[cfg(feature = "nova")]
pub struct NovaBackend;

#[cfg(feature = "halo2")]
impl ProverWithParams for Halo2Backend {
    fn prove<T: FieldElement, R: io::Read>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
        params: R,
    ) -> Option<Proof> {
        Some(halo2::prove_ast_read_params(pil, fixed, witness, params))
    }

    fn generate_params<T: FieldElement>(size: usize) -> Params {
        halo2::generate_params::<T>(size)
    }
}

#[cfg(feature = "halo2")]
impl ProverWithoutParams for Halo2MockBackend {
    fn prove<T: FieldElement>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
    ) -> Option<Proof> {
        halo2::mock_prove(pil, fixed, witness);
        None
    }
}

#[cfg(feature = "halo2")]
impl ProverAggregationWithParams for Halo2AggregationBackend {
    fn prove<T: FieldElement, R1: io::Read, R2: io::Read>(
        pil: &Analyzed<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
        proof: R1,
        params: R2,
    ) -> Proof {
        halo2::prove_aggr_read_proof_params(pil, fixed, witness, proof, params)
    }
}

#[cfg(feature = "nova")]
// TODO implement ProverWithoutParams, and remove dependent on main_machine
impl NovaBackend {
    pub fn prove<T: FieldElement>(
        pil: &Analyzed<T>,
        main_machine: &Machine<T>,
        fixed: Vec<(&str, Vec<T>)>,
        witness: Vec<(&str, Vec<T>)>,
        public_io: Vec<T>,
    ) -> Option<Proof> {
        Some(nova::prove_ast_read_params(
            pil,
            main_machine,
            fixed,
            witness,
            public_io,
        ));
        Some(vec![])
    }
}
