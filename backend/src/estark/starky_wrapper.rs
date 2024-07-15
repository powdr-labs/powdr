use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use crate::{Backend, BackendFactory, BackendOptions, Error};
use powdr_ast::analyzed::Analyzed;
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};

use starky::{
    merklehash::MerkleTreeGL,
    polsarray::{PolKind, PolsArray},
    stark_gen::StarkProof,
    stark_setup::StarkSetup,
    stark_verify::stark_verify,
    traits::FieldExtension,
    transcript::TranscriptGL,
    types::{StarkStruct, PIL},
};

use super::{create_stark_struct, first_step_fixup, ProofType};

pub struct Factory;

impl<F: FieldElement> BackendFactory<F> for Factory {
    fn create<'a>(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, Vec<F>)>>,
        _output_dir: Option<PathBuf>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
        verification_app_key: Option<&mut dyn std::io::Read>,
        options: BackendOptions,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if F::modulus().to_arbitrary_integer() != GoldilocksField::modulus().to_arbitrary_integer()
        {
            unimplemented!("eSTARK is only implemented for Goldilocks field");
        }

        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }

        if verification_app_key.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        if pil.degrees().len() > 1 {
            return Err(Error::NoVariableDegreeAvailable);
        }

        let proof_type: ProofType = ProofType::from(options);

        let params = create_stark_struct(pil.degree(), proof_type.hash_type());

        let (pil_json, fixed) = first_step_fixup(&pil, fixed);

        let const_pols = to_starky_pols_array(&fixed, &pil_json, PolKind::Constant);

        let setup = if let Some(vkey) = verification_key {
            serde_json::from_reader(vkey).unwrap()
        } else {
            create_stark_setup(pil_json.clone(), &const_pols, &params)
        };

        Ok(Box::new(EStark {
            fixed,
            pil_json,
            params,
            setup,
            proof_type,
        }))
    }
}

fn create_stark_setup(
    mut pil: PIL,
    const_pols: &PolsArray,
    params: &StarkStruct,
) -> StarkSetup<MerkleTreeGL> {
    StarkSetup::<MerkleTreeGL>::new(
        const_pols,
        &mut pil,
        params,
        Some("main.first_step".to_string()),
    )
    .unwrap()
}

pub struct EStark<F: FieldElement> {
    fixed: Arc<Vec<(String, Vec<F>)>>,
    pil_json: PIL,
    params: StarkStruct,
    // eSTARK calls it setup, but it works similarly to a verification key and depends only on the
    // constants and circuit.
    setup: StarkSetup<MerkleTreeGL>,
    proof_type: ProofType,
}

impl<F: FieldElement> EStark<F> {
    fn verify_stark_gl_with_publics(
        &self,
        proof: &StarkProof<MerkleTreeGL>,
        instances: &[Vec<F>],
    ) -> Result<(), Error> {
        assert!(matches!(self.proof_type, ProofType::StarkGL));

        assert_eq!(instances.len(), 1);
        let proof_publics = proof
            .publics
            .iter()
            .map(|x| F::from(x.as_int()))
            .collect::<Vec<_>>();
        assert_eq!(instances[0], proof_publics);

        self.verify_stark_gl(proof)
    }

    fn verify_stark_gl(&self, proof: &StarkProof<MerkleTreeGL>) -> Result<(), Error> {
        assert!(matches!(self.proof_type, ProofType::StarkGL));

        match stark_verify::<MerkleTreeGL, TranscriptGL>(
            proof,
            &self.setup.const_root,
            &self.setup.starkinfo,
            &self.params,
            &self.setup.program,
        ) {
            Ok(true) => Ok(()),
            Ok(false) => Err(Error::BackendError("Proof is invalid".to_string())),
            Err(e) => Err(Error::BackendError(e.to_string())),
        }
    }

    fn prove_stark_gl(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<crate::Proof>,
        // TODO: Implement challenges
        _witgen_callback: WitgenCallback<F>,
    ) -> Result<crate::Proof, Error> {
        assert!(matches!(self.proof_type, ProofType::StarkGL));

        // TODO this should be supported by GL -> GL compression
        if prev_proof.is_some() {
            return Err(Error::NoAggregationAvailable);
        }
        if witness.is_empty() {
            return Err(Error::EmptyWitness);
        }

        log::info!("Creating eSTARK proof.");

        let cm_pols = to_starky_pols_array(witness, &self.pil_json, PolKind::Commit);
        let start = Instant::now();

        // TODO it would be good not to recompute this here
        let const_pols = to_starky_pols_array(&self.fixed, &self.pil_json, PolKind::Constant);

        let starkproof = StarkProof::<MerkleTreeGL>::stark_gen::<TranscriptGL>(
            cm_pols,
            const_pols,
            &self.setup.const_tree,
            &self.setup.starkinfo,
            &self.setup.program,
            &self.pil_json,
            &self.params,
            "",
        );

        let starkproof = match starkproof {
            Ok(p) => p,
            Err(e) => return Err(Error::BackendError(e.to_string())),
        };

        let duration = start.elapsed();

        log::info!("Proof done in: {:?}", duration);

        match self.verify_stark_gl(&starkproof) {
            Ok(_) => Ok(serde_json::to_string(&starkproof).unwrap().into_bytes()),
            Err(e) => Err(e),
        }
    }
}

impl<'a, F: FieldElement> Backend<'a, F> for EStark<F> {
    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        match self.proof_type {
            ProofType::StarkGL => {
                let proof: StarkProof<MerkleTreeGL> =
                    serde_json::from_str(&String::from_utf8(proof.to_vec()).unwrap()).unwrap();
                self.verify_stark_gl_with_publics(&proof, instances)
            }
            ProofType::StarkBN => unimplemented!(),
            ProofType::SnarkBN => unimplemented!(),
        }
    }

    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<crate::Proof>,
        // TODO: Implement challenges
        _witgen_callback: WitgenCallback<F>,
    ) -> Result<crate::Proof, Error> {
        match self.proof_type {
            ProofType::StarkGL => self.prove_stark_gl(witness, prev_proof, _witgen_callback),
            ProofType::StarkBN => unimplemented!(),
            ProofType::SnarkBN => unimplemented!(),
        }
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        serde_json::to_vec(&self.setup)
            .map_err(|_| Error::BackendError("Could not serialize verification key".to_string()))
    }
}

fn to_starky_pols_array<F: FieldElement>(
    array: &[(String, Vec<F>)],
    pil: &PIL,
    kind: PolKind,
) -> PolsArray {
    let mut output = PolsArray::new(pil, kind);
    assert_eq!(output.array.len(), array.len());
    for ((_, from), to) in array.iter().zip(output.array.iter_mut()) {
        assert_eq!(from.len(), to.len());

        for (f, t) in from.iter().zip(to.iter_mut()) {
            *t = TryInto::<u64>::try_into(f.to_integer().to_arbitrary_integer())
                .unwrap()
                .into();
        }
    }

    output
}
