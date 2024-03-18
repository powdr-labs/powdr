use std::io;
use std::iter::{once, repeat};
use std::time::Instant;

use crate::{pilstark, Backend, BackendFactory, Error};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{DegreeType, FieldElement, GoldilocksField, LargeInt};

use starky::{
    merklehash::MerkleTreeGL,
    polsarray::{PolKind, PolsArray},
    stark_gen::StarkProof,
    stark_setup::StarkSetup,
    stark_verify::stark_verify,
    traits::FieldExtension,
    transcript::TranscriptGL,
    types::{StarkStruct, Step, PIL},
};

pub struct EStarkFactory;

impl<F: FieldElement> BackendFactory<F> for EStarkFactory {
    fn create<'a>(
        &self,
        pil: &'a Analyzed<F>,
        fixed: &'a [(String, Vec<F>)],
        _output_dir: Option<&std::path::Path>,
        setup: Option<&mut dyn std::io::Read>,
        verification_key: Option<&mut dyn std::io::Read>,
    ) -> Result<Box<dyn crate::Backend<'a, F> + 'a>, Error> {
        if F::modulus().to_arbitrary_integer() != GoldilocksField::modulus().to_arbitrary_integer()
        {
            unimplemented!("eSTARK is only implemented for Goldilocks field");
        }

        if setup.is_some() {
            return Err(Error::NoSetupAvailable);
        }

        let degree = pil.degree();
        assert!(degree > 1);
        let n_bits = (DegreeType::BITS - (degree - 1).leading_zeros()) as usize;
        let n_bits_ext = n_bits + 1;

        let steps = (2..=n_bits_ext)
            .rev()
            .step_by(4)
            .map(|b| Step { nBits: b })
            .collect();

        let params = StarkStruct {
            nBits: n_bits,
            nBitsExt: n_bits_ext,
            nQueries: 2,
            verificationHashType: "GL".to_owned(),
            steps,
        };

        let (pil_json, fixed) = pil_json(pil, fixed);
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
        }))
    }
}

fn pil_json<'a, F: FieldElement>(
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
) -> (PIL, Vec<(String, Vec<F>)>) {
    let degree = pil.degree();

    let mut pil: PIL = pilstark::json_exporter::export(pil);

    // TODO starky requires a fixed column with the equivalent
    // semantics to Polygon zkEVM's `L1` column.
    // It takes the name of that column via the API.
    // Powdr generated PIL will always have `main.first_step`,
    // but directly given PIL may not have it.
    // This is a hack to inject such column if it doesn't exist.
    // It should be eventually improved.
    let mut fixed = fixed.to_vec();
    if !fixed.iter().any(|(k, _)| k == "main.first_step") {
        use starky::types::Reference;
        pil.nConstants += 1;
        pil.references.insert(
            "main.first_step".to_string(),
            Reference {
                polType: None,
                type_: "constP".to_string(),
                id: fixed.len(),
                polDeg: degree as usize,
                isArray: false,
                elementType: None,
                len: None,
            },
        );
        fixed.push((
            "main.first_step".to_string(),
            once(F::one())
                .chain(repeat(F::zero()))
                .take(degree as usize)
                .collect(),
        ));
    }

    (pil, fixed)
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
    fixed: Vec<(String, Vec<F>)>,
    pil_json: PIL,
    params: StarkStruct,
    // eSTARK calls it setup, but it works similarly to a verification key and depends only on the
    // constants and circuit.
    setup: StarkSetup<MerkleTreeGL>,
}

impl<F: FieldElement> EStark<F> {
    fn verify_stark_with_publics(
        &self,
        proof: &StarkProof<MerkleTreeGL>,
        instances: &[Vec<F>],
    ) -> Result<(), Error> {
        assert_eq!(instances.len(), 1);
        let proof_publics = proof
            .publics
            .iter()
            .map(|x| F::from(x.as_int()))
            .collect::<Vec<_>>();
        assert_eq!(instances[0], proof_publics);

        self.verify_stark(proof)
    }

    fn verify_stark(&self, proof: &StarkProof<MerkleTreeGL>) -> Result<(), Error> {
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
}

impl<'a, F: FieldElement> Backend<'a, F> for EStark<F> {
    fn verify(&self, proof: &[u8], instances: &[Vec<F>]) -> Result<(), Error> {
        let proof: StarkProof<MerkleTreeGL> =
            serde_json::from_str(&String::from_utf8(proof.to_vec()).unwrap()).unwrap();
        self.verify_stark_with_publics(&proof, instances)
    }

    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        prev_proof: Option<crate::Proof>,
    ) -> Result<crate::Proof, Error> {
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

        match self.verify_stark(&starkproof) {
            Ok(_) => Ok(serde_json::to_string(&starkproof).unwrap().into_bytes()),
            Err(e) => Err(e),
        }
    }

    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        match serde_json::to_writer(output, &self.setup) {
            Ok(_) => Ok(()),
            Err(_) => Err(Error::BackendError(
                "Could not export verification key".to_string(),
            )),
        }
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
