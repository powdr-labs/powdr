use std::iter::{once, repeat};
use std::time::Instant;

use crate::{pilstark, Backend, BackendFactory, Error};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{BigInt, DegreeType, FieldElement, GoldilocksField};

use starky::{
    merklehash::MerkleTreeGL,
    polsarray::{PolKind, PolsArray},
    stark_gen::StarkProof,
    stark_setup::StarkSetup,
    stark_verify::stark_verify,
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
        if verification_key.is_some() {
            return Err(Error::NoVerificationAvailable);
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

        Ok(Box::new(EStark {
            pil,
            fixed,
            params: StarkStruct {
                nBits: n_bits,
                nBitsExt: n_bits_ext,
                nQueries: 2,
                verificationHashType: "GL".to_owned(),
                steps,
            },
        }))
    }
}

pub struct EStark<'a, F: FieldElement> {
    pil: &'a Analyzed<F>,
    fixed: &'a [(String, Vec<F>)],
    params: StarkStruct,
}

impl<'a, F: FieldElement> Backend<'a, F> for EStark<'a, F> {
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

        let degree = self.pil.degree();

        let mut pil: PIL = pilstark::json_exporter::export(self.pil);

        // TODO starky requires a fixed column with the equivalent
        // semantics to Polygon zkEVM's `L1` column.
        // It takes the name of that column via the API.
        // Powdr generated PIL will always have `main.first_step`,
        // but directly given PIL may not have it.
        // This is a hack to inject such column if it doesn't exist.
        // It should be eventually improved.
        let mut fixed = self.fixed.to_vec();
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

        let const_pols = to_starky_pols_array(&fixed, &pil, PolKind::Constant);
        let cm_pols = to_starky_pols_array(witness, &pil, PolKind::Commit);

        let mut setup = StarkSetup::<MerkleTreeGL>::new(
            &const_pols,
            &mut pil,
            &self.params,
            Some("main.first_step".to_string()),
        )
        .unwrap();

        let start = Instant::now();
        let starkproof = StarkProof::<MerkleTreeGL>::stark_gen::<TranscriptGL>(
            cm_pols,
            const_pols,
            &setup.const_tree,
            &setup.starkinfo,
            &setup.program,
            &pil,
            &self.params,
            "",
        )
        .unwrap();
        let duration = start.elapsed();

        log::info!("Proof done in: {:?}", duration);

        let valid = stark_verify::<MerkleTreeGL, TranscriptGL>(
            &starkproof,
            &setup.const_root,
            &setup.starkinfo,
            &self.params,
            &mut setup.program,
        )
        .map_err(|e| Error::BackendError(e.to_string()))?;

        if valid {
            Ok(serde_json::to_vec(&starkproof).unwrap())
        } else {
            Err(Error::BackendError("Proof verification failed".to_string()))
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
