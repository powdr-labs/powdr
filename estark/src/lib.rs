mod json_exporter;

use ast::analyzed::Analyzed;
use number::{BigInt, DegreeType, FieldElement, GoldilocksField};

use starky::{
    merklehash::MerkleTreeGL,
    polsarray::{PolKind, PolsArray},
    stark_gen::StarkProof,
    stark_setup::StarkSetup,
    stark_verify::stark_verify,
    transcript::TranscriptGL,
    types::{StarkStruct, Step, PIL},
};

pub struct EStark {
    pil: PIL,
    const_pols: PolsArray,
    params: StarkStruct,
    setup: StarkSetup<MerkleTreeGL>,
}

impl EStark {
    /// Creates our default configuration stark struct.
    pub fn new<F: FieldElement>(
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        degree: DegreeType,
    ) -> Self {
        if F::modulus().to_arbitrary_integer() != GoldilocksField::modulus().to_arbitrary_integer()
        {
            unimplemented!("eSTARK is only implemented for Goldilocks field");
        }
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

        let pil: PIL = json_exporter::export(pil);

        let const_pols = to_starky_pols_array(fixed, &pil, PolKind::Constant);

        let setup = StarkSetup::new(
            &const_pols,
            &mut pil.clone(),
            &params,
            Some("main.first_step".to_string()),
        )
        .unwrap();

        Self {
            params,
            pil,
            const_pols,
            setup,
        }
    }

    pub fn prove_and_verify<F: FieldElement>(
        &mut self,
        witness: &[(&str, Vec<F>)],
    ) -> (Vec<u8>, String) {
        let proof = self.prove(witness);

        let serialized_proof_and_pil = (
            serde_json::to_vec(&proof).unwrap(),
            serde_json::to_string(&self.pil).unwrap(),
        );

        assert!(self.verify(proof));

        serialized_proof_and_pil
    }

    fn prove<F: FieldElement>(&self, witness: &[(&str, Vec<F>)]) -> StarkProof<MerkleTreeGL> {
        log::info!("Creating eSTARK proof.");

        let cm_pols = to_starky_pols_array(witness, &self.pil, PolKind::Commit);

        StarkProof::<MerkleTreeGL>::stark_gen::<TranscriptGL>(
            &cm_pols,
            &self.const_pols,
            &self.setup.const_tree,
            &self.setup.starkinfo,
            &self.setup.program,
            &self.pil,
            &self.params,
        )
        .unwrap()
    }

    fn verify(&mut self, proof: StarkProof<MerkleTreeGL>) -> bool {
        log::info!("Verifying eSTARK proof.");

        stark_verify::<MerkleTreeGL, TranscriptGL>(
            &proof,
            &self.setup.const_root,
            &self.setup.starkinfo,
            &self.params,
            &mut self.setup.program,
        )
        .unwrap()
    }
}

fn to_starky_pols_array<F: FieldElement>(
    array: &[(&str, Vec<F>)],
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

pub fn pil_to_json<T: FieldElement>(pil: &Analyzed<T>) -> String {
    serde_json::to_string(&json_exporter::export(pil)).unwrap()
}
