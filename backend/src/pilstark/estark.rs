use crate::{pilstark, BackendImpl};
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
    params: StarkStruct,
}

impl<F: FieldElement> BackendImpl<F> for EStark {
    /// Creates our default configuration stark struct.
    fn new(degree: DegreeType) -> Self {
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

        Self { params }
    }

    fn prove(
        &self,
        pil: &Analyzed<F>,
        fixed: &[(&str, Vec<F>)],
        witness: &[(&str, Vec<F>)],
        prev_proof: Option<crate::Proof>,
    ) -> (Option<crate::Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("aggregration is not implemented");
        }

        log::info!("Creating eSTARK proof.");

        let mut pil: PIL = pilstark::json_exporter::export(pil);

        let const_pols = to_starky_pols_array(fixed, &pil, PolKind::Constant);
        let cm_pols = to_starky_pols_array(witness, &pil, PolKind::Commit);

        let mut setup = StarkSetup::<MerkleTreeGL>::new(
            &const_pols,
            &mut pil,
            &self.params,
            Some("main.first_step".to_string()),
        )
        .unwrap();

        let starkproof = StarkProof::<MerkleTreeGL>::stark_gen::<TranscriptGL>(
            &cm_pols,
            &const_pols,
            &setup.const_tree,
            &setup.starkinfo,
            &setup.program,
            &pil,
            &self.params,
        )
        .unwrap();

        assert!(stark_verify::<MerkleTreeGL, TranscriptGL>(
            &starkproof,
            &setup.const_root,
            &setup.starkinfo,
            &self.params,
            &mut setup.program,
        )
        .unwrap());

        (
            Some(serde_json::to_vec(&starkproof).unwrap()),
            Some(serde_json::to_string(&pil).unwrap()),
        )
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
