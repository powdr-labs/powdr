use std::marker::PhantomData;

use crate::{pilstark, BackendImpl};
use ast::analyzed::Analyzed;
use number::{BigInt, Bn254Field, DegreeType, FieldElement};

use starky::{
    merklehash_bn128::MerkleTreeBN128,
    polsarray::{PolKind, PolsArray},
    stark_gen::StarkProof,
    stark_setup::StarkSetup,
    traits::{MerkleTree, Transcript},
    transcript_bn128::TranscriptBN128,
    types::{StarkStruct, Step, PIL},
};
use winter_math::fields::f64::BaseElement;

/// Group together Powdr types with their equivalent Starky types.
pub trait GroupedTypes {
    // TODO: maybe this should be a dynamic parameter.
    const VERIF_HASH_TYPE: &'static str;
    type PowdrField: FieldElement;
    type StarkyMerkle: MerkleTree;
    type StarkyTranscript: Transcript;
}

pub struct EStark<G: GroupedTypes> {
    params: StarkStruct,
    _types: PhantomData<G>,
}

impl<G: GroupedTypes> BackendImpl<G::PowdrField> for EStark<G> {
    /// Creates our default configuration stark struct.
    fn new(degree: DegreeType) -> Self {
        let degree_bits = (DegreeType::BITS - degree.leading_zeros()) as usize;
        let params = StarkStruct {
            nBits: degree_bits,
            nBitsExt: degree_bits + 1,
            nQueries: 1,
            verificationHashType: G::VERIF_HASH_TYPE.to_owned(),
            steps: vec![Step { nBits: 20 }],
        };

        Self {
            params,
            _types: PhantomData,
        }
    }

    fn prove(
        &self,
        pil: &Analyzed<G::PowdrField>,
        fixed: &[(&str, Vec<G::PowdrField>)],
        witness: &[(&str, Vec<G::PowdrField>)],
        prev_proof: Option<crate::Proof>,
    ) -> (Option<crate::Proof>, Option<String>) {
        if prev_proof.is_some() {
            unimplemented!("aggregration is not implemented");
        }

        let mut pil: PIL = pilstark::json_exporter::export(pil);

        let const_pols = to_sparky_pols_array::<G>(fixed, &pil, PolKind::Constant);
        let cm_pols = to_sparky_pols_array::<G>(witness, &pil, PolKind::Commit);

        let setup =
            StarkSetup::<G::StarkyMerkle>::new(&const_pols, &mut pil, &self.params).unwrap();

        let starkproof = StarkProof::<G::StarkyMerkle>::stark_gen::<G::StarkyTranscript>(
            &cm_pols,
            &const_pols,
            &setup.const_tree,
            &setup.starkinfo,
            &setup.program,
            &pil,
            &self.params,
        )
        .unwrap();

        (
            Some(serde_json::to_vec(&starkproof).unwrap()),
            Some(serde_json::to_string(&pil).unwrap()),
        )
    }
}

fn to_sparky_pols_array<G: GroupedTypes>(
    array: &[(&str, Vec<G::PowdrField>)],
    pil: &PIL,
    kind: PolKind,
) -> PolsArray {
    let mut output = PolsArray::new(pil, kind);
    assert_eq!(output.array.len(), array.len());
    for ((_, from), to) in array.iter().zip(output.array.iter_mut()) {
        assert_eq!(from.len(), to.len());

        for (f, t) in from.iter().zip(to.iter_mut()) {
            *t = BaseElement::new(f.to_integer().to_arbitrary_integer().try_into().unwrap());
        }
    }

    output
}

pub struct BN128;

impl GroupedTypes for BN128 {
    const VERIF_HASH_TYPE: &'static str = "BN128";
    type PowdrField = Bn254Field;
    type StarkyMerkle = MerkleTreeBN128;
    type StarkyTranscript = TranscriptBN128;
}
