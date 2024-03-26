use p3_field::AbstractField;
use p3_matrix::{dense::RowMajorMatrix, MatrixRowSlices};
use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, IdentityKind},
    parsed::SelectedExpressions,
};

use powdr_number::Plonky3FieldElement;

use p3_air::{Air, AirBuilder, BaseAir};

#[derive(Clone)]
pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the fixed columns
    _fixed: &'a [(String, Vec<T>)],
    /// The value of the witness columns, if set
    _witness: Option<&'a [(String, Vec<T>)]>,
    /// Column name and index of the public cells
    _publics: Vec<(String, usize)>,
}

pub struct Plonky3Prover<'a, F> {
    _circuit: PowdrCircuit<'a, F>,
}

impl<'a, T: Plonky3FieldElement> BaseAir<T::Plonky3Field> for PowdrCircuit<'a, T> {
    fn width(&self) -> usize {
        self.analyzed.commitment_count()
            + self.analyzed.constant_count()
            + self.analyzed.intermediate_count()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<T::Plonky3Field>> {
        None
    }
}

impl<'a, T: Plonky3FieldElement, AB: AirBuilder<F = T::Plonky3Field>> Air<AB>
    for PowdrCircuit<'a, T>
{
    fn eval(&self, builder: &mut AB) {
        let degree = self.analyzed.degree.unwrap();

        let main = builder.main();

        for i in 0..degree as usize {
            let current = main.row_slice(i);
            // let next = main.row_slice(i + 1);

            for identity in &self.analyzed.identities {
                match identity.kind {
                    IdentityKind::Polynomial => {
                        println!("{identity}");

                        let _left = to_plonky3_expr::<T, AB>(&identity.left);
                        let _right = to_plonky3_expr::<T, AB>(&identity.right);

                        builder.assert_zero(current[1] - current[0] + AB::Expr::one());
                    }
                    IdentityKind::Plookup => unimplemented!(),
                    IdentityKind::Permutation => unimplemented!(),
                    IdentityKind::Connect => unimplemented!(),
                }
            }
        }
    }
}

fn to_plonky3_expr<'a, T: Plonky3FieldElement, AB: AirBuilder<F = T::Plonky3Field>>(
    _expr: &SelectedExpressions<AlgebraicExpression<T>>,
) -> AB::Expr {
    <AB::Expr as AbstractField>::zero()
}

#[cfg(test)]
mod tests {

    use p3_challenger::DuplexChallenger;
    use p3_commit::ExtensionMmcs;
    use p3_dft::Radix2DitParallel;
    use p3_field::{extension::BinomialExtensionField, AbstractField, Field};
    use p3_fri::{FriConfig, TwoAdicFriPcs};
    use p3_goldilocks::{DiffusionMatrixGoldilocks, Goldilocks};
    use p3_matrix::{dense::RowMajorMatrix, Matrix};
    use p3_merkle_tree::FieldMerkleTreeMmcs;
    use p3_poseidon2::Poseidon2;
    use p3_symmetric::{PaddingFreeSponge, TruncatedPermutation};
    use p3_uni_stark::{prove, verify, StarkConfig};
    use p3_util::log2_ceil_usize;
    use powdr_number::GoldilocksField;
    use powdr_pipeline::Pipeline;
    use rand::{distributions::Distribution, thread_rng};

    use crate::PowdrCircuit;

    type Val = p3_goldilocks::Goldilocks;
    type Perm = Poseidon2<Val, DiffusionMatrixGoldilocks, 16, 7>;
    type MyHash = PaddingFreeSponge<Perm, 16, 8, 8>;
    type MyCompress = TruncatedPermutation<Perm, 2, 8, 16>;
    type ValMmcs = FieldMerkleTreeMmcs<
        <Val as Field>::Packing,
        <Val as Field>::Packing,
        MyHash,
        MyCompress,
        8,
    >;
    type Challenge = BinomialExtensionField<Val, 2>;
    type ChallengeMmcs = ExtensionMmcs<Val, Challenge, ValMmcs>;
    type Challenger = DuplexChallenger<Val, Perm, 16>;
    type Dft = Radix2DitParallel;
    type Pcs = TwoAdicFriPcs<Val, Dft, ValMmcs, ChallengeMmcs>;
    type MyConfig = StarkConfig<Pcs, Challenge, Challenger>;

    #[test]
    fn test_incorrect_public_value() {
        let content = "namespace Global(2); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";

        let trace: RowMajorMatrix<Goldilocks> = RowMajorMatrix::new(
            [1, 2, 2, 3]
                .into_iter()
                .map(Goldilocks::from_canonical_usize)
                .collect(),
            2,
        );

        let mut pipeline =
            Pipeline::<GoldilocksField>::default().from_pil_string(content.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let air = PowdrCircuit {
            analyzed: &pil,
            _fixed: &fixed_cols,
            _witness: Some(&witness),
            _publics: vec![],
        };

        let perm = Perm::new_from_rng(8, 22, DiffusionMatrixGoldilocks, &mut thread_rng());
        let hash = MyHash::new(perm.clone());
        let compress = MyCompress::new(perm.clone());
        let val_mmcs = ValMmcs::new(hash, compress);
        let challenge_mmcs = ChallengeMmcs::new(val_mmcs.clone());
        let dft = Dft {};
        let fri_config = FriConfig {
            log_blowup: 2,
            num_queries: 28,
            proof_of_work_bits: 8,
            mmcs: challenge_mmcs,
        };
        let pcs = Pcs::new(log2_ceil_usize(trace.height()), dft, val_mmcs, fri_config);
        let config = MyConfig::new(pcs);
        let mut challenger = Challenger::new(perm.clone());
        let pis = vec![];
        let proof = prove(&config, &air, &mut challenger, trace, &pis);
        verify(&config, &air, &mut challenger, &proof, &pis).unwrap();
    }
}
