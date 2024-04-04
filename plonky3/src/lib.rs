//! Plonky3 adapter for powdr
//! Since plonky3 does not have fixed columns, we encode them as witness columns.
//! The encoded plonky3 columns are chosen to be the powdr witness columns followed by the powdr fixed columns
//! TODO: refactor powdr to remove the distinction between fixed and witness columns, so that we do not have to rearrange things here

use p3_matrix::{dense::RowMajorMatrix, MatrixRowSlices};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicUnaryOperator, Analyzed, IdentityKind,
    PolynomialType,
};

use powdr_number::Plonky3FieldElement;

use p3_air::{Air, AirBuilder, BaseAir};

#[derive(Clone)]
pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the fixed columns
    fixed: &'a [(String, Vec<T>)],
    /// The value of the witness columns
    witness: &'a [(String, Vec<T>)],
    /// Column name and index of the public cells
    _publics: Vec<(String, usize)>,
}

impl<'a, T: Plonky3FieldElement> PowdrCircuit<'a, T> {
    fn to_plonky3_expr<AB: AirBuilder<F = T::Plonky3Field>>(
        &self,
        e: &AlgebraicExpression<T>,
        main: &<AB as AirBuilder>::M,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                let row = match r.next {
                    true => main.row_slice(1),
                    false => main.row_slice(0),
                };

                // witness columns indexes are unchanged, fixed ones are offset
                let index = match poly_id.ptype {
                    PolynomialType::Committed => self
                        .witness
                        .as_ref()
                        .iter()
                        .position(|(name, _)| *name == r.name)
                        .unwrap(),
                    PolynomialType::Constant => {
                        self.witness.as_ref().len()
                            + self
                                .fixed
                                .iter()
                                .position(|(name, _)| *name == r.name)
                                .unwrap()
                    }
                    PolynomialType::Intermediate => {
                        unreachable!("intermediate polynomials should have been inlined")
                    }
                };

                row[index].into()
            }
            AlgebraicExpression::PublicReference(_) => todo!(),
            AlgebraicExpression::Number(n) => AB::Expr::from((*n).into_plonky3()),
            AlgebraicExpression::BinaryOperation(left, op, right) => {
                let left: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(left, main);
                let right = self.to_plonky3_expr::<AB>(right, main);

                match op {
                    AlgebraicBinaryOperator::Add => left + right,
                    AlgebraicBinaryOperator::Sub => left - right,
                    AlgebraicBinaryOperator::Mul => left * right,
                    AlgebraicBinaryOperator::Pow => unimplemented!(),
                }
            }
            AlgebraicExpression::UnaryOperation(op, e) => {
                let e: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(e, main);

                match op {
                    AlgebraicUnaryOperator::Minus => -e,
                }
            }
            AlgebraicExpression::Challenge(_) => todo!(),
        };
        res
    }
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
        let width = self.witness.len() + self.fixed.len();
        let joined_iter = self.witness.iter().chain(self.fixed);
        let len = self.analyzed.degree.unwrap();

        let values = (0..len)
            .flat_map(move |i| {
                joined_iter
                    .clone()
                    .map(move |(_, v)| v[i as usize].into_plonky3())
            })
            .collect();

        Some(RowMajorMatrix::new(values, width))
    }
}

impl<'a, T: Plonky3FieldElement, AB: AirBuilder<F = T::Plonky3Field>> Air<AB>
    for PowdrCircuit<'a, T>
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();

        for identity in &self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            match identity.kind {
                IdentityKind::Polynomial => {
                    assert_eq!(identity.left.expressions.len(), 0);
                    assert_eq!(identity.right.expressions.len(), 0);
                    assert!(identity.right.selector.is_none());

                    let left =
                        self.to_plonky3_expr::<AB>(identity.left.selector.as_ref().unwrap(), &main);

                    builder.assert_zero(left);
                }
                IdentityKind::Plookup => unimplemented!(),
                IdentityKind::Permutation => unimplemented!(),
                IdentityKind::Connect => unimplemented!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use p3_air::BaseAir;
    use p3_challenger::DuplexChallenger;
    use p3_commit::testing::TrivialPcs;
    use p3_dft::Radix2DitParallel;
    use p3_field::extension::BinomialExtensionField;
    use p3_goldilocks::DiffusionMatrixGoldilocks;
    use p3_matrix::Matrix;
    use p3_poseidon2::Poseidon2;
    use p3_uni_stark::{prove, verify, StarkConfig};
    use p3_util::log2_ceil_usize;
    use powdr_number::GoldilocksField;
    use powdr_pipeline::Pipeline;
    use rand::thread_rng;

    use crate::PowdrCircuit;

    /// Prove and verify execution using a trivial PCS (coefficients of the polynomials)
    fn run_test_goldilocks_trivial_pcs(pil: &str) {
        type Val = p3_goldilocks::Goldilocks;
        type Perm = Poseidon2<Val, DiffusionMatrixGoldilocks, 16, 7>;
        type Challenge = BinomialExtensionField<Val, 2>;
        type Challenger = DuplexChallenger<Val, Perm, 16>;
        type Dft = Radix2DitParallel;
        type Pcs = TrivialPcs<Val, Radix2DitParallel>;
        type MyConfig = StarkConfig<Pcs, Challenge, Challenger>;

        let mut pipeline = Pipeline::<GoldilocksField>::default().from_pil_string(pil.to_string());

        let pil = pipeline.compute_optimized_pil().unwrap();
        let fixed_cols = pipeline.compute_fixed_cols().unwrap();
        let witness = pipeline.compute_witness().unwrap();

        let air = PowdrCircuit {
            analyzed: &pil,
            fixed: &fixed_cols,
            witness: &witness,
            _publics: vec![],
        };

        let trace = air.preprocessed_trace().unwrap();

        let perm = Perm::new_from_rng(8, 22, DiffusionMatrixGoldilocks, &mut thread_rng());
        let dft = Dft {};
        let pcs = p3_commit::testing::TrivialPcs {
            dft,
            log_n: log2_ceil_usize(trace.height()),
            _phantom: std::marker::PhantomData,
        };
        let config = MyConfig::new(pcs);
        let mut challenger = Challenger::new(perm.clone());
        let pis = vec![];
        let proof = prove(&config, &air, &mut challenger, trace, &pis);
        verify(&config, &air, &mut challenger, &proof, &pis).unwrap();
    }

    #[test]
    #[should_panic = "assertion failed: width >= 1"]
    fn empty() {
        let content = "namespace Global(8);";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    fn single_fixed_column() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*;";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    fn single_witness_column() {
        let content = "namespace Global(8); pol witness a;";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    fn polynomial_identity() {
        let content = "namespace Global(8); pol fixed z = [1, 2]*; pol witness a; a = z + 1;";
        run_test_goldilocks_trivial_pcs(content);
    }

    #[test]
    #[should_panic = "not implemented"]
    fn lookup() {
        let content = "namespace Global(8); pol fixed z = [0, 1]*; pol witness a; a in z;";
        run_test_goldilocks_trivial_pcs(content);
    }
}
