//! A plonky3 adapter for powdr
//! Since plonky3 does not have fixed columns, we encode them as witness columns.
//! The encoded plonky3 columns are chosen to be the powdr witness columns followed by the powdr fixed columns

use std::collections::BTreeMap;

use p3_air::{Air, AirBuilder, BaseAir};
use p3_field::AbstractField;
use p3_matrix::{dense::RowMajorMatrix, MatrixRowSlices};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicUnaryOperator, Analyzed, IdentityKind,
    PolynomialType,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, LargeInt};

pub type Val = p3_goldilocks::Goldilocks;

#[derive(Clone)]
pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The number of committed polynomials, computed from `analyzed` and cached
    commitment_count: usize,
    /// The value of the fixed columns
    fixed: &'a [(String, Vec<T>)],
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,
    /// Column name and index of the public cells
    publics: Vec<(String, usize)>,
    /// Callback to augment the witness in the later stages
    _witgen_callback: Option<WitgenCallback<T>>,
}

pub fn cast_to_goldilocks<T: FieldElement>(v: T) -> Val {
    Val::from_canonical_u64(v.to_integer().try_into_u64().unwrap())
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub(crate) fn new(analyzed: &'a Analyzed<T>, fixed: &'a [(String, Vec<T>)]) -> Self {
        let mut publics = analyzed
            .public_declarations
            .values()
            .map(|public_declaration| {
                let witness_name = public_declaration.referenced_poly_name();
                let witness_offset = public_declaration.index as usize;
                (witness_name, witness_offset)
            })
            .collect::<Vec<_>>();
        // Sort, so that the order is deterministic
        publics.sort();

        Self {
            analyzed,
            commitment_count: analyzed.commitment_count(),
            fixed,
            witness: None,
            publics,
            _witgen_callback: None,
        }
    }

    fn witness(&self) -> &'a [(String, Vec<T>)] {
        self.witness.as_ref().unwrap()
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        Self {
            witness: Some(witness),
            ..self
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            _witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    /// Computes the instance column from the witness
    pub(crate) fn instance_column(&self) -> Vec<Val> {
        let witness = self
            .witness
            .as_ref()
            .expect("Witness needs to be set")
            .iter()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        self.publics
            .iter()
            .map(|(col_name, i)| cast_to_goldilocks(witness.get(col_name).unwrap()[*i]))
            .collect()
    }

    /// Conversion to plonky3 expression
    fn to_plonky3_expr<AB: AirBuilder<F = Val>>(
        &self,
        e: &AlgebraicExpression<T>,
        builder: &AB,
    ) -> AB::Expr {
        let matrix = builder.main();

        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                let row = match r.next {
                    true => matrix.row_slice(1),
                    false => matrix.row_slice(0),
                };

                // witness columns indexes are unchanged, fixed ones are offset by `commitment_count`
                let index = match poly_id.ptype {
                    PolynomialType::Committed => r.poly_id.id as usize,
                    PolynomialType::Constant => self.commitment_count + r.poly_id.id as usize,
                    PolynomialType::Intermediate => {
                        unreachable!("intermediate polynomials should have been inlined")
                    }
                };

                row[index].into()
            }
            AlgebraicExpression::PublicReference(_) => unimplemented!(
                "public references are not supported inside algebraic expressions in plonky3"
            ),
            AlgebraicExpression::Number(n) => AB::Expr::from(cast_to_goldilocks(*n)),
            AlgebraicExpression::BinaryOperation(left, op, right) => {
                let left = self.to_plonky3_expr(left, builder);
                let right = self.to_plonky3_expr(right, builder);

                match op {
                    AlgebraicBinaryOperator::Add => left + right,
                    AlgebraicBinaryOperator::Sub => left - right,
                    AlgebraicBinaryOperator::Mul => left * right,
                    AlgebraicBinaryOperator::Pow => {
                        unreachable!("exponentiations should have been evaluated")
                    }
                }
            }
            AlgebraicExpression::UnaryOperation(op, e) => {
                let e: <AB as AirBuilder>::Expr = self.to_plonky3_expr(e, builder);

                match op {
                    AlgebraicUnaryOperator::Minus => -e,
                }
            }
            AlgebraicExpression::Challenge(challenge) => {
                unimplemented!("Challenge API for {challenge:?} not accessible in plonky3")
            }
        };
        res
    }
}

impl<'a, T: FieldElement> BaseAir<Val> for PowdrCircuit<'a, T> {
    fn width(&self) -> usize {
        self.commitment_count + self.analyzed.constant_count()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<Val>> {
        // an iterator over all columns, committed then fixed
        let joined_iter = self.witness().iter().chain(self.fixed);
        let len = self.analyzed.degree.unwrap();

        // for each row, get the value of each column
        let values = (0..len)
            .flat_map(move |i| {
                joined_iter
                    .clone()
                    .map(move |(_, v)| cast_to_goldilocks(v[i as usize]))
            })
            .collect();

        Some(RowMajorMatrix::new(values, self.width()))
    }
}

impl<'a, T: FieldElement, AB: AirBuilder<F = Val>> Air<AB> for PowdrCircuit<'a, T> {
    fn eval(&self, builder: &mut AB) {
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
                        self.to_plonky3_expr(identity.left.selector.as_ref().unwrap(), builder);

                    builder.assert_zero(left);
                }
                IdentityKind::Plookup => unimplemented!("Plonky3 does not support plookup"),
                IdentityKind::Permutation => unimplemented!("Plonky does not support permutations"),
                IdentityKind::Connect => unimplemented!("Plonky3 does not support connections"),
            }
        }
    }
}
