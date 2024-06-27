//! A plonky3 adapter for powdr
//!
//! Support for public values invokes fixed selector columns, currently
//! implemented as extra witness columns in the execution trace.
//!
//! Namely, given ith public value pub[i] corresponding to a witness value in
//! row j of column Ci, a corresponding selector column Pi is constructed to
//! constrain Pi * (pub[i] - Ci) on every row. Pi is precomputed in the trace
//! to be 0 everywhere and 1 in row j.

use std::{any::TypeId, collections::BTreeMap};

use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues, BaseAir};
use p3_field::AbstractField;
use p3_goldilocks::Goldilocks;
use p3_matrix::{dense::RowMajorMatrix, MatrixRowSlices};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, IdentityKind, PolynomialType,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};

pub type Val = p3_goldilocks::Goldilocks;

pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,
    /// Callback to augment the witness in the later stages
    _witgen_callback: Option<WitgenCallback<T>>,
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn generate_trace_rows(&self) -> RowMajorMatrix<Goldilocks> {
        // an iterator over all columns, committed then fixed
        let witness = self.witness().iter();
        let publics = self.get_publics().into_iter();
        let len = self.analyzed.degree.unwrap();

        // for each row, get the value of each column
        let values = (0..len)
            .flat_map(move |i| {
                // witness values
                witness.clone().map(move |(_, v)| v[i as usize]).chain(
                    publics
                        .clone()
                        .map(move |(_, _, idx)| T::from(i as usize == idx)),
                )
            })
            .map(cast_to_goldilocks)
            .collect();
        RowMajorMatrix::new(values, self.width())
    }
}

pub fn cast_to_goldilocks<T: FieldElement>(v: T) -> Val {
    assert_eq!(TypeId::of::<T>(), TypeId::of::<GoldilocksField>());
    Val::from_canonical_u64(v.to_integer().try_into_u64().unwrap())
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
        if analyzed.constant_count() > 0 {
            unimplemented!("Fixed columns are not supported in Plonky3");
        }
        if analyzed
            .definitions
            .iter()
            .any(|(_, (s, _))| matches!(s.stage, Some(stage) if stage > 0))
        {
            unimplemented!("Multi-stage proving is not supported in Plonky3")
        }

        Self {
            analyzed,
            witness: None,
            _witgen_callback: None,
        }
    }

    fn witness(&self) -> &'a [(String, Vec<T>)] {
        self.witness.as_ref().unwrap()
    }

    /// Retrieves (col_name, col_idx, offset) of each public witness in the trace.
    pub(crate) fn get_publics(&self) -> Vec<(String, usize, usize)> {
        let mut publics = self
            .analyzed
            .public_declarations
            .values()
            .map(|public_declaration| {
                let witness_name = public_declaration.referenced_poly_name();
                let witness_column = {
                    let base = public_declaration.polynomial.poly_id.unwrap().id as usize;
                    match public_declaration.array_index {
                        Some(array_idx) => base + array_idx,
                        None => base,
                    }
                };
                let witness_offset = public_declaration.index as usize;
                (witness_name, witness_column, witness_offset)
            })
            .collect::<Vec<_>>();

        // Sort, so that the order is deterministic
        publics.sort();
        publics
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn get_public_values(&self) -> Vec<Goldilocks> {
        let publics = self.get_publics();

        let witness = self
            .witness
            .as_ref()
            .expect("Witness needs to be set")
            .iter()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        publics
            .into_iter()
            .map(|(col_name, _, idx)| {
                let vals = *witness.get(&col_name).unwrap();
                cast_to_goldilocks(vals[idx])
            })
            .collect()
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        assert_eq!(witness.len(), self.analyzed.commitment_count());
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
    /// Conversion to plonky3 expression
    fn to_plonky3_expr<AB: AirBuilder<F = Val>>(
        &self,
        e: &AlgebraicExpression<T>,
        matrix: &AB::M,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                let row = match r.next {
                    true => matrix.row_slice(1),
                    false => matrix.row_slice(0),
                };

                // witness columns indexes are unchanged, fixed ones are offset by `commitment_count`
                let index = match poly_id.ptype {
                    PolynomialType::Committed => {
                        assert!(
                            r.poly_id.id < self.analyzed.commitment_count() as u64,
                            "Plonky3 expects `poly_id` to be contiguous"
                        );
                        r.poly_id.id as usize
                    }
                    PolynomialType::Constant => {
                        unreachable!(
                            "fixed columns are not supported, should have been checked earlier"
                        )
                    }
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
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = self.to_plonky3_expr::<AB>(left, matrix);
                let right = self.to_plonky3_expr::<AB>(right, matrix);

                match op {
                    AlgebraicBinaryOperator::Add => left + right,
                    AlgebraicBinaryOperator::Sub => left - right,
                    AlgebraicBinaryOperator::Mul => left * right,
                    AlgebraicBinaryOperator::Pow => {
                        unreachable!("exponentiations should have been evaluated")
                    }
                }
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                let expr: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(expr, matrix);

                match op {
                    AlgebraicUnaryOperator::Minus => -expr,
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
        assert_eq!(self.analyzed.constant_count(), 0);
        self.analyzed.commitment_count() + self.analyzed.publics_count()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<Val>> {
        unimplemented!()
    }
}

impl<'a, T: FieldElement, AB: AirBuilderWithPublicValues<F = Val>> Air<AB> for PowdrCircuit<'a, T> {
    fn eval(&self, builder: &mut AB) {
        let matrix = builder.main();
        let pi = builder.public_values();
        let publics = self.get_publics();
        assert_eq!(publics.len(), pi.len());

        // public constraints
        let pi_moved = pi.to_vec();
        let local = matrix.row_slice(0);

        // constraining Pi * (Ci - pub[i]) = 0
        publics.iter().zip(pi_moved).enumerate().for_each(
            |(index, ((_, col_id, _), public_value))| {
                let selector = local[self.analyzed.commitment_count() + index];
                let witness_col = local[*col_id];
                builder.assert_zero(selector * (public_value.into() - witness_col));
            },
        );

        // circuit constraints
        for identity in &self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            match identity.kind {
                IdentityKind::Polynomial => {
                    assert_eq!(identity.left.expressions.len(), 0);
                    assert_eq!(identity.right.expressions.len(), 0);
                    assert!(identity.right.selector.is_none());

                    let left = self
                        .to_plonky3_expr::<AB>(identity.left.selector.as_ref().unwrap(), &matrix);

                    builder.assert_zero(left);
                }
                IdentityKind::Plookup => unimplemented!("Plonky3 does not support plookup"),
                IdentityKind::Permutation => {
                    unimplemented!("Plonky3 does not support permutations")
                }
                IdentityKind::Connect => unimplemented!("Plonky3 does not support connections"),
            }
        }
    }
}
