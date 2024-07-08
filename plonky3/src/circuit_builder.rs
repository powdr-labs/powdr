//! A plonky3 adapter for powdr
//!
//! Supports public values without the use of fixed columns.
//!
//! Namely, given public value pub corresponding to a witness value in
//! row j of witness column x, a corresponding selector column s is constructed
//! to constrain s * (pub - x) on every row:
//!
//! col witness x;
//! public out_x = col x(j);
//! col witness s;
//! s * (pub - x) = 0;
//!
//! Moreover, s is constrained to be 1 at evaluation index s(j) and 0
//! everywhere else by applying the `is_zero` transformation to a column 'decr'
//! decrementing by 1 each row from an initial value set to j in the first row:
//!
//! col witness decr;
//! decr(0) = j;
//! decr - decr' - 1 = 0;
//! s = is_zero(decr);
//!
//! Note that in Plonky3 this transformation requires an additional column
//! `inv_decr` to track the inverse of decr for the `is_zero` operation,
//! therefore requiring a total of 3 extra witness columns per public value.

use std::{any::TypeId, collections::BTreeMap};

use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, PairBuilder};
use p3_field::AbstractField;
use p3_goldilocks::Goldilocks;
use p3_matrix::{dense::RowMajorMatrix, Matrix};
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
    /// The matrix of preprocessed values, used in debug mode to check the constraints before proving
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Goldilocks>>,
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn generate_trace_rows(&self) -> RowMajorMatrix<Goldilocks> {
        // an iterator over all columns, committed then fixed
        let witness = self.witness().iter();

        let publics = self.get_publics().into_iter();
        let degrees = self.analyzed.degrees();

        let values = match degrees.len() {
            1 => {
                // for each row, get the value of each column
                let degree = degrees.iter().next().unwrap();
                (0..*degree)
                    .flat_map(move |i| {
                        // witness values
                        witness.clone().map(move |(_, v)| v[i as usize]).chain(
                            // publics rows: decrementor | inverse | selector
                            publics.clone().flat_map(move |(_, _, row_id)| {
                                let decr = T::from(row_id as u64) - T::from(i);
                                let inv_decr = if i as usize == row_id {
                                    T::zero()
                                } else {
                                    T::one() / decr
                                };
                                let s = T::from(i as usize == row_id);
                                [decr, inv_decr, s]
                            }),
                        )
                    })
                    .map(cast_to_goldilocks)
                    .collect()
            }
            0 => {
                // in this case, there are no columns, so there are no values
                assert!(witness.clone().next().is_none());
                vec![]
            }
            _ => unreachable!(),
        };
        RowMajorMatrix::new(values, self.width())
    }
}

pub fn cast_to_goldilocks<T: FieldElement>(v: T) -> Val {
    assert_eq!(TypeId::of::<T>(), TypeId::of::<GoldilocksField>());
    Val::from_canonical_u64(v.to_integer().try_into_u64().unwrap())
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
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
            #[cfg(debug_assertions)]
            preprocessed: None,
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

    #[cfg(debug_assertions)]
    pub(crate) fn with_preprocessed(
        mut self,
        preprocessed_matrix: RowMajorMatrix<Goldilocks>,
    ) -> Self {
        self.preprocessed = Some(preprocessed_matrix);
        self
    }

    /// Conversion to plonky3 expression
    fn to_plonky3_expr<AB: AirBuilder<F = Val>>(
        &self,
        e: &AlgebraicExpression<T>,
        main: &AB::M,
        fixed: &AB::M,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                match poly_id.ptype {
                    PolynomialType::Committed => {
                        assert!(
                            r.poly_id.id < self.analyzed.commitment_count() as u64,
                            "Plonky3 expects `poly_id` to be contiguous"
                        );
                        let row = main.row_slice(r.next as usize);
                        row[r.poly_id.id as usize].into()
                    }
                    PolynomialType::Constant => {
                        assert!(
                            r.poly_id.id < self.analyzed.constant_count() as u64,
                            "Plonky3 expects `poly_id` to be contiguous"
                        );
                        let row = fixed.row_slice(r.next as usize);
                        row[r.poly_id.id as usize].into()
                    }
                    PolynomialType::Intermediate => {
                        unreachable!("intermediate polynomials should have been inlined")
                    }
                }
            }
            AlgebraicExpression::PublicReference(_) => unimplemented!(
                "public references are not supported inside algebraic expressions in plonky3"
            ),
            AlgebraicExpression::Number(n) => AB::Expr::from(cast_to_goldilocks(*n)),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = self.to_plonky3_expr::<AB>(left, main, fixed);
                let right = self.to_plonky3_expr::<AB>(right, main, fixed);

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
                let expr: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(expr, main, fixed);

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
        self.analyzed.commitment_count() + 3 * self.analyzed.publics_count()
    }

    fn preprocessed_width(&self) -> usize {
        self.analyzed.constant_count()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<Val>> {
        #[cfg(debug_assertions)]
        {
            self.preprocessed.clone()
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
    }
}

impl<'a, T: FieldElement, AB: AirBuilderWithPublicValues<F = Val> + PairBuilder> Air<AB>
    for PowdrCircuit<'a, T>
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let fixed = builder.preprocessed();
        let pi = builder.public_values();
        let publics = self.get_publics();
        assert_eq!(publics.len(), pi.len());

        // public constraints
        let pi_moved = pi.to_vec();
        let (local, next) = (main.row_slice(0), main.row_slice(1));

        let public_offset = self.analyzed.commitment_count();

        publics.iter().zip(pi_moved).enumerate().for_each(
            |(index, ((_, col_id, row_id), public_value))| {
                //set decr for each public to be row_id in the first row and decrement by 1 each row
                let (decr, inv_decr, s, decr_next) = (
                    local[public_offset + 3 * index],
                    local[public_offset + 3 * index + 1],
                    local[public_offset + 3 * index + 2],
                    next[public_offset + 3 * index],
                );

                let mut when_first_row = builder.when_first_row();
                when_first_row.assert_eq(
                    decr,
                    cast_to_goldilocks(GoldilocksField::from(*row_id as u32)),
                );

                let mut when_transition = builder.when_transition();
                when_transition.assert_eq(decr, decr_next + AB::Expr::one());

                // is_zero logic-- s(row) is 1 if decr(row) is 0 and 0 otherwise
                builder.assert_bool(s); //constraining s to 1 or 0
                builder.assert_eq(s, AB::Expr::one() - inv_decr * decr);
                builder.assert_zero(s * decr); //constraining is_zero

                // constraining s(i) * (pub[i] - x(i)) = 0
                let witness_col = local[*col_id];
                builder.assert_zero(s * (public_value.into() - witness_col));
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

                    let left = self.to_plonky3_expr::<AB>(
                        identity.left.selector.as_ref().unwrap(),
                        &main,
                        &fixed,
                    );

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
