//! A plonky3 adapter for powdr
//!
//! Supports public inputs with the use of fixed columns.
//! Namely, given public value pub corresponding to a witness value in row j
//! of witness column x, a corresponding fixed selector column s which is 0
//! everywhere save for at row j is constructed to constrain s * (pub - x) on
//! every row.

use std::collections::{BTreeMap, HashSet};

use crate::params::{Commitment, FieldElementMap, Plonky3Field, ProverData};
use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, PairBuilder};
use p3_matrix::{dense::RowMajorMatrix, Matrix};
// use p3_mds::MdsPermutation;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Identity, IdentityKind,
    PolynomialType, SelectedExpressions,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement};

/// A description of the constraint system.
/// All of the data is derived from the analyzed PIL, but is materialized
/// here for performance reasons.
struct ConstraintSystem<T> {
    identities: Vec<Identity<SelectedExpressions<AlgebraicExpression<T>>>>,
    publics: Vec<(String, usize, usize)>,
    commitment_count: usize,
    constant_count: usize,
    degrees: HashSet<DegreeType>,
}

impl<T: FieldElement> From<&Analyzed<T>> for ConstraintSystem<T> {
    fn from(analyzed: &Analyzed<T>) -> Self {
        let identities = analyzed.identities_with_inlined_intermediate_polynomials();
        let publics = analyzed.get_publics();
        let commitment_count = analyzed.commitment_count();
        let constant_count = analyzed.constant_count();
        let degrees = analyzed.degrees();

        Self {
            identities,
            publics,
            commitment_count,
            constant_count,
            degrees,
        }
    }
}
pub(crate) struct PowdrCircuit<'a, T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    /// The constraint system description
    constraint_system: ConstraintSystem<T>,
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,
    /// Callback to augment the witness in the later stages
    _witgen_callback: Option<WitgenCallback<T>>,
    /// The matrix of preprocessed values, used in debug mode to check the constraints before proving
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Plonky3Field<T>>>,
}

impl<'a, T: FieldElementMap> PowdrCircuit<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub fn generate_trace_rows(&self) -> RowMajorMatrix<Plonky3Field<T>> {
        // an iterator over all columns, committed then fixed
        let witness = self.witness().iter();
        let degrees = &self.constraint_system.degrees;

        let values = match degrees.len() {
            1 => {
                // for each row, get the value of each column
                let degree = degrees.iter().next().unwrap();
                (0..*degree)
                    .flat_map(move |i| {
                        // witness values
                        witness.clone().map(move |(_, v)| v[i as usize])
                    })
                    .map(|f| f.into_p3_field())
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

impl<'a, T: FieldElementMap> PowdrCircuit<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
        if analyzed
            .definitions
            .iter()
            .any(|(_, (s, _))| matches!(s.stage, Some(stage) if stage > 0))
        {
            unimplemented!("Multi-stage proving is not supported in Plonky3")
        }

        Self {
            constraint_system: analyzed.into(),
            witness: None,
            _witgen_callback: None,
            #[cfg(debug_assertions)]
            preprocessed: None,
        }
    }

    fn witness(&self) -> &'a [(String, Vec<T>)] {
        self.witness.as_ref().unwrap()
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn get_public_values(&self) -> Vec<Plonky3Field<T>> {
        let witness = self
            .witness
            .as_ref()
            .expect("Witness needs to be set")
            .iter()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        self.constraint_system
            .publics
            .iter()
            .map(|(col_name, _, idx)| {
                let vals = *witness.get(&col_name).unwrap();
                vals[*idx].into_p3_field()
            })
            .collect()
    }

    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        assert_eq!(witness.len(), self.constraint_system.commitment_count);
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
        preprocessed_matrix: RowMajorMatrix<Plonky3Field<T>>,
    ) -> Self {
        self.preprocessed = Some(preprocessed_matrix);
        self
    }

    /// Conversion to plonky3 expression
    fn to_plonky3_expr<AB: AirBuilder<F = Plonky3Field<T>> + AirBuilderWithPublicValues>(
        &self,
        e: &AlgebraicExpression<T>,
        main: &AB::M,
        fixed: &AB::M,
        publics: &BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                match poly_id.ptype {
                    PolynomialType::Committed => {
                        assert!(
                            r.poly_id.id < self.constraint_system.commitment_count as u64,
                            "Plonky3 expects `poly_id` to be contiguous"
                        );
                        let row = main.row_slice(r.next as usize);
                        row[r.poly_id.id as usize].into()
                    }
                    PolynomialType::Constant => {
                        assert!(
                            r.poly_id.id < self.constraint_system.constant_count as u64,
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
            AlgebraicExpression::PublicReference(id) => (*publics
                .get(id)
                .expect("Referenced public value does not exist"))
            .into(),
            AlgebraicExpression::Number(n) => AB::Expr::from(n.into_p3_field()),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = self.to_plonky3_expr::<AB>(left, main, fixed, publics);
                let right = self.to_plonky3_expr::<AB>(right, main, fixed, publics);

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
                let expr: <AB as AirBuilder>::Expr =
                    self.to_plonky3_expr::<AB>(expr, main, fixed, publics);

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

/// An extension of [Air] allowing access to the number of fixed columns

impl<'a, T: FieldElementMap> BaseAir<Plonky3Field<T>> for PowdrCircuit<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn width(&self) -> usize {
        self.constraint_system.commitment_count
    }

    fn preprocessed_width(&self) -> usize {
        self.constraint_system.constant_count + self.constraint_system.publics.len()
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<Plonky3Field<T>>> {
        #[cfg(debug_assertions)]
        {
            self.preprocessed.clone()
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
    }
}

impl<'a, T: FieldElementMap, AB: AirBuilderWithPublicValues<F = Plonky3Field<T>> + PairBuilder>
    Air<AB> for PowdrCircuit<'a, T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let fixed = builder.preprocessed();
        let pi = builder.public_values();
        assert_eq!(self.constraint_system.publics.len(), pi.len());

        let local = main.row_slice(0);

        // public constraints
        let public_vals_by_id = self
            .constraint_system
            .publics
            .iter()
            .zip(pi.to_vec())
            .map(|((id, _, _), val)| (id, val))
            .collect::<BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>();

        let fixed_local = fixed.row_slice(0);
        let public_offset = self.constraint_system.constant_count;

        self.constraint_system.publics.iter().enumerate().for_each(
            |(index, (pub_id, col_id, _))| {
                let selector = fixed_local[public_offset + index];
                let witness_col = local[*col_id];
                let public_value = public_vals_by_id[pub_id];

                // constraining s(i) * (pub[i] - x(i)) = 0
                builder.assert_zero(selector * (public_value.into() - witness_col));
            },
        );

        // circuit constraints
        for identity in &self.constraint_system.identities {
            match identity.kind {
                IdentityKind::Polynomial => {
                    assert_eq!(identity.left.expressions.len(), 0);
                    assert_eq!(identity.right.expressions.len(), 0);
                    assert!(identity.right.selector.is_none());

                    let left = self.to_plonky3_expr::<AB>(
                        identity.left.selector.as_ref().unwrap(),
                        &main,
                        &fixed,
                        &public_vals_by_id,
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
