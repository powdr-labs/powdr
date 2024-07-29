//! A plonky3 adapter for powdr
//!
//! Supports public inputs with the use of fixed columns.
//! Namely, given public value pub corresponding to a witness value in row j
//! of witness column x, a corresponding fixed selector column s which is 0
//! everywhere save for at row j is constructed to constrain s * (pub - x) on
//! every row.

use std::{any::TypeId, cmp::max, collections::BTreeMap};

use p3_air::{
    Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, ExtensionBuilder, PairBuilder,
    PermutationAirBuilder,
};
use p3_field::{AbstractField, ExtensionField};
use p3_goldilocks::Goldilocks;
use p3_matrix::{dense::RowMajorMatrix, MatrixRowSlices};
use p3_symmetric::Permutation;
use p3_uni_stark::StarkGenericConfig;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Challenge, IdentityKind,
    PolynomialType,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};
use tracing::field::Field;

pub type Val = p3_goldilocks::Goldilocks;

pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the witness columns, if set
    witness: Option<&'a [(String, Vec<T>)]>,
    /// Callback to augment the witness in the later stages
    witgen_callback: Option<WitgenCallback<T>>,
    /// Value of challenges at every stage
    challenge_values: Vec<Option<BTreeMap<u64, FieldElement>>>,
    /// Vector containing traces of higher-stage witnesses.
    multi_stage_traces: Vec<Option<RowMajorMatrix<Goldilocks>>>,
    /// The matrix of preprocessed values, used in debug mode to check the constraints before provingg
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Goldilocks>>,
}

// implementations for challenges
impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    /// Generate next stage trace, assuming that
    fn next_stage_trace(&self, stage: u32, challenge_values: BTreeMap<u64, FieldElement>) {
        self.challenge_values.append(challenge_values);
        let new_trace = self.generate_multi_stage_trace_rows(stage);
        // we need to somehow feed these matrices back into the folder
    }
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn generate_trace_rows(&self) -> RowMajorMatrix<Goldilocks> {
        self.generate_multi_stage_trace_rows(0)
    }

    /// Generate the witness trace for a given stage.
    pub(crate) fn generate_multi_stage_trace_rows(&self, stage: u32) -> RowMajorMatrix<Goldilocks> {
        let current_witness = self.witness().iter();
        let witness = match stage {
            0 => current_witness,
            _ => self
                .witgen_callback
                .unwrap_or_else(panic!("Need witness callback for multi-stage challenges!"))
                .next_stage_witness(
                    current_witness,
                    self.challenge_values[stage].unwrap_or_default(),
                    stage,
                ),
        };

        let degrees = self.analyzed.degrees();

        let values = match degrees.len() {
            1 => {
                // for each row, get the value of each column
                let degree = degrees.iter().next().unwrap();
                (0..*degree)
                    .flat_map(move |i| {
                        // witness values
                        witness.clone().map(move |(_, v)| v[i as usize])
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
        RowMajorMatrix::new(values, self.multi_stage_width(stage))
    }
}

pub fn cast_to_goldilocks<T: FieldElement>(v: T) -> Val {
    assert_eq!(TypeId::of::<T>(), TypeId::of::<GoldilocksField>());
    Val::from_canonical_u64(v.to_integer().try_into_u64().unwrap())
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            witness: None,
            witgen_callback: None,
            #[cfg(debug_assertions)]
            preprocessed: None,
            challenge_values: None,
            multi_stage_traces: None,
        }
    }

    fn witness(&self) -> &'a [(String, Vec<T>)] {
        self.witness.as_ref().unwrap()
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn get_public_values(&self) -> Vec<Goldilocks> {
        let witness = self
            .witness
            .as_ref()
            .expect("Witness needs to be set")
            .iter()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        self.analyzed
            .get_publics()
            .iter()
            .map(|(col_name, _, idx)| {
                let vals = *witness.get(&col_name).unwrap();
                cast_to_goldilocks(vals[*idx])
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
            witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    /// Returns a vector of referenced challenges in each stage.
    pub(crate) fn get_challenges(self) -> Vec<Vec<u64>> {
        let mut challenges = BTreeMap::new(); // extracting the challenges from identities
        for identity in self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            identity.pre_visit_expressions(&mut |expr| {
                if let AlgebraicExpression::Challenge(challenge) = expr {
                    challenges
                        .entry(challenge.stage)
                        .and_modify(|e| *e.append(challenge.id))
                        .or_insert(vec![challenge.id])
                }
            });
        }

        challenges.values().collect::<Vec<Vec<u64>>>()
    }
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    #[cfg(debug_assertions)]
    pub(crate) fn with_preprocessed(
        mut self,
        preprocessed_matrix: RowMajorMatrix<Goldilocks>,
    ) -> Self {
        self.preprocessed = Some(preprocessed_matrix);
        self
    }

    /// Conversion to plonky3 expression
    fn to_plonky3_expr<AB: AirBuilder<F = Val> + AirBuilderWithPublicValues>(
        &self,
        e: &AlgebraicExpression<T>,
        main: &AB::M,
        fixed: &AB::M,
        publics: &BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>,
        stage: u32,
        multi_stage: fn(u32) -> Vec<&AB::M>, // returns a reference to the stage _ matrix
        challenges: fn(
            u32,
        )
            -> Vec<&BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                match poly_id.ptype {
                    PolynomialType::Committed => {
                        //TODO: our changes should come from here
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
                        let row = stage_1.row_slice(r.next as usize);
                        row[r.poly_id.id as usize].into();
                        unreachable!("intermediate polynomials should have been inlined")
                    }
                }
            }
            AlgebraicExpression::PublicReference(id) => {
                assert!(
                    publics.contains_key(id),
                    "Referenced public value does not exist"
                );
                publics[id].into()
            }

            AlgebraicExpression::Number(n) => AB::Expr::from(cast_to_goldilocks(*n)),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = self.to_plonky3_expr::<AB>(
                    left,
                    main,
                    fixed,
                    publics,
                    stage,
                    multi_stage_trace,
                    challenges,
                );
                let right = self.to_plonky3_expr::<AB>(
                    right,
                    main,
                    fixed,
                    publics,
                    stage,
                    multi_stage_trace,
                    challenges,
                );

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
                let expr: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(
                    expr,
                    main,
                    fixed,
                    publics,
                    stage,
                    multi_stage_trace,
                    challenges,
                );

                match op {
                    AlgebraicUnaryOperator::Minus => -expr,
                }
            }
            AlgebraicExpression::Challenge(challenge) => {
                assert!(
                    challenges[challenge.stage].contains_key(challenge.id),
                    "Referenced challenge does not exist in this stage."
                );
                challenges[challenge.stage][challenge.id].into()
            }
        };
        res
    }
}

// challenge eval implementations TODO: move this up later
impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    /// Conversion of constraint involving challenges to plonky3 expression in the extension field.
    fn to_plonky3_expr_vanilla<AB: AirBuilder<F = Val> + AirBuilderWithPublicValues>(
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
                        //TODO: our changes should come from here
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
                        let row = stage_1.row_slice(r.next as usize);
                        row[r.poly_id.id as usize].into();
                        unreachable!("intermediate polynomials should have been inlined")
                    }
                }
            }
            AlgebraicExpression::PublicReference(id) => {
                assert!(
                    publics.contains_key(id),
                    "Referenced public value does not exist"
                );
                publics[id].into()
            }
            AlgebraicExpression::Number(n) => AB::Expr::from(cast_to_goldilocks(*n)),
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = self.to_plonky3_expr_vanilla::<AB>(left, main, fixed, publics);
                let right = self.to_plonky3_expr_vanilla::<AB>(right, main, fixed, publics);

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
                let expr: <AB as AirBuilder>::Expr = self.to_plonky3_expr::<AB>(
                    expr,
                    main,
                    fixed,
                    publics,
                    stage,
                    multi_stage_trace,
                    multi_stage_challenge,
                );

                match op {
                    AlgebraicUnaryOperator::Minus => -expr,
                }
            }
            AlgebraicExpression::Challenge(challenge) => {
                unreachable!("Level 0 expressions should not invoke challenges!")
            }
        };
        res
    }

    fn to_plonky3_expr_multi_stage<AB: AirBuilder<F = Val> + AirBuilderWithPublicValues>(
        &self,
        e: &AlgebraicExpression<T>,
        main: &AB::M,
        fixed: &AB::M,
        publics: &BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>,
        stage: u32,
        multi_stage: fn(u32) -> Vec<&AB::M>, // returns a reference to the stage _ matrix
        challenges: fn(
            u32,
        )
            -> Vec<&BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>,
    ) -> AB::Expr {
        unimplemented!()
    }
}

impl<'a, T: FieldElement> BaseAir<Val> for PowdrCircuit<'a, T> {
    fn width(&self) -> usize {
        self.analyzed.commitment_count()
    }

    fn preprocessed_width(&self) -> usize {
        self.analyzed.constant_count() + self.analyzed.publics_count()
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

/// An extension of [Air] allowing access to the number of fixed columns
/// TODO: fix pls
pub trait PowdrAirBuilder:
    AirBuilder + AirBuilderWithPublicValues<F = Val> + PairBuilder + ExtensionBuilder
{
    fn multi_stage(&self, stage: u32) -> Self::M;

    /// Challenges from each stage, as public elements.
    fn challenges(&self, stage: u32) -> &Vec<Self::PublicVar>;

    fn preprocessed(&self) -> Self::M;
}

impl<'a, T: FieldElement, AB: PowdrAirBuilder> Air<AB> for PowdrCircuit<'a, T> {
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        // get fixed only if we have constant columns, otherwise it panics. We could solve this by implementing our own `PairBuilder` returning an `Option`
        let fixed = builder.preprocessed();

        let pi = builder.public_values();
        let publics = self.analyzed.get_publics();
        assert_eq!(publics.len(), pi.len());

        let public_vals_by_id = publics
            .iter()
            .zip(pi.to_vec())
            .map(|((id, _, _), val)| (id, val))
            .collect::<BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>();

        // challenges
        let multi_stage = (1..3)
            .map(|stage| builder.multi_stage(stage))
            .collect::<Vec<&<AB as PermutationAirBuilder>::M>>();

        let challenge_ids = self.get_challenges();

        let challenge_vals_by_id = (0..3)
            .map(|stage| {
                challenge_ids[stage]
                    .zip(builder.challenges(stage))
                    .collect::<BTreeMap<u64, <AB as AirBuilderWithPublicValues>::PublicVar>>()
            })
            .collect::<Vec<BTreeMap<u64, <AB as AirBuilderWithPublicValues>::PublicVar>>>();

        let local = main.row_slice(0);
        let fixed_local = fixed.row_slice(0);

        // circuit constraints
        for identity in &self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            let mut stage: u32 = 0;
            identity.pre_visit_expressions(&mut |expr| {
                if let AlgebraicExpression::Challenge(challenge) = expr {
                    stage = stage.max(challenge.stage)
                }
            });

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
                // TODO: support for challenges
                IdentityKind::Plookup => unimplemented!("Plonky3 does not support plookup"),
                IdentityKind::Permutation => {
                    unimplemented!("Plonky3 does not support permutations")
                }
                IdentityKind::Connect => unimplemented!("Plonky3 does not support connections"),
            }
        }

        // public variable onstraints
        let public_offset = self.analyzed.constant_count();
        publics
            .iter()
            .enumerate()
            .for_each(|(index, (pub_id, col_id, _))| {
                let selector = fixed_local[public_offset + index];
                let witness_col = local[*col_id];
                let public_value = public_vals_by_id[pub_id];

                // constraining s(i) * (pub[i] - x(i)) = 0
                builder.assert_zero(selector * (public_value.into() - witness_col));
            });
    }
}
