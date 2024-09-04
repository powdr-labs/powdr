//! A plonky3 adapter for powdr
//!
//! Supports public inputs with the use of fixed columns.
//! Namely, given public value pub corresponding to a witness value in row j
//! of witness column x, a corresponding fixed selector column s which is 0
//! everywhere save for at row j is constructed to constrain s * (pub - x) on
//! every row.

use std::{any::TypeId, collections::BTreeMap};

use p3_air::{
    Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, PairBuilder,
};

use p3_field::AbstractField;
use p3_goldilocks::Goldilocks;
use p3_matrix::dense::RowMajorMatrix;
use p3_matrix::Matrix;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, IdentityKind, PolynomialType,
};

use powdr_number::{FieldElement, GoldilocksField, LargeInt};

pub type Val = p3_goldilocks::Goldilocks;

pub struct Stage<T> {
    witness: Vec<(String, Vec<T>)>,
    /// the values of the challenges drawn for this stage. Empty for stage 0
    challenges: BTreeMap<u64, T>,
}

impl<T> Stage<T> {
    fn width(&self) -> usize {
        self.witness.len()
    }
}

pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    /// The value of the witness columns, if set
    stages: Vec<Stage<T>>,
    /// The matrix of preprocessed values, used in debug mode to check the constraints before proving
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Goldilocks>>,
}

// // implementations for challenges
// impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
//     pub(crate) fn next_stage_witness(
//         &self,
//         stage: u32,
//         challenge_values: BTreeMap<u64, Goldilocks>,
//     ) {
//         self.challenge_values.append(challenge_values);
//         self.witness = Option(
//             self.witgen_callback
//                 .unwrap_or_else(panic!("Need witness callback for multi-stage challenges!"))
//                 .next_stage_witness(
//                     self.witness(),
//                     self.challenge_values[stage].unwrap_or_default(),
//                     stage,
//                 ),
//         );
//     }
// }

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub fn generate_trace_rows(&self, stage: u32) -> RowMajorMatrix<Goldilocks> {
        let stage = &self.stages[stage as usize];
        let witness = stage.witness.iter();
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
        RowMajorMatrix::new(values, stage.width())
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
            stages: vec![],
            #[cfg(debug_assertions)]
            preprocessed: None,
        }
    }

    // iterate through the entire witness so far
    fn witness_so_far(&self) -> impl Iterator<Item = &(String, Vec<T>)> {
        self.stages.iter().flat_map(|stage| &stage.witness)
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn public_values_so_far(&self) -> Vec<Goldilocks> {
        let witness = self
            .witness_so_far()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        self.analyzed
            .get_publics()
            .iter()
            .filter_map(|(col_name, _, idx)| {
                witness
                    .get(&col_name)
                    .map(|column| cast_to_goldilocks(column[*idx]))
            })
            .collect()
    }

    pub(crate) fn with_phase_0_witness(self, witness: Vec<(String, Vec<T>)>) -> Self {
        assert!(self.stages.is_empty());
        Self {
            stages: vec![Stage {
                witness,
                challenges: Default::default(),
            }],
            ..self
        }
    }

    // /// Returns a vector of referenced challenges in each stage.
    // pub(crate) fn get_challenges(self) -> BTreeMap<u32, Vec<u64>> {
    //     let mut challenges: BTreeMap<u32, Vec<u64>> = BTreeMap::new(); // extracting the challenges from identities
    //     for identity in self
    //         .analyzed
    //         .identities_with_inlined_intermediate_polynomials()
    //     {
    //         identity.pre_visit_expressions(&mut |expr| {
    //             if let AlgebraicExpression::Challenge(challenge) = expr {
    //                 challenges
    //                     .entry(challenge.stage)
    //                     .or_default()
    //                     .push(challenge.id)
    //             }
    //         });
    //     }
    //     challenges
    // }

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
            AlgebraicExpression::PublicReference(id) => (*publics
                .get(id)
                .expect("Referenced public value does not exist"))
            .into(),
            AlgebraicExpression::Number(n) => AB::Expr::from(cast_to_goldilocks(*n)),
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
            AlgebraicExpression::Challenge(challenge) => AB::Expr::from(cast_to_goldilocks(
                self.stages[challenge.stage as usize].challenges[&challenge.id],
            )),
        };
        res
    }
}

/// An extension of [Air] allowing access to the number of fixed columns

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

    fn multi_stage_width(&self, _stage: u32) -> usize {
        unimplemented!()
    }
}

// pub trait PowdrAirBuilder:
//     AirBuilder + AirBuilderWithPublicValues<F = Val> + PairBuilder
// {
//     /// Traces from each stage.
//     fn multi_stage(&self, stage: u32) -> Self::M;

//     /// Challenges from each stage, as public elements.
//     fn challenges(&self, stage: u32) -> &Vec<Self::PublicVar>;

//     fn preprocessed(&self) -> Self::M;
// }

// impl<'a, T> NextStageTraceCallback<Config> for PowdrCircuit<'a, T>
// {
//     fn get_next_stage_trace(&self, trace_stage:u32, challenge_values: &BTreeMap<u64, Goldilocks>) -> RowMajorMatrix<Goldilocks> {
//         self.next_stage_witness(trace_stage, challenge_values); // next-stage trace filled in
//         self.generate_trace_rows(trace_stage)
//     }
// }

impl<'a, T: FieldElement, AB: AirBuilderWithPublicValues<F = Val> + PairBuilder> Air<AB>
    for PowdrCircuit<'a, T>
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let fixed = builder.preprocessed();
        let pi = builder.public_values();
        let publics = self.analyzed.get_publics();
        assert_eq!(publics.len(), pi.len());

        let _local = main.row_slice(0);

        // public constraints
        let public_vals_by_id = publics
            .iter()
            .zip(pi.to_vec())
            .map(|((id, _, _), val)| (id, val))
            .collect::<BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>();

        // // challenges
        // let multi_stage = (1..3)
        //     .map(|stage| builder.multi_stage(stage))
        //     .collect();

        // let challenge_ids = self.get_challenges();

        // // challenge values are represented as
        // let challenge_vals_by_id = (0..2)
        //     .map(|stage| {
        //         challenge_ids[stage]
        //             .zip(
        //                 builder
        //                     .challenges(stage)
        //                     .chunks_exact(2)
        //                     .map(|chunk| (chunk[0], chunk[1])),
        //             )
        //             .collect::<BTreeMap<
        //                 u64,
        //                 (
        //                     <AB as AirBuilderWithPublicValues>::PublicVar,
        //                     <AB as AirBuilderWithPublicValues>::PublicVar,
        //                 ),
        //             >>()
        //     })
        //     .collect::<Vec<
        //         BTreeMap<
        //             u64,
        //             (
        //                 <AB as AirBuilderWithPublicValues>::PublicVar,
        //                 <AB as AirBuilderWithPublicValues>::PublicVar,
        //             ),
        //         >,
        //     >>();

        let local = main.row_slice(0);
        let fixed_local = fixed.row_slice(0);
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
