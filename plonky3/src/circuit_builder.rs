//! A plonky3 adapter for powdr
//!
//! Supports public inputs with the use of fixed columns.
//! Namely, given public value pub corresponding to a witness value in row j
//! of witness column x, a corresponding fixed selector column s which is 0
//! everywhere save for at row j is constructed to constrain s * (pub - x) on
//! every row.

use std::{
    any::TypeId,
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    sync::Mutex,
};

use p3_air::{
    Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, MultistageAirBuilder, PairBuilder,
};

use p3_field::{AbstractField, PrimeField};
use p3_goldilocks::Goldilocks;
use p3_matrix::dense::RowMajorMatrix;
use p3_matrix::Matrix;

use p3_uni_stark::NextStageTraceCallback;
use powdr_ast::{
    analyzed::{
        AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
        AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Challenge, IdentityKind,
        PolynomialType,
    },
    parsed::visitor::ExpressionVisitable,
};

use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, GoldilocksField, LargeInt};

use crate::params::Config;

pub type Val = p3_goldilocks::Goldilocks;

pub(crate) struct PowdrCircuit<'a, T> {
    /// The analyzed PIL
    analyzed: &'a Analyzed<T>,
    // the values of the witness, in a refcell as they get mutated as we go through stages
    pub witness_so_far: Mutex<RefCell<Vec<(String, Vec<T>)>>>,
    /// The witgen callback
    witgen_callback: Option<WitgenCallback<T>>,
    /// The matrix of preprocessed values, used in debug mode to check the constraints before proving
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Goldilocks>>,
}

impl<'a, T: FieldElement> NextStageTraceCallback<Config> for PowdrCircuit<'a, T> {
    // this wraps the witgen callback to make it compatible with p3:
    // - p3 passes its local challenge values and the stage id
    // - it receives the trace for the next stage in the expected format
    // - internally, the full witness is accumulated in [Self] as it's needed in order to call the witgen callback
    fn get_next_stage_trace(
        &self,
        trace_stage: u32,
        challenge_values: &[Goldilocks],
    ) -> RowMajorMatrix<Goldilocks> {
        let stage_challenges: BTreeSet<Challenge> = self
            .get_challenges()
            .into_iter()
            .filter(|c| c.stage == trace_stage)
            .collect();
        assert_eq!(stage_challenges.len(), challenge_values.len());
        let challenge_map = stage_challenges
            .into_iter()
            .zip(challenge_values)
            .map(|(c, v)| (c.id, from_goldilocks(*v)))
            .collect();

        // to call the witgen callback, we need to pass the witness for all stages so far
        let next_trace = {
            let binding = &self.witness_so_far.lock().unwrap();
            let current_witness = binding.borrow();
            self.witgen_callback.as_ref().unwrap().next_stage_witness(
                &current_witness,
                challenge_map,
                trace_stage as u8,
            )
        };

        // generate the next trace in the format p3 expects
        let res = generate_matrix(
            next_trace
                .iter()
                .map(|(name, values)| (name, values.as_ref())),
        );

        // append the witness to the full witness
        self.witness_so_far
            .lock()
            .unwrap()
            .borrow_mut()
            .extend(next_trace);
        // return the next trace
        res
    }
}

/// Convert a witness for a stage
pub fn generate_matrix<'a, T: FieldElement>(
    witness: impl Iterator<Item = (&'a String, &'a [T])> + Clone,
) -> RowMajorMatrix<Goldilocks> {
    let width = witness.clone().count();

    let size = witness.clone().next().map(|(_, values)| values.len());

    let values = size
        .map(|size|
                // for each row, get the value of each column
                (0..size)
                    .flat_map(move |i| {
                        // witness values
                        witness.clone().map(move |(_, v)| v[i])
                    })
                    .map(cast_to_goldilocks)
                    .collect())
        .unwrap_or_default();
    RowMajorMatrix::new(values, width)
}

pub fn cast_to_goldilocks<T: FieldElement>(v: T) -> Val {
    assert_eq!(TypeId::of::<T>(), TypeId::of::<GoldilocksField>());
    Val::from_canonical_u64(v.to_integer().try_into_u64().unwrap())
}

pub fn from_goldilocks<T: FieldElement>(v: Goldilocks) -> T {
    assert_eq!(TypeId::of::<T>(), TypeId::of::<GoldilocksField>());
    T::from(v.as_canonical_biguint().to_u64_digits()[0])
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            witgen_callback: None,
            witness_so_far: Default::default(),
            #[cfg(debug_assertions)]
            preprocessed: None,
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            witgen_callback: Some(witgen_callback),
            ..self
        }
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn public_values_so_far(&self) -> Vec<Goldilocks> {
        let binding = &self.witness_so_far.lock().unwrap();
        let witness = binding.borrow();

        let witness = witness
            .iter()
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
        assert!(self.witness_so_far.lock().unwrap().borrow().is_empty());
        Self {
            witness_so_far: RefCell::new(witness).into(),
            ..self
        }
    }

    /// Returns a vector of referenced challenges in each stage.
    pub(crate) fn get_challenges(&self) -> BTreeSet<Challenge> {
        let mut challenges = BTreeSet::default(); // extracting the challenges from identities
        for identity in self
            .analyzed
            .identities_with_inlined_intermediate_polynomials()
        {
            identity.pre_visit_expressions(&mut |expr| {
                if let AlgebraicExpression::Challenge(challenge) = expr {
                    challenges.insert(*challenge);
                }
            });
        }
        challenges
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
    fn to_plonky3_expr<
        AB: AirBuilder<F = Val> + AirBuilderWithPublicValues + MultistageAirBuilder,
    >(
        &self,
        e: &AlgebraicExpression<T>,
        stages: &[AB::M],
        fixed: &AB::M,
        publics: &BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>,
        challenges: &BTreeMap<u32, BTreeMap<u64, <AB as AirBuilder>::Expr>>,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                match poly_id.ptype {
                    PolynomialType::Committed => {
                        // find the stage for this reference
                        let stage = self
                            .analyzed
                            .definitions_in_source_order(PolynomialType::Committed)
                            .iter()
                            .find_map(|(s, _)| {
                                let symbol_stage = s.stage.unwrap_or_default();
                                s.array_elements()
                                    .any(|(_, id)| id == poly_id)
                                    .then_some(symbol_stage)
                            })
                            .unwrap();
                        // find the index of this reference in the list of all references of this stage
                        let index = self
                            .analyzed
                            .definitions_in_source_order(PolynomialType::Committed)
                            .iter()
                            .filter(|&(s, _)| (s.stage.unwrap_or_default() == stage))
                            .flat_map(|(s, _)| s.array_elements())
                            .position(|(_, pid)| pid == poly_id)
                            .unwrap();
                        let row = stages[stage as usize].row_slice(r.next as usize);
                        row[index].into()
                    }
                    PolynomialType::Constant => {
                        // find the index of this reference in the list of all constant references
                        let index = self
                            .analyzed
                            .definitions_in_source_order(PolynomialType::Constant)
                            .iter()
                            .flat_map(|(s, _)| s.array_elements())
                            .position(|(_, pid)| pid == poly_id)
                            .unwrap();
                        let row = fixed.row_slice(r.next as usize);
                        row[index].into()
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
                let left = self.to_plonky3_expr::<AB>(left, stages, fixed, publics, challenges);
                let right = self.to_plonky3_expr::<AB>(right, stages, fixed, publics, challenges);

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
                    self.to_plonky3_expr::<AB>(expr, stages, fixed, publics, challenges);

                match op {
                    AlgebraicUnaryOperator::Minus => -expr,
                }
            }
            AlgebraicExpression::Challenge(challenge) => {
                challenges[&challenge.stage][&challenge.id].clone()
            }
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

    fn stage_count(&self) -> usize {
        2
    }

    fn preprocessed_trace(&self) -> Option<RowMajorMatrix<Val>> {
        #[cfg(debug_assertions)]
        {
            self.preprocessed.clone()
        }
        #[cfg(not(debug_assertions))]
        unimplemented!()
    }

    fn multi_stage_width(&self, stage: u32) -> usize {
        self.analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .iter()
            .filter_map(|(s, _)| {
                let symbol_stage = s.stage.unwrap_or_default();
                (stage == symbol_stage).then(|| s.array_elements().count())
            })
            .sum()
    }

    fn challenge_count(&self, stage: u32) -> usize {
        self.get_challenges()
            .iter()
            .filter(|c| c.stage == stage)
            .count()
    }
}

impl<
        'a,
        T: FieldElement,
        AB: AirBuilderWithPublicValues<F = Val> + PairBuilder + MultistageAirBuilder,
    > Air<AB> for PowdrCircuit<'a, T>
{
    fn eval(&self, builder: &mut AB) {
        let stage_count = self.stage_count();
        let stages: Vec<AB::M> = (0..stage_count).map(|i| builder.multi_stage(i)).collect();
        let fixed = builder.preprocessed();
        let pi = builder.public_values();
        let publics = self.analyzed.get_publics();
        // for each stage, the ids of the challenges drawn at the end of that stage
        let challenges: BTreeMap<u32, Vec<u64>> =
            self.get_challenges()
                .into_iter()
                .fold(Default::default(), |mut map, c| {
                    map.entry(c.stage).or_default().push(c.id);
                    map
                });
        // for each stage, the values of the challenges drawn at the end of that stage
        let challenges: BTreeMap<u32, BTreeMap<u64, <AB as AirBuilder>::Expr>> = challenges
            .into_iter()
            .map(|(stage, ids)| {
                let p3_challenges = builder.challenges(stage as usize).to_vec();
                assert_eq!(p3_challenges.len(), ids.len());
                (stage, ids.into_iter().zip(p3_challenges).collect())
            })
            .collect();
        assert_eq!(publics.len(), pi.len());

        // public constraints
        let public_vals_by_id = publics
            .iter()
            .zip(pi.to_vec())
            .map(|((id, _, _), val)| (id, val))
            .collect::<BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>();

        // constrain public inputs using witness columns in stage 0
        let fixed_local = fixed.row_slice(0);
        let public_offset = self.analyzed.constant_count();
        let stage_0_local = stages[0].row_slice(0);

        publics
            .iter()
            .enumerate()
            .for_each(|(index, (pub_id, col_id, _))| {
                let selector = fixed_local[public_offset + index];
                let witness_col = stage_0_local[*col_id];
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
                        &stages,
                        &fixed,
                        &public_vals_by_id,
                        &challenges,
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
