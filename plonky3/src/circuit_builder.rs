//! A plonky3 adapter for powdr
//!
//! Supports public inputs with the use of fixed columns.
//! Namely, given public value pub corresponding to a witness value in row j
//! of witness column x, a corresponding fixed selector column s which is 0
//! everywhere save for at row j is constructed to constrain s * (pub - x) on
//! every row.

use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    sync::Mutex,
};

use crate::params::{Commitment, FieldElementMap, Plonky3Field, ProverData};
use p3_air::{Air, AirBuilder, AirBuilderWithPublicValues, BaseAir, PairBuilder};
use p3_matrix::{dense::RowMajorMatrix, Matrix};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, Challenge, Identity, IdentityKind,
    PolyID, PolynomialType, SelectedExpressions,
};

use crate::{CallbackResult, MultiStageAir, MultistageAirBuilder, NextStageTraceCallback};
use powdr_ast::parsed::visitor::ExpressionVisitable;

use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;

type Witness<T> = Mutex<RefCell<Vec<(String, Vec<T>)>>>;

/// A description of the constraint system.
/// All of the data is derived from the analyzed PIL, but is materialized
/// here for performance reasons.
struct ConstraintSystem<T> {
    // for each witness column, the stage and index of this column in this stage
    witness_columns: HashMap<PolyID, (usize, usize)>,
    // for each fixed column, the index of this column in the fixed columns
    fixed_columns: HashMap<PolyID, usize>,
    identities: Vec<Identity<SelectedExpressions<AlgebraicExpression<T>>>>,
    publics: Vec<(String, PolyID, usize)>,
    commitment_count: usize,
    constant_count: usize,
    // for each stage, the number of witness columns
    stage_widths: Vec<usize>,
    challenges: BTreeSet<Challenge>,
}

impl<T: FieldElement> From<&Analyzed<T>> for ConstraintSystem<T> {
    fn from(analyzed: &Analyzed<T>) -> Self {
        let identities = analyzed.identities_with_inlined_intermediate_polynomials();
        let publics = analyzed.get_publics();
        let commitment_count = analyzed.commitment_count();
        let constant_count = analyzed.constant_count();
        let stage_widths = (0..analyzed.stage_count() as u32)
            .map(|stage| {
                analyzed
                    .definitions_in_source_order(PolynomialType::Committed)
                    .iter()
                    .filter_map(|(s, _)| {
                        let symbol_stage = s.stage.unwrap_or_default();
                        (stage == symbol_stage).then(|| s.array_elements().count())
                    })
                    .sum()
            })
            .collect();

        let fixed_columns = analyzed
            .definitions_in_source_order(PolynomialType::Constant)
            .iter()
            .flat_map(|(symbol, _)| symbol.array_elements())
            .enumerate()
            .map(|(index, (_, id))| (id, index))
            .collect();

        let witness_columns = analyzed
            .definitions_in_source_order(PolynomialType::Committed)
            .iter()
            .into_group_map_by(|(s, _)| s.stage.unwrap_or_default())
            .into_iter()
            .flat_map(|(stage, symbols)| {
                symbols
                    .into_iter()
                    .flat_map(|(s, _)| s.array_elements())
                    .enumerate()
                    .map(move |(index_in_stage, (_, poly_id))| {
                        (poly_id, (stage as usize, index_in_stage))
                    })
            })
            .collect();

        let mut challenges = BTreeSet::default();
        for identity in &identities {
            identity.pre_visit_expressions(&mut |expr| {
                if let AlgebraicExpression::Challenge(challenge) = expr {
                    challenges.insert(*challenge);
                }
            });
        }

        Self {
            identities,
            publics,
            commitment_count,
            constant_count,
            stage_widths,
            witness_columns,
            fixed_columns,
            challenges,
        }
    }
}

pub(crate) struct PowdrCircuit<T: FieldElementMap>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    /// The constraint system description
    constraint_system: ConstraintSystem<T>,
    /// The values of the witness, in a [RefCell] as it gets mutated as we go through stages
    witness_so_far: Witness<T>,
    /// Callback to augment the witness in the later stages
    witgen_callback: Option<WitgenCallback<T>>,
    /// The matrix of preprocessed values, used in debug mode to check the constraints before proving
    #[cfg(debug_assertions)]
    preprocessed: Option<RowMajorMatrix<Plonky3Field<T>>>,
}

/// Convert a witness for a stage
pub fn generate_matrix<'a, T: FieldElementMap>(
    witness: impl Iterator<Item = (&'a String, &'a [T])> + Clone,
) -> RowMajorMatrix<Plonky3Field<T>>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
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
                .map(|f| f.into_p3_field())
                .collect())
        .unwrap_or_default();
    RowMajorMatrix::new(values, width)
}

impl<T: FieldElementMap> PowdrCircuit<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    pub(crate) fn new(analyzed: &Analyzed<T>) -> Self {
        Self {
            constraint_system: analyzed.into(),
            witgen_callback: None,
            witness_so_far: Default::default(),
            #[cfg(debug_assertions)]
            preprocessed: None,
        }
    }

    /// Calculates public values from generated witness values.
    pub(crate) fn public_values_so_far(&self) -> Vec<Plonky3Field<T>> {
        let binding = &self.witness_so_far.lock().unwrap();
        let witness = binding.borrow();

        let witness = witness
            .iter()
            .map(|(name, values)| (name, values))
            .collect::<BTreeMap<_, _>>();

        self.constraint_system
            .publics
            .iter()
            .filter_map(|(col_name, _, idx)| {
                witness
                    .get(&col_name)
                    .map(|column| column[*idx].into_p3_field())
            })
            .collect()
    }

    pub(crate) fn with_phase_0_witness(self, witness: &[(String, Vec<T>)]) -> Self {
        assert!(self.witness_so_far.lock().unwrap().borrow().is_empty());
        Self {
            witness_so_far: RefCell::new(witness.to_vec()).into(),
            ..self
        }
    }

    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            witgen_callback: Some(witgen_callback),
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
    fn to_plonky3_expr<AB: AirBuilder<F = Plonky3Field<T>> + MultistageAirBuilder>(
        &self,
        e: &AlgebraicExpression<T>,
        traces_by_stage: &[AB::M],
        fixed: &AB::M,
        publics: &BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>,
        challenges: &BTreeMap<u32, BTreeMap<u64, <AB as MultistageAirBuilder>::Challenge>>,
    ) -> AB::Expr {
        let res = match e {
            AlgebraicExpression::Reference(r) => {
                let poly_id = r.poly_id;

                match poly_id.ptype {
                    PolynomialType::Committed => {
                        // find the stage and index in that stage
                        let (stage, index) = self.constraint_system.witness_columns[&poly_id];
                        traces_by_stage[stage].row_slice(r.next as usize)[index].into()
                    }
                    PolynomialType::Constant => {
                        // find the index in the fixed columns
                        let index = self.constraint_system.fixed_columns[&poly_id];
                        fixed.row_slice(r.next as usize)[index].into()
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
                let left =
                    self.to_plonky3_expr::<AB>(left, traces_by_stage, fixed, publics, challenges);
                let right =
                    self.to_plonky3_expr::<AB>(right, traces_by_stage, fixed, publics, challenges);

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
                    self.to_plonky3_expr::<AB>(expr, traces_by_stage, fixed, publics, challenges);

                match op {
                    AlgebraicUnaryOperator::Minus => -expr,
                }
            }
            AlgebraicExpression::Challenge(challenge) => {
                challenges[&challenge.stage][&challenge.id].clone().into()
            }
        };
        res
    }
}

/// An extension of [Air] allowing access to the number of fixed columns

impl<T: FieldElementMap> BaseAir<Plonky3Field<T>> for PowdrCircuit<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn width(&self) -> usize {
        self.constraint_system.commitment_count
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

impl<
        T: FieldElementMap,
        AB: AirBuilderWithPublicValues<F = Plonky3Field<T>> + PairBuilder + MultistageAirBuilder,
    > Air<AB> for PowdrCircuit<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn eval(&self, builder: &mut AB) {
        let stage_count = <Self as MultiStageAir<AB>>::stage_count(self);
        let trace_by_stage: Vec<AB::M> = (0..stage_count).map(|i| builder.stage_trace(i)).collect();
        let fixed = builder.preprocessed();
        let pi = builder.public_values();

        // for each stage, the values of the challenges drawn at the end of that stage
        let challenges: BTreeMap<u32, BTreeMap<u64, _>> = self
            .constraint_system
            .challenges
            .iter()
            .map(|c| (c.stage, c.id))
            .into_group_map()
            .into_iter()
            .map(|(stage, ids)| {
                let p3_challenges = builder.stage_challenges(stage as usize).to_vec();
                assert_eq!(p3_challenges.len(), ids.len());
                (stage, ids.into_iter().zip(p3_challenges).collect())
            })
            .collect();
        assert_eq!(self.constraint_system.publics.len(), pi.len());

        let stage_0_local = trace_by_stage[0].row_slice(0);

        // public constraints
        let public_vals_by_id = self
            .constraint_system
            .publics
            .iter()
            .zip(pi.to_vec())
            .map(|((id, _, _), val)| (id, val))
            .collect::<BTreeMap<&String, <AB as AirBuilderWithPublicValues>::PublicVar>>();

        // constrain public inputs using witness columns in stage 0
        let fixed_local = fixed.row_slice(0);
        let public_offset = self.constraint_system.constant_count;

        self.constraint_system.publics.iter().enumerate().for_each(
            |(index, (pub_id, poly_id, _))| {
                let selector = fixed_local[public_offset + index];
                let (stage, index) = self.constraint_system.witness_columns[poly_id];
                assert_eq!(
                    stage, 0,
                    "public inputs are only allowed in the first stage"
                );
                let witness_col = stage_0_local[index];
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
                        &trace_by_stage,
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

impl<
        T: FieldElementMap,
        AB: AirBuilderWithPublicValues<F = Plonky3Field<T>> + PairBuilder + MultistageAirBuilder,
    > MultiStageAir<AB> for PowdrCircuit<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    fn preprocessed_width(&self) -> usize {
        self.constraint_system.constant_count + self.constraint_system.publics.len()
    }

    fn stage_count(&self) -> usize {
        self.constraint_system.stage_widths.len()
    }

    fn stage_trace_width(&self, stage: u32) -> usize {
        self.constraint_system.stage_widths[stage as usize]
    }

    fn stage_challenge_count(&self, stage: u32) -> usize {
        self.constraint_system
            .challenges
            .iter()
            .filter(|c| c.stage == stage)
            .count()
    }
}

impl<T: FieldElementMap> NextStageTraceCallback<T::Config> for PowdrCircuit<T>
where
    ProverData<T>: Send,
    Commitment<T>: Send,
{
    // this wraps the witgen callback to make it compatible with p3:
    // - p3 passes its local challenge values and the stage id
    // - it receives the trace for the next stage in the expected format,
    // as well as the public values for this stage and the updated challenges
    // for the previous stage
    // - internally, the full witness is accumulated in [Self] as it's needed
    // in order to call the witgen callback
    fn compute_stage(
        &self,
        trace_stage: u32,
        new_challenge_values: &[Plonky3Field<T>],
    ) -> CallbackResult<Plonky3Field<T>> {
        let witness = self.witness_so_far.lock().unwrap();
        let mut witness = witness.borrow_mut();

        let previous_stage_challenges: BTreeSet<Challenge> = self
            .constraint_system
            .challenges
            .iter()
            .filter(|c| c.stage == trace_stage - 1)
            .cloned()
            .collect();
        assert_eq!(previous_stage_challenges.len(), new_challenge_values.len());
        let challenge_map = previous_stage_challenges
            .into_iter()
            .zip(new_challenge_values)
            .map(|(c, v)| (c.id, T::from_p3_field(*v)))
            .collect();

        let columns_before: BTreeSet<String> =
            witness.iter().map(|(name, _)| name.clone()).collect();

        // to call the witgen callback, we need to pass the witness for all stages so far
        *witness = {
            self.witgen_callback.as_ref().unwrap().next_stage_witness(
                &witness,
                challenge_map,
                trace_stage as u8,
            )
        };

        // generate the next trace in the format p3 expects
        // TODO: since the current witgen callback returns the entire witness so far,
        // we filter out the columns we already know about. Instead, return only the
        // new witness in the witgen callback.
        let trace = generate_matrix(
            witness
                .iter()
                .filter(|(name, _)| !columns_before.contains(name))
                .map(|(name, values)| (name, values.as_ref())),
        );

        // return the next trace
        // later stage publics are not supported, so we return an empty vector. TODO: change this
        // shared challenges are unsupported so we return the local challenges. TODO: change this
        CallbackResult::new(trace, vec![], new_challenge_values.to_vec())
    }
}
