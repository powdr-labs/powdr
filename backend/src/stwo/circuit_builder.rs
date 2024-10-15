use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, IdentityKind, PolynomialType,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{FieldElement, KnownField};

use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::m31::{PackedBaseField, LOG_N_LANES, N_LANES};
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::FieldExpOps;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec};

use std::ops::{Div};

use stwo_prover::core::fields::ExtensionOf;
use stwo_prover::core::circle::CirclePoint;
use stwo_prover::core::air::accumulation::{PointEvaluationAccumulator,DomainEvaluationAccumulator};
use stwo_prover::core::air::{Component,ComponentProver,Trace};

use stwo_prover::core::constraints::{coset_vanishing, pair_vanishing};
use stwo_prover::core::air::mask::shifted_mask_points;
use stwo_prover::core::backend::{Col, Column, CpuBackend};
use stwo_prover::core::utils::bit_reverse_index;
use stwo_prover::core::vcs::poseidon252_merkle::Poseidon252MerkleChannel;
use stwo_prover::core::channel::Poseidon252Channel;
use stwo_prover::core::prover::{prove, verify};
use stwo_prover::core::circle::Coset;
use stwo_prover::core::fields::qm31::SecureField;


// Circuit structure for POWDR.
pub(crate) struct PowdrCircuit<'a, T> {
    analyzed: &'a Analyzed<T>,
    witness: Option<&'a [(String, Vec<T>)]>,
    _witgen_callback: Option<WitgenCallback<T>>,
}

impl<'a, T: FieldElement> PowdrCircuit<'a, T> {
    // Constructs a new circuit instance.
    pub(crate) fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            witness: None,
            _witgen_callback: None,
        }
    }

    // Returns the witness if it is set.
    fn witness(&self) -> &'a [(String, Vec<T>)] {
        self.witness.as_ref().unwrap()
    }

    // Associates a witness with the circuit.
    pub(crate) fn with_witness(self, witness: &'a [(String, Vec<T>)]) -> Self {
        assert_eq!(witness.len(), self.analyzed.commitment_count());
        Self {
            witness: Some(witness),
            ..self
        }
    }

    // Associates a witgen callback with the circuit.
    pub(crate) fn with_witgen_callback(self, witgen_callback: WitgenCallback<T>) -> Self {
        Self {
            _witgen_callback: Some(witgen_callback),
            ..self
        }
    }
}


pub struct FibonacciComponent {
    pub log_size: u32,
    pub claim: BaseField,
}

impl FibonacciComponent {
    pub fn new(log_size: u32, claim: BaseField) -> Self {
        Self { log_size, claim }
    }

    /// Evaluates the step constraint quotient polynomial on a single point.
    /// The step constraint is defined as:
    ///   mask[0]^2 + mask[1]^2 - mask[2]
    fn step_constraint_eval_quotient_by_mask<F: ExtensionOf<BaseField>>(
        &self,
        point: CirclePoint<F>,
        mask: &[F; 3],
    ) -> F {
        let constraint_zero_domain = Coset::subgroup(self.log_size);
        let constraint_value = mask[0] + mask[1] - mask[2];
        let selector = pair_vanishing(
            constraint_zero_domain
                .at(constraint_zero_domain.size() - 2)
                .into_ef(),
            constraint_zero_domain
                .at(constraint_zero_domain.size() - 1)
                .into_ef(),
            point,
        );
        let num = constraint_value * selector;
        let denom = coset_vanishing(constraint_zero_domain, point);
        num / denom
    }

    /// Evaluates the boundary constraint quotient polynomial on a single point.
    fn boundary_constraint_eval_quotient_by_mask<F: ExtensionOf<BaseField>>(
        &self,
        point: CirclePoint<F>,
        mask: &[F; 1],
    ) -> F {
        let constraint_zero_domain = Coset::subgroup(self.log_size);
        let p = constraint_zero_domain.at(constraint_zero_domain.size() - 1);
        // On (1,0), we should get 1.
        // On p, we should get self.claim.
        // 1 + y * (self.claim - 1) * p.y^-1
        // TODO(spapini): Cache the constant.
        let linear = F::one() + point.y * (self.claim - BaseField::from_u32_unchecked(1)) * p.y.inverse();

        let num = mask[0] - linear;
        let denom = pair_vanishing(p.into_ef(), CirclePoint::zero(), point);
        num / denom
    }
}

impl Component for FibonacciComponent {
    fn n_constraints(&self) -> usize {
        2
    }

    fn max_constraint_log_degree_bound(&self) -> u32 {
        // Step constraint is of degree 2.
        self.log_size + 1
    }

    fn trace_log_degree_bounds(&self) -> TreeVec<ColumnVec<u32>> {
        TreeVec::new(vec![vec![self.log_size]])
    }

    fn mask_points(
        &self,
        point: CirclePoint<SecureField>,
    ) -> TreeVec<ColumnVec<Vec<CirclePoint<SecureField>>>> {
        TreeVec::new(vec![shifted_mask_points(
            &vec![vec![0, 1, 2]],
            &[CanonicCoset::new(self.log_size)],
            point,
        )])
    }

    fn evaluate_constraint_quotients_at_point(
        &self,
        point: CirclePoint<SecureField>,
        mask: &TreeVec<ColumnVec<Vec<SecureField>>>,
        evaluation_accumulator: &mut PointEvaluationAccumulator
    ) {
        evaluation_accumulator.accumulate(
            self.step_constraint_eval_quotient_by_mask(point, &mask[0][0][..].try_into().unwrap()),
        );
        evaluation_accumulator.accumulate(self.boundary_constraint_eval_quotient_by_mask(
            point,
            &mask[0][0][..1].try_into().unwrap(),
        ));
    }
}

impl ComponentProver<CpuBackend> for FibonacciComponent {
    fn evaluate_constraint_quotients_on_domain(
        &self,
        trace: &Trace<'_,CpuBackend>,
        evaluation_accumulator: &mut DomainEvaluationAccumulator<CpuBackend>,

    ) { 
        //println!("trace poly in componentProver eval constraint quotient on domain \n {:?} \n ",trace.polys);
        //println!("trace eval in componentProver eval constraint quotient on domain \n {:?} \n ",trace.evals);


        let poly = &trace.polys[0][0];
        let trace_domain = CanonicCoset::new(self.log_size);
        let trace_eval_domain = CanonicCoset::new(self.log_size + 1).circle_domain();
        let trace_eval = poly.evaluate(trace_eval_domain).bit_reverse();

        // Step constraint.
        let constraint_log_degree_bound = trace_domain.log_size() + 1;
        let [mut accum] = evaluation_accumulator.columns([(constraint_log_degree_bound, 2)]);
        let constraint_eval_domain = trace_eval_domain;
        for (off, point_coset) in [
            (0, constraint_eval_domain.half_coset),
            (
                constraint_eval_domain.half_coset.size(),
                constraint_eval_domain.half_coset.conjugate(),
            ),
        ] {
            let eval = trace_eval.fetch_eval_on_coset(point_coset.shift(trace_domain.index_at(0)));
            let mul = trace_domain.step_size().div(point_coset.step_size);
            for (i, point) in point_coset.iter().enumerate() {
                let mask = [eval[i], eval[i as isize + mul], eval[i as isize + 2 * mul]];
                let mut res = self.boundary_constraint_eval_quotient_by_mask(point, &[mask[0]])
                    * accum.random_coeff_powers[0];
                res += self.step_constraint_eval_quotient_by_mask(point, &mask)
                    * accum.random_coeff_powers[1];
                accum.accumulate(bit_reverse_index(i + off, constraint_log_degree_bound), res);
            }
        }
    }
}


/// Generate execution trace
pub fn generate_trace<T: Clone>(length: usize, witness: &[(String, Vec<T>)]
)-> ColumnVec<CircleEvaluation<CpuBackend, BaseField, BitReversedOrder>> {

    let trace: Vec<BaseField> = witness
    .iter()
    .flat_map(|(_, vec)| {
        vec.iter().flat_map(|mersenne| {

            let ptr = mersenne as *const T as *const u32;

            let value = unsafe {
                *ptr // Dereference the pointer to get the u32 value
            };

            // Convert chunks to PackedBaseField
            // Note: We use unsafe block because PackedBaseField::load is unsafe
           
                vec![
                    BaseField::from_u32_unchecked(value),
                ]
            
        })
    })
    .collect(); // Collect the flattened iterator into a Vec<PackedBaseField>

    let half_size = trace.len() / 2;
    let trace = trace[..half_size].to_vec();
   
        // println!("the generated stwo trace is {:?}", trace);

        let domain = CanonicCoset::new((length as u32));
    //  let evaluated_trace = CircleEvaluation::<CpuBackend, BaseField, BitReversedOrder>::new(domain, trace);
    //  vec![evaluated_trace]

     vec![CircleEvaluation::new_canonical_ordered(domain, trace)]
}






