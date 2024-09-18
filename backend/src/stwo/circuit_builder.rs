use itertools::Itertools;
use num_traits::{One, Zero};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator, Analyzed, IdentityKind, PolynomialType,
};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::{DegreeType, FieldElement, KnownField, Mersenne31Field};
use p3_mersenne_31::Mersenne31;
use stwo_prover::constraint_framework::{EvalAtRow, FrameworkComponent, FrameworkEval};
use stwo_prover::core::backend::simd::m31::{PackedBaseField, LOG_N_LANES, N_LANES};
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::{Col, Column};
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::FieldExpOps;
use stwo_prover::core::poly::circle::{CanonicCoset, CircleEvaluation};
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::ColumnVec;
use stwo_prover::core::pcs::{CommitmentSchemeProver, CommitmentSchemeVerifier, PcsConfig, TreeVec};


// Type alias for a wide Fibonacci component with a constant size.
pub type WideFibonacciComponent<const N: usize> = FrameworkComponent<WideFibonacciEval<N>>;

// Input structure for Fibonacci computation.
#[derive(Debug)]
pub struct FibInput {
    a: PackedBaseField,
    b: PackedBaseField,
}

// Evaluation structure for wide Fibonacci computation.
pub struct WideFibonacciEval<const N: usize> {
    pub log_n_rows: u32,
}

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

// Generates the STWO trace.
pub fn generate_stwo_trace<T>(witness: &[(String, Vec<T>)], log_n_instances: u32
)-> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
    let trace: Vec<PackedBaseField> = witness
        .iter()
        .flat_map(|(_, vec)| {
            vec.chunks_exact(N_LANES)
                .map(|chunk| {
                    // Convert each chunk of Mersenne31 to an array of u32 values.
                    let array: [u32; N_LANES] = chunk
                        .iter()
                        .map(|mersenne| {
                            // Obtain a raw pointer to the `Mersenne31` value and cast it to a `*const u32`.
                            let ptr = mersenne as *const T as *const u32;

                            // Dereference the raw pointer to get the u32 value.
                            unsafe { *ptr }
                        })
                        .collect::<Vec<u32>>()
                        .try_into()
                        .expect("Chunk should be of size N_LANES");

                    // Load the array into a PackedBaseField.
                    unsafe { PackedBaseField::load(array.as_ptr()) }
                })
        })
        .collect(); // Collect the flattened iterator into a Vec<PackedBaseField>.
        println!("from generate stwo trace trace");
        println!("{:?}", trace);
    
        let mut trace_stwo= (0..2)
        .map(|_| Col::<SimdBackend, BaseField>::zeros(1 << 5))
        .collect_vec();
        // column x
        trace_stwo[0].data[0]= trace[0];
        trace_stwo[0].data[1]= trace[1];

        trace_stwo[1].data[0]= trace[2];
        trace_stwo[1].data[1]= trace[3];

        println!("from generate stwo trace trace_stwo");
        println!("{:?}", trace_stwo);

        let domain = CanonicCoset::new(5).circle_domain();
        trace_stwo
        .into_iter()
        .map(|eval| CircleEvaluation::<SimdBackend, BaseField, BitReversedOrder>::new(domain, eval))
        .collect_vec()  

      

        
}
