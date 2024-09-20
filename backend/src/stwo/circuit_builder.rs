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
#[derive(Debug)]
pub struct WideFibonacciEval<const N: usize> {
    pub log_n_rows: u32,
}

impl<const N: usize> FrameworkEval for WideFibonacciEval<N> {
    fn log_size(&self) -> u32 {
        self.log_n_rows
    }
    fn max_constraint_log_degree_bound(&self) -> u32 {
        self.log_n_rows + 1
    }
    fn evaluate<E: EvalAtRow>(&self, mut eval: E) -> E {
        let mut a = eval.next_trace_mask();
        let mut b = eval.next_trace_mask();
        for _ in 2..N {
            let c = eval.next_trace_mask();
            eval.add_constraint(c - (a + b));
            a = b;
            b = c;
        }
        eval
    }
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

pub fn generate_parallel_stwo_trace_by_witness_repitition<T: Clone>(witness: &[(String, Vec<T>)], log_n_instances: u32
)-> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {

    let trace: Vec<PackedBaseField> = witness
    .iter()
    .flat_map(|(_, vec)| {
        vec.iter().flat_map(|mersenne| {

            let ptr = mersenne as *const T as *const u32;

            let value = unsafe {
                *ptr // Dereference the pointer to get the u32 value
            };

            // Repeat the value 32 times
            let repeated = vec![value; 32];

            // Split the repeated vector into two chunks of 16 elements each
            let chunk1: [u32; N_LANES] = repeated[0..16]
                .try_into()
                .expect("Chunk should be of size N_LANES");
            let chunk2: [u32; N_LANES] = repeated[16..32]
                .try_into()
                .expect("Chunk should be of size N_LANES");

            // Convert chunks to PackedBaseField
            // Note: We use unsafe block because PackedBaseField::load is unsafe
            unsafe {
                vec![
                    PackedBaseField::load(chunk1.as_ptr()),
                    PackedBaseField::load(chunk2.as_ptr()),
                ]
            }
        })
    })
    .collect(); // Collect the flattened iterator into a Vec<PackedBaseField>


        println!("from generate stwo trace trace");
        println!("{:?}", trace);

        let mut trace_stwo= (0..6)//fibonacci length
        .map(|_| Col::<SimdBackend, BaseField>::zeros(1 << log_n_instances))
        .collect_vec();

        
        // column x
        trace_stwo[0].data[0]= trace[0];
        trace_stwo[0].data[1]= trace[1];

        println!("from generate stwo trace trace 64 ......");
        println!("{:?}", trace[64]);

        println!("from generate stwo trace trace 65 ......");
        println!("{:?}", trace[65]);

        trace_stwo[1].data[0]= trace[64];
        trace_stwo[1].data[1]= trace[65];

        trace_stwo[2].data[0]= trace[66];
        trace_stwo[2].data[1]= trace[67];

        trace_stwo[3].data[0]= trace[68];
        trace_stwo[3].data[1]= trace[69];

        trace_stwo[4].data[0]= trace[70];
        trace_stwo[4].data[1]= trace[71];

        trace_stwo[5].data[0]= trace[72];
        trace_stwo[5].data[1]= trace[73];

        println!("from generate stwo trace trace_stwo repititions");
        println!("{:?}", trace_stwo);

        let domain = CanonicCoset::new(5).circle_domain();
        trace_stwo
        .into_iter()
        .map(|eval| CircleEvaluation::<SimdBackend, BaseField, BitReversedOrder>::new(domain, eval))
        .collect_vec()  

      

        
}
