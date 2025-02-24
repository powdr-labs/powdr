use itertools::Itertools;
use num_traits::Zero;
use stwo_prover::core::backend::simd::column::BaseColumn;
use stwo_prover::core::backend::simd::column::SecureColumn;
use stwo_prover::core::backend::simd::conversion::Pack;
use stwo_prover::core::backend::simd::m31::PackedBaseField;
use stwo_prover::core::backend::simd::m31::LOG_N_LANES;
use stwo_prover::core::backend::simd::qm31::{PackedQM31, PackedSecureField};
use stwo_prover::core::backend::simd::SimdBackend;
use stwo_prover::core::backend::Column;
use stwo_prover::core::backend::ColumnOps;
use stwo_prover::core::fields::m31::BaseField;
use stwo_prover::core::fields::qm31::{SecureField, QM31};
use stwo_prover::core::fields::secure_column::SecureColumnByCoords;
use stwo_prover::core::fields::FieldExpOps;
use stwo_prover::core::poly::circle::CanonicCoset;
use stwo_prover::core::poly::circle::CircleEvaluation;
use stwo_prover::core::poly::BitReversedOrder;
use stwo_prover::core::utils::bit_reverse_index;
use stwo_prover::core::ColumnVec;
use stwo_prover::core::fields::m31::M31;
use stwo_prover::core::backend::Col;
use stwo_prover::core::utils::coset_index_to_circle_domain_index;

pub struct PowdrLogupTraceGenerator {
    log_size: u32,
    /// Current allocated interaction columns.
    pub trace: Vec<SecureColumnByCoords<SimdBackend>>,
    /// Denominator expressions (z + sum_i alpha^i * x_i) being generated for the current lookup.
    denom: SecureColumn,
}

impl PowdrLogupTraceGenerator {
    pub fn new(log_size: u32) -> Self {
        let trace = vec![];
        let denom = SecureColumn::zeros(1 << log_size);
        Self {
            log_size,
            trace,
            denom,
        }
    }

    /// Allocate a new lookup column.
    pub fn new_col(&mut self) -> PowdrLogupColGenerator<'_> {
        let log_size = self.log_size;
        PowdrLogupColGenerator {
            gen: self,
            numerator: SecureColumnByCoords::<SimdBackend>::zeros(1 << log_size),
            log_size,
        }
    }
}

/// Trace generator for a single lookup column.

pub struct PowdrLogupColGenerator<'a> {
    gen: &'a mut PowdrLogupTraceGenerator,
    /// Numerator expressions (i.e. multiplicities) being generated for the current lookup.
    numerator: SecureColumnByCoords<SimdBackend>,
    log_size: u32,
}
impl PowdrLogupColGenerator<'_> {
    /// Write a fraction to the column at a row.
    pub fn write_frac(
        &mut self,
        vec_row: usize,
        numerator: PackedSecureField,
        denom: PackedSecureField,
    ) {
        debug_assert!(
            denom.to_array().iter().all(|x| *x != SecureField::zero()),
            "{:?}",
            ("denom at vec_row {} is zero {}", denom, vec_row)
        );
        unsafe {
            self.numerator.set_packed(vec_row, numerator);
            *self.gen.denom.data.get_unchecked_mut(vec_row) = denom;
        }
    }

    /// Finalizes generating the column.
    pub fn finalize_col(
        mut self,
    ) -> ColumnVec<CircleEvaluation<SimdBackend, BaseField, BitReversedOrder>> {
        let denom_inv = PackedSecureField::batch_inverse(&self.gen.denom.data);

        #[allow(clippy::needless_range_loop)]
        for vec_row in 0..(1 << (self.gen.log_size - LOG_N_LANES)) {
            unsafe {
                let value = self.numerator.packed_at(vec_row) * denom_inv[vec_row];
                let prev_value = self
                    .gen
                    .trace
                    .last()
                    .map(|col| col.packed_at(vec_row))
                    .unwrap_or_else(PackedSecureField::zero);
                self.numerator.set_packed(vec_row, value + prev_value)
            };
        }

        self.gen.trace.push(self.numerator.clone());

        

        let mut last_value = QM31::zero();

        #[allow(clippy::needless_range_loop)]
        for vec_row in 0..(1 << (self.gen.log_size - LOG_N_LANES)) {
            unsafe {
                let value = self.gen.trace.last().unwrap().packed_at(vec_row);
                let acc_pack_array: [QM31; 16] = value
                    .to_array()
                    .iter()
                    .scan(last_value, |state, &x| {
                        *state += x;
                        Some(*state)
                    })
                    .collect::<Vec<QM31>>() // Collect into a Vec<QM31>
                    .try_into() // Convert Vec<QM31> into [QM31; 16]
                    .expect("Expected exactly 16 elements");
                last_value=acc_pack_array[15];
                
                let acc_pack = PackedQM31::from_array(acc_pack_array);
                
                self.numerator.set_packed(vec_row, acc_pack);
            };
        }
        self.gen.trace.push(self.numerator);

   

        


        let trace = self
            .gen
            .trace
            .clone()
            .iter()
            .flat_map(|eval| {
                eval.clone().columns.map(|col| {
                    let nature_order_col =  col.into_cpu_vec();
                    let mut bit_reversed_col =Col::<SimdBackend, BaseField>::zeros(1 << self.log_size);
                    for index in 0..(1 << self.log_size) {
                        bit_reversed_col.set(
                            bit_reverse_index(
                                coset_index_to_circle_domain_index(
                                    index,
                                    self.log_size,
                                ),
                                self.log_size,
                            ),
                            nature_order_col[index],
                        );
                    }
                    CircleEvaluation::new(CanonicCoset::new(self.log_size).circle_domain(), bit_reversed_col)
                })
            })
            .collect_vec();
        
       trace[trace.len().saturating_sub(4)..].to_vec()
       
    }

}
