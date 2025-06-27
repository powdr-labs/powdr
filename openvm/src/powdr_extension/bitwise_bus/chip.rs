use openvm_stark_backend::p3_field::PrimeField32;
use std::sync::Arc;

use crate::powdr_extension::bitwise_bus::air::BitwiseLookupAir;





pub struct BitwiseLookupChip<F: PrimeField32> {
    air: Arc<BitwiseLookupAir<F>>,
}