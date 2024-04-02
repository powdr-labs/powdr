use ark_ff::{Fp64, MontBackend, MontConfig};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Plonky3FieldElement;

#[derive(MontConfig)]
#[modulus = "2013265921"]
#[generator = "31"]
pub struct BabyBearBaseFieldConfig;
pub type BabyBearBaseField = Fp64<MontBackend<BabyBearBaseFieldConfig, 1>>;

powdr_field!(BabyBearField, BabyBearBaseField);

impl Plonky3FieldElement for BabyBearField {
    type Plonky3Field = p3_baby_bear::BabyBear;
}