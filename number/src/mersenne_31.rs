use ark_ff::{Fp64, MontBackend, MontConfig};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Plonky3FieldElement;

#[derive(MontConfig)]
#[modulus = "2147483647"]
#[generator = "7"]
pub struct Mersenne31BaseFieldConfig;
pub type Mersenne31BaseField = Fp64<MontBackend<Mersenne31BaseFieldConfig, 1>>;

powdr_field!(Mersenne31Field, Mersenne31BaseField);

impl Plonky3FieldElement for Mersenne31Field {
    type Plonky3Field = p3_mersenne_31::Mersenne31;
}