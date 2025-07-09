#![cfg_attr(not(feature = "std"), no_main)]
#![cfg_attr(not(feature = "std"), no_std)]

openvm::entry!(main);

mod sub_mod;
use sub_mod::{exp2, FINAL_EXPONENT, u27_coeff_0};

fn main() {
    exp2();
    FINAL_EXPONENT;
    u27_coeff_0();
    
}