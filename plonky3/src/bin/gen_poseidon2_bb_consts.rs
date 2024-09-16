use itertools::Itertools;
use p3_baby_bear::BabyBear;
use p3_poseidon2::Poseidon2ExternalMatrixGeneral;
use powdr_plonky3::{
    baby_bear::{ROUNDS_F, ROUNDS_P, WIDTH},
    poseidon2,
};

fn main() {
    println!("EXTERNAL_CONSTANTS = [");
    let ec = poseidon2::external_constants::<BabyBear, WIDTH>(*ROUNDS_F);
    for row in ec {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("EXTERNAL_MDS = [");
    let mds = Poseidon2ExternalMatrixGeneral;
    // TODO: to be continued

    println!("INTERNAL_CONSTANTS = [");
    let ic = poseidon2::internal_constants::<BabyBear>(*ROUNDS_P);
    for &elem in ic.iter() {
        println!("    {},", elem);
    }
    println!("];");
}
