use itertools::Itertools;
use p3_poseidon2::Poseidon2ExternalMatrixGeneral;
use powdr_plonky3::baby_bear;

fn main() {
    println!("EXTERNAL_CONSTANTS = [");
    let ec = baby_bear::poseidon2_external_constants();
    for row in ec {
        println!("    [{}],", row.into_iter().format(", "));
    }
    println!("];");

    println!("EXTERNAL_MDS = [");
    let mds = Poseidon2ExternalMatrixGeneral;
    // TODO: to be continued

    println!("INTERNAL_CONSTANTS = [");
    let ic = baby_bear::poseidon2_internal_constants();
    for &elem in ic.iter() {
        println!("    {},", elem);
    }
    println!("];");
}
