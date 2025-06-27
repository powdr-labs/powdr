use openvm::io::{println, read, reveal_bytes32};
use openvm_client_executor::{io::ClientExecutorInput, ClientExecutor};
// Imports needed by the linker, but clippy can't tell:
#[allow(unused_imports, clippy::single_component_path_imports)]
use {
    k256::Secp256k1Point,
    openvm_algebra_guest::IntMod,
    openvm_keccak256_guest, // trigger extern native-keccak256
    openvm_pairing::{bls12_381::Bls12_381G1Affine, bn254::Bn254G1Affine},
};

openvm::init!();

pub fn main() {
    println("client-eth starting");
    // Read the input.
    let input: ClientExecutorInput = read();
    println("finished reading input");

    // Execute the block.
    let executor = ClientExecutor;
    let header = executor.execute(input).expect("failed to execute client");
    let block_hash = header.hash_slow();

    // Reveal the block hash.
    reveal_bytes32(*block_hash);
}
