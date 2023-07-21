// This is a dummy implementation of Poseidon hash,
// which will be replaced with a call to the poseidon
// coporocessor during compilation.
// The function itself will be removed by the compiler
// during the reachability analysis.
extern "C" {
    fn poseidon_coprocessor(a: u32, b: u32) -> u32;
}

pub fn poseidon_hash(a: u32, b: u32) -> u32 {
    unsafe { poseidon_coprocessor(a, b) }
}
