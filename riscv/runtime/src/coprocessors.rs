// This is a dummy implementation of Poseidon hash,
// which will be replaced with a call to the poseidon
// coporocessor during compilation.
// The function itself will be removed by the compiler
// during the reachability analysis.
#[no_mangle]
#[inline(never)]
pub fn poseidon_hash(_a: u32, _b: u32) -> u32 {
    0
}
