// This is a dummy implementation of Poseidon hash,
// which will be replaced with a call to the Poseidon
// coprocessor during compilation.
// The function itself will be removed by the compiler
// during the reachability analysis.
extern "C" {
    fn poseidon_gl_coprocessor(data: *mut [u64; 12]);
}

const GOLDILOCKS: u64 = 0xffffffff00000001;

/// Calls the low level Poseidon coprocessor in PIL, where
/// the last 4 elements are the "cap"
/// and the return value is placed in data[0:4].
/// The safe version below also checks that each u64 element
/// is less than the Goldilocks field.
/// The unsafe version does not perform such checks.
pub fn poseidon_gl(mut data: [u64; 12]) -> [u64; 4] {
    for &n in data.iter() {
        assert!(n < GOLDILOCKS);
    }

    unsafe {
        poseidon_gl_coprocessor(&mut data as *mut [u64; 12]);
    }

    [data[0], data[1], data[2], data[3]]
}

pub fn poseidon_gl_unsafe(mut data: [u64; 12]) -> [u64; 4] {
    unsafe {
        poseidon_gl_coprocessor(&mut data as *mut [u64; 12]);
    }

    [data[0], data[1], data[2], data[3]]
}
