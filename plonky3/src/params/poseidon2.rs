//! Poseidon2 constants generation.

use rand::{distributions::Standard, prelude::Distribution, Rng, SeedableRng};

pub const RNG_SEED: u64 = 42;

pub(crate) fn poseidon2_external_constants<T, const WIDTH: usize>(
    full_rounds: usize,
) -> Vec<[T; WIDTH]>
where
    Standard: Distribution<[T; WIDTH]>,
{
    rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED)
        .sample_iter(Standard)
        .take(full_rounds)
        .collect()
}

pub(crate) fn poseidon2_internal_constants<T>(partial_rounds: usize) -> Vec<T>
where
    Standard: Distribution<T>,
{
    // Use a different seed here so numbers don't repeat.
    rand_chacha::ChaCha8Rng::seed_from_u64(RNG_SEED + 1)
        .sample_iter(Standard)
        .take(partial_rounds)
        .collect()
}
