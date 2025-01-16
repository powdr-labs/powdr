use crate::poseidon2::{external_constants, internal_constants};

use super::{D, ROUNDS_F, ROUNDS_P, WIDTH};
use lazy_static::lazy_static;
use p3_goldilocks::{DiffusionMatrixGoldilocks, Goldilocks};
use p3_poseidon2::{Poseidon2, Poseidon2ExternalMatrixGeneral};

/// The Poseidon2 permutation type.
pub type Permutation =
    Poseidon2<Goldilocks, Poseidon2ExternalMatrixGeneral, DiffusionMatrixGoldilocks, WIDTH, D>;

lazy_static! {
    pub static ref PERM: Permutation = Permutation::new(
        *ROUNDS_F,
        external_constants(*ROUNDS_F),
        Poseidon2ExternalMatrixGeneral,
        *ROUNDS_P,
        internal_constants(*ROUNDS_P),
        DiffusionMatrixGoldilocks,
    );
}
