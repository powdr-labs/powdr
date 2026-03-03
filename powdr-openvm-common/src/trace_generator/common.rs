use derive_more::From;
use openvm_circuit::system::{phantom::PhantomExecutor, SystemExecutor};
use openvm_circuit_derive::{AnyEnum, Executor, MeteredExecutor, PreflightExecutor};
use openvm_circuit_primitives::Chip;
use openvm_stark_backend::p3_field::PrimeField32;

use crate::isa::OpenVmISA;

#[allow(clippy::large_enum_variant)]
#[derive(Chip, PreflightExecutor, Executor, MeteredExecutor, AnyEnum)]
pub enum DummyExecutor<F: PrimeField32, ISA: OpenVmISA> {
    #[any_enum]
    Base(ISA::OriginalExecutor<F>),
    #[any_enum]
    Shared(SharedExecutor<F>),
    #[any_enum]
    /// We keep the `SystemExecutor` variant to allow for system-level operations. Failing to do this compiles, but at runtime since the `PhantomChip` cannot be found
    /// This seems like a bug in openvm, as it breaks abstraction: wrapping an executor should not require the user to know about the system executor.
    System(SystemExecutor<F>),
}

#[derive(Chip, PreflightExecutor, Executor, MeteredExecutor, From, AnyEnum)]
pub enum SharedExecutor<F: PrimeField32> {
    Phantom(PhantomExecutor<F>),
}
