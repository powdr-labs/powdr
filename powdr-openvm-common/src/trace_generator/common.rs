use derive_more::From;
use openvm_circuit::system::phantom::PhantomExecutor;
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
}

#[derive(Chip, PreflightExecutor, Executor, MeteredExecutor, From, AnyEnum)]
pub enum SharedExecutor<F: PrimeField32> {
    Phantom(PhantomExecutor<F>),
}
