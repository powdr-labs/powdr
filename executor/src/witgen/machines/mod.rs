use std::collections::HashMap;

use powdr_number::FieldElement;

use self::block_machine::BlockMachine;
use self::double_sorted_witness_machine::DoubleSortedWitnesses;
pub use self::fixed_lookup_machine::FixedLookup;
use self::profiling::record_end;
use self::profiling::record_start;
use self::sorted_witness_machine::SortedWitnesses;
use self::write_once_memory::WriteOnceMemory;

use super::generator::Generator;
use super::rows::RowPair;
use super::EvalResult;
use super::FixedData;
use super::MutableState;
use super::QueryCallback;

mod block_machine;
mod double_sorted_witness_machine;
mod fixed_lookup_machine;
pub mod machine_extractor;
pub mod profiling;
mod sorted_witness_machine;
mod write_once_memory;

/// A machine is a set of witness columns and identities where the columns
/// are used on the right-hand-side of lookups. It can process lookups.
pub trait Machine<'a, T: FieldElement>: Send + Sync {
    /// Like `process_plookup`, but also records the time spent in this machine.
    fn process_plookup_timed<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        record_start(self.name());
        let result = self.process_plookup(mutable_state, identity_id, caller_rows);
        record_end(self.name());
        result
    }

    /// Returns a unique name for this machine.
    fn name(&self) -> &str;

    /// Processes a connecting identity of a given ID (which must be known to the callee).
    /// Returns an error if the query leads to a constraint failure.
    /// Otherwise, it computes any updates to the caller row pair and returns them.
    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T>;

    /// Returns the final values of the witness columns.
    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        fixed_lookup: &'b mut FixedLookup<T>,
        query_callback: &'b mut Q,
    ) -> HashMap<String, Vec<T>>;

    /// Returns the identity IDs that this machine is responsible for.
    fn identity_ids(&self) -> Vec<u64>;
}

/// All known implementations of [Machine].
/// This allows us to treat machines uniformly without putting them into a `Box`,
/// which requires that all lifetime parameters are 'static.
pub enum KnownMachine<'a, T: FieldElement> {
    SortedWitnesses(SortedWitnesses<'a, T>),
    DoubleSortedWitnesses(DoubleSortedWitnesses<'a, T>),
    WriteOnceMemory(WriteOnceMemory<'a, T>),
    BlockMachine(BlockMachine<'a, T>),
    Vm(Generator<'a, T>),
}

impl<'a, T: FieldElement> Machine<'a, T> for KnownMachine<'a, T> {
    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        match self {
            KnownMachine::SortedWitnesses(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::DoubleSortedWitnesses(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::WriteOnceMemory(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::BlockMachine(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::Vm(m) => m.process_plookup(mutable_state, identity_id, caller_rows),
        }
    }

    fn name(&self) -> &str {
        match self {
            KnownMachine::SortedWitnesses(m) => m.name(),
            KnownMachine::DoubleSortedWitnesses(m) => m.name(),
            KnownMachine::WriteOnceMemory(m) => m.name(),
            KnownMachine::BlockMachine(m) => m.name(),
            KnownMachine::Vm(m) => m.name(),
        }
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        fixed_lookup: &'b mut FixedLookup<T>,
        query_callback: &'b mut Q,
    ) -> HashMap<String, Vec<T>> {
        match self {
            KnownMachine::SortedWitnesses(m) => {
                m.take_witness_col_values(fixed_lookup, query_callback)
            }
            KnownMachine::DoubleSortedWitnesses(m) => {
                m.take_witness_col_values(fixed_lookup, query_callback)
            }
            KnownMachine::WriteOnceMemory(m) => {
                m.take_witness_col_values(fixed_lookup, query_callback)
            }
            KnownMachine::BlockMachine(m) => {
                m.take_witness_col_values(fixed_lookup, query_callback)
            }
            KnownMachine::Vm(m) => m.take_witness_col_values(fixed_lookup, query_callback),
        }
    }

    fn identity_ids(&self) -> Vec<u64> {
        match self {
            KnownMachine::SortedWitnesses(m) => m.identity_ids(),
            KnownMachine::DoubleSortedWitnesses(m) => m.identity_ids(),
            KnownMachine::WriteOnceMemory(m) => m.identity_ids(),
            KnownMachine::BlockMachine(m) => m.identity_ids(),
            KnownMachine::Vm(m) => m.identity_ids(),
        }
    }
}
