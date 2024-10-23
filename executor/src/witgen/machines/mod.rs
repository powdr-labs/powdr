use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;

use powdr_ast::analyzed::{self, DegreeRange, LookupIdentity, PermutationIdentity, PolyID};

use powdr_number::FieldElement;

use crate::Identity;

use self::block_machine::BlockMachine;
use self::double_sorted_witness_machine_16::DoubleSortedWitnesses16;
use self::double_sorted_witness_machine_32::DoubleSortedWitnesses32;
pub use self::fixed_lookup_machine::FixedLookup;
use self::profiling::{record_end, record_start};
use self::sorted_witness_machine::SortedWitnesses;
use self::write_once_memory::WriteOnceMemory;

use super::generator::Generator;
use super::rows::RowPair;
use super::{EvalResult, FixedData, MutableState, QueryCallback};

mod block_machine;
mod double_sorted_witness_machine_16;
mod double_sorted_witness_machine_32;
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
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>>;

    /// Returns the identity IDs of the connecting identities that this machine is responsible for.
    fn identity_ids(&self) -> Vec<u64>;
}

/// All known implementations of [Machine].
/// This allows us to treat machines uniformly without putting them into a `Box`,
/// which requires that all lifetime parameters are 'static.
pub enum KnownMachine<'a, T: FieldElement> {
    SortedWitnesses(SortedWitnesses<'a, T>),
    DoubleSortedWitnesses16(DoubleSortedWitnesses16<'a, T>),
    DoubleSortedWitnesses32(DoubleSortedWitnesses32<'a, T>),
    WriteOnceMemory(WriteOnceMemory<'a, T>),
    BlockMachine(BlockMachine<'a, T>),
    Vm(Generator<'a, T>),
    FixedLookup(FixedLookup<'a, T>),
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
            KnownMachine::DoubleSortedWitnesses16(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::DoubleSortedWitnesses32(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::WriteOnceMemory(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::BlockMachine(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
            KnownMachine::Vm(m) => m.process_plookup(mutable_state, identity_id, caller_rows),
            KnownMachine::FixedLookup(m) => {
                m.process_plookup(mutable_state, identity_id, caller_rows)
            }
        }
    }

    fn name(&self) -> &str {
        match self {
            KnownMachine::SortedWitnesses(m) => m.name(),
            KnownMachine::DoubleSortedWitnesses16(m) => m.name(),
            KnownMachine::DoubleSortedWitnesses32(m) => m.name(),
            KnownMachine::WriteOnceMemory(m) => m.name(),
            KnownMachine::BlockMachine(m) => m.name(),
            KnownMachine::Vm(m) => m.name(),
            KnownMachine::FixedLookup(m) => m.name(),
        }
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        match self {
            KnownMachine::SortedWitnesses(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::DoubleSortedWitnesses16(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::DoubleSortedWitnesses32(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::WriteOnceMemory(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::BlockMachine(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::Vm(m) => m.take_witness_col_values(mutable_state),
            KnownMachine::FixedLookup(m) => m.take_witness_col_values(mutable_state),
        }
    }

    fn identity_ids(&self) -> Vec<u64> {
        match self {
            KnownMachine::SortedWitnesses(m) => m.identity_ids(),
            KnownMachine::DoubleSortedWitnesses16(m) => m.identity_ids(),
            KnownMachine::DoubleSortedWitnesses32(m) => m.identity_ids(),
            KnownMachine::WriteOnceMemory(m) => m.identity_ids(),
            KnownMachine::BlockMachine(m) => m.identity_ids(),
            KnownMachine::Vm(m) => m.identity_ids(),
            KnownMachine::FixedLookup(m) => m.identity_ids(),
        }
    }
}

#[derive(Copy, Clone)]
/// A reference to a connecting identity, which is either a permutation or a lookup.
pub enum ConnectingIdentityRef<'a, T> {
    Lookup(&'a LookupIdentity<T>),
    Permutation(&'a PermutationIdentity<T>),
}

impl<'a, T: Display> Display for ConnectingIdentityRef<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectingIdentityRef::Lookup(i) => write!(f, "{i}"),
            ConnectingIdentityRef::Permutation(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ConnectionType {
    Lookup,
    Permutation,
}

impl<'a, T> ConnectingIdentityRef<'a, T> {
    pub fn left(&self) -> &'a analyzed::SelectedExpressions<T> {
        match self {
            ConnectingIdentityRef::Lookup(i) => &i.left,
            ConnectingIdentityRef::Permutation(i) => &i.left,
        }
    }

    pub fn right(&self) -> &'a analyzed::SelectedExpressions<T> {
        match self {
            ConnectingIdentityRef::Lookup(i) => &i.right,
            ConnectingIdentityRef::Permutation(i) => &i.right,
        }
    }

    fn id(&self) -> u64 {
        match self {
            ConnectingIdentityRef::Lookup(i) => i.id,
            ConnectingIdentityRef::Permutation(i) => i.id,
        }
    }

    fn kind(&self) -> ConnectionType {
        match self {
            ConnectingIdentityRef::Lookup(_) => ConnectionType::Lookup,
            ConnectingIdentityRef::Permutation(_) => ConnectionType::Permutation,
        }
    }

    fn is_lookup(&self) -> bool {
        matches!(self, ConnectingIdentityRef::Lookup(_))
    }

    fn is_permutation(&self) -> bool {
        matches!(self, ConnectingIdentityRef::Permutation(_))
    }
}

/// The parts of Analyzed that are assigned to a machine.
/// Also includes FixedData for convenience.
#[derive(Clone)]
pub struct MachineParts<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    /// Connecting identities, indexed by their ID.
    /// These are the identities that connect another machine to this one,
    /// where this one is on the RHS of a lookup.
    pub connecting_identities: BTreeMap<u64, ConnectingIdentityRef<'a, T>>,
    /// Identities relevant to this machine and only this machine.
    pub identities: Vec<&'a Identity<T>>,
    /// Witness columns relevant to this machine.
    pub witnesses: HashSet<PolyID>,
    /// Prover functions that are relevant for this machine.
    pub prover_functions: Vec<&'a analyzed::Expression>,
}

impl<'a, T: FieldElement> MachineParts<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        connecting_identities: BTreeMap<u64, ConnectingIdentityRef<'a, T>>,
        identities: Vec<&'a Identity<T>>,
        witnesses: HashSet<PolyID>,
        prover_functions: Vec<&'a analyzed::Expression>,
    ) -> Self {
        Self {
            fixed_data,
            connecting_identities,
            identities,
            witnesses,
            prover_functions,
        }
    }

    /// Returns a copy of the machine parts but only containing identities that
    /// have a "next" reference.
    pub fn restricted_to_identities_with_next_references(&self) -> MachineParts<'a, T> {
        let identities_with_next_reference = self
            .identities
            .iter()
            .filter_map(|identity| identity.contains_next_ref().then_some(*identity))
            .collect::<Vec<_>>();
        Self {
            identities: identities_with_next_reference,
            ..self.clone()
        }
    }

    /// Returns the common degree of the witness columns.
    pub fn common_degree_range(&self) -> DegreeRange {
        self.fixed_data.common_degree_range(&self.witnesses)
    }

    /// Returns the IDs of the connecting identities.
    pub fn identity_ids(&self) -> Vec<u64> {
        self.connecting_identities.keys().cloned().collect()
    }

    /// Returns the name of a column.
    pub fn column_name(&self, poly_id: &PolyID) -> &str {
        self.fixed_data.column_name(poly_id)
    }
}
