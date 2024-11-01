use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;

use powdr_ast::analyzed::{self, DegreeRange, PolyID};

use powdr_number::DegreeType;
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

    /// Processes a connection of a given ID (which must be known to the callee).
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

#[derive(Clone, Copy)]
/// A connection is a witness generation directive to propagate rows across machines
pub struct Connection<'a, T> {
    pub left: &'a analyzed::SelectedExpressions<T>,
    pub right: &'a analyzed::SelectedExpressions<T>,
    /// For [ConnectionKind::Permutation], rows of `left` are a permutation of rows of `right`. For [ConnectionKind::Lookup], all rows in `left` are in `right`.
    pub kind: ConnectionKind,
}

impl<'a, T: Display> Display for Connection<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.right, self.kind)
    }
}

impl<'a, T> Connection<'a, T> {
    fn is_permutation(&self) -> bool {
        self.kind == ConnectionKind::Permutation
    }

    fn is_lookup(&self) -> bool {
        self.kind == ConnectionKind::Lookup
    }
}

impl<'a, T> TryFrom<&'a Identity<T>> for Connection<'a, T> {
    type Error = &'a Identity<T>;

    fn try_from(identity: &'a Identity<T>) -> Result<Self, Self::Error> {
        match identity {
            Identity::Lookup(i) => Ok(Connection {
                left: &i.left,
                right: &i.right,
                kind: ConnectionKind::Lookup,
            }),
            Identity::Permutation(i) => Ok(Connection {
                left: &i.left,
                right: &i.right,
                kind: ConnectionKind::Permutation,
            }),
            _ => Err(identity),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConnectionKind {
    Lookup,
    Permutation,
}

impl Display for ConnectionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectionKind::Lookup => write!(f, "in"),
            ConnectionKind::Permutation => write!(f, "is"),
        }
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
    pub connections: BTreeMap<u64, Connection<'a, T>>,
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
        connections: BTreeMap<u64, Connection<'a, T>>,
        identities: Vec<&'a Identity<T>>,
        witnesses: HashSet<PolyID>,
        prover_functions: Vec<&'a analyzed::Expression>,
    ) -> Self {
        Self {
            fixed_data,
            connections,
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
        self.connections.keys().cloned().collect()
    }

    /// Returns the name of a column.
    pub fn column_name(&self, poly_id: &PolyID) -> &str {
        self.fixed_data.column_name(poly_id)
    }
}

/// The minimum size for which a warning is logged if the used rows are less than half of the size.
/// This number coincides with 2**powdr_linker::MIN_DEGREE_LOG.
/// It's probably not worth introducing a dependency to the linker just for this constant.
const MIN_REPORTING_SIZE: DegreeType = 32;

pub fn compute_size_and_log(name: &str, used_rows: usize, degree_range: DegreeRange) -> DegreeType {
    let size = used_rows.next_power_of_two() as DegreeType;
    let size = degree_range.fit(size);
    let fraction_used = used_rows as f64 / size as f64;

    if size > MIN_REPORTING_SIZE && fraction_used < 0.5 {
        // In a machine configured to use VADCOP, we would expect the next power of two to be used.
        let percentage = fraction_used * 100.0;
        let configuration_description = degree_range
            .try_into_unique()
            .map(|unique_degree| format!("be of static size {unique_degree}"))
            .unwrap_or_else(|| format!("support sizes in the range {degree_range}"));
        log::info!(
            "Only {used_rows} of {size} rows ({percentage:.2}%) are used in machine '{name}', which is configured to {configuration_description}. \
            If the min_degree of this machine was lower, we could size it down such that the fraction of used rows is at least 50%. \
            If the backend supports it, consider lowering the min_degree.",
        );
    } else {
        log::debug!("{used_rows} of {size} rows are used in machine '{name}'.");
    }
    size
}
