use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;

use bit_vec::BitVec;
use dynamic_machine::DynamicMachine;
use powdr_ast::analyzed::{self, AlgebraicExpression, ContainsNextRef, DegreeRange, PolyID};

use powdr_number::DegreeType;
use powdr_number::FieldElement;

use crate::witgen::data_structures::mutable_state::MutableState;

use self::block_machine::BlockMachine;
use self::double_sorted_witness_machine_16::DoubleSortedWitnesses16;
use self::double_sorted_witness_machine_32::DoubleSortedWitnesses32;
pub use self::fixed_lookup_machine::FixedLookup;
use self::profiling::{record_end, record_start};
use self::second_stage_machine::SecondStageMachine;
use self::sorted_witness_machine::SortedWitnesses;
use self::write_once_memory::WriteOnceMemory;

use super::data_structures::identity::{BusReceive, Identity};
use super::jit::witgen_inference::CanProcessCall;
use super::range_constraints::RangeConstraint;
use super::rows::RowPair;
use super::{EvalError, EvalResult, FixedData, QueryCallback};

mod block_machine;
mod double_sorted_witness_machine_16;
mod double_sorted_witness_machine_32;
mod dynamic_machine;
mod fixed_lookup_machine;
pub mod machine_extractor;
pub mod profiling;
mod second_stage_machine;
mod sorted_witness_machine;
mod write_once_memory;

/// A machine is a set of witness columns and identities where the columns
/// are used on the right-hand-side of lookups. It can process lookups.
pub trait Machine<'a, T: FieldElement>: Send + Sync {
    /// Runs the machine without any arguments from the first row (.
    fn run_timed<Q: QueryCallback<T>>(&mut self, mutable_state: &MutableState<'a, T, Q>) {
        record_start(self.name());
        self.run(mutable_state);
        record_end(self.name());
    }

    /// Runs the machine without any arguments from the first row.
    fn run<Q: QueryCallback<T>>(&mut self, _mutable_state: &MutableState<'a, T, Q>) {
        unimplemented!(
            "Running machine {} without a machine call is not supported.",
            self.name()
        );
    }

    /// Returns Some(..) if this machine can alway fully process a call via the given
    /// identity, the set of known arguments and a list of range constraints
    /// on the parameters. Note that the range constraints can be imposed both
    /// on inputs and on outputs.
    /// If this returns Some(..), then corresponding calls to `process_lookup_direct`
    /// are safe.
    /// The value returned inside the option is a vector of range constraints on the arguments,
    /// which again can be imposed both on inputs and on outputs.
    /// The function requires `&mut self` because it usually builds an index structure
    /// or something similar.
    fn can_process_call_fully(
        &mut self,
        _can_process: impl CanProcessCall<T>,
        _identity_id: u64,
        _known_arguments: &BitVec,
        _range_constraints: &[RangeConstraint<T>],
    ) -> Option<Vec<RangeConstraint<T>>> {
        None
    }

    /// Like `process_plookup`, but also records the time spent in this machine.
    fn process_plookup_timed<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        record_start(self.name());
        let result = self.process_plookup(mutable_state, identity_id, caller_rows);
        record_end(self.name());
        result
    }

    /// Like 'process_lookup_direct', but also records the time spent in this machine.
    fn process_lookup_direct_timed<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        values: &mut [LookupCell<'c, T>],
    ) -> Result<bool, EvalError<T>> {
        record_start(self.name());
        let result = self.process_lookup_direct(mutable_state, identity_id, values);
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
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T>;

    /// Process a connection of a given ID (which must be known to the callee).
    /// This is a more direct version of `process_plookup`, where the caller
    /// provides values or targets to where to write the results directly.
    /// The length of `values` needs to be the same as the number of expressions
    /// in the LHS / RHS of the connection.
    /// It does not allow to return range constraints or complex expressions.
    /// The boolean return value indicates whether the lookup was successful.
    /// If it returns true, all output values in `values` need to have been set.
    /// If it returns false, none of them should be changed.
    /// An error is always unrecoverable.
    fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        values: &mut [LookupCell<'c, T>],
    ) -> Result<bool, EvalError<T>>;

    /// Returns the final values of the witness columns.
    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>>;

    /// Returns the identity IDs of the connecting identities that this machine is responsible for.
    fn identity_ids(&self) -> Vec<u64>;
}

#[repr(C)]
pub enum LookupCell<'a, T> {
    /// Value is known (i.e. an input)
    Input(&'a T),
    /// Value is not known (i.e. an output)
    Output(&'a mut T),
}

impl<T> LookupCell<'_, T> {
    pub fn is_input(&self) -> bool {
        match self {
            LookupCell::Input(_) => true,
            LookupCell::Output(_) => false,
        }
    }
}

/// All known implementations of [Machine].
/// We cannot use Box<dyn Machine<..>> because the trait is not object-safe,
/// since it has generic methods.
pub enum KnownMachine<'a, T: FieldElement> {
    SecondStageMachine(SecondStageMachine<'a, T>),
    SortedWitnesses(SortedWitnesses<'a, T>),
    DoubleSortedWitnesses16(DoubleSortedWitnesses16<'a, T>),
    DoubleSortedWitnesses32(DoubleSortedWitnesses32<'a, T>),
    WriteOnceMemory(WriteOnceMemory<'a, T>),
    BlockMachine(BlockMachine<'a, T>),
    DynamicMachine(DynamicMachine<'a, T>),
    FixedLookup(FixedLookup<'a, T>),
}

/// A macro to dispatch a method call to the correct variant of [KnownMachine].
macro_rules! match_variant {
    ($s:ident, $i:ident => $e:expr) => {
        match $s {
            KnownMachine::SecondStageMachine($i) => $e,
            KnownMachine::SortedWitnesses($i) => $e,
            KnownMachine::DoubleSortedWitnesses16($i) => $e,
            KnownMachine::DoubleSortedWitnesses32($i) => $e,
            KnownMachine::WriteOnceMemory($i) => $e,
            KnownMachine::BlockMachine($i) => $e,
            KnownMachine::DynamicMachine($i) => $e,
            KnownMachine::FixedLookup($i) => $e,
        }
    };
}

impl<'a, T: FieldElement> Machine<'a, T> for KnownMachine<'a, T> {
    fn run<Q: QueryCallback<T>>(&mut self, mutable_state: &MutableState<'a, T, Q>) {
        match_variant!(self, m => m.run(mutable_state));
    }

    fn can_process_call_fully(
        &mut self,
        can_process: impl CanProcessCall<T>,
        identity_id: u64,
        known_arguments: &BitVec,
        range_constraints: &[RangeConstraint<T>],
    ) -> Option<Vec<RangeConstraint<T>>> {
        match_variant!(
            self,
            m => m.can_process_call_fully(can_process, identity_id, known_arguments, range_constraints)
        )
    }

    fn process_plookup<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        caller_rows: &'b RowPair<'b, 'a, T>,
    ) -> EvalResult<'a, T> {
        match_variant!(self, m => m.process_plookup(mutable_state, identity_id, caller_rows))
    }

    fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
        identity_id: u64,
        values: &mut [LookupCell<'c, T>],
    ) -> Result<bool, EvalError<T>> {
        match_variant!(self, m => m.process_lookup_direct(mutable_state, identity_id, values))
    }

    fn name(&self) -> &str {
        match_variant!(self, m => m.name())
    }

    fn take_witness_col_values<'b, Q: QueryCallback<T>>(
        &mut self,
        mutable_state: &'b MutableState<'a, T, Q>,
    ) -> HashMap<String, Vec<T>> {
        match_variant!(self, m => m.take_witness_col_values(mutable_state))
    }

    fn identity_ids(&self) -> Vec<u64> {
        match_variant!(self, m => m.identity_ids())
    }
}

#[derive(Clone, Copy, Debug)]
/// A connection is a witness generation directive to propagate rows across machines
pub struct Connection<'a, T> {
    pub id: u64,
    pub left: &'a analyzed::SelectedExpressions<T>,
    pub right: &'a analyzed::SelectedExpressions<T>,
    /// For [ConnectionKind::Permutation], rows of `left` are a permutation of rows of `right`. For [ConnectionKind::Lookup], all rows in `left` are in `right`.
    pub kind: ConnectionKind,
    /// If the connection comes from a phantom lookup, this is the multiplicity column.
    /// For each row of `right` it counts how often that row occurs in `left`.
    /// Note that multiple connections can share the same multiplicity column.
    pub multiplicity_column: Option<PolyID>,
}

impl<T: Display> Display for Connection<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.kind, self.right)
    }
}

impl<'a, T: FieldElement> Connection<'a, T> {
    /// Creates a connection if the identity is a bus send.
    pub fn try_new(
        identity: &'a Identity<T>,
        bus_receives: &'a BTreeMap<T, BusReceive<T>>,
    ) -> Option<Self> {
        match identity {
            Identity::BusSend(bus_interaction) => {
                let send = bus_interaction;
                let receive = send
                    .try_match_static(bus_receives)
                    .expect("No matching receive!");
                let multiplicity_column = if receive.has_arbitrary_multiplicity() {
                    receive
                        .multiplicity
                        .as_ref()
                        .and_then(|multiplicity| match multiplicity {
                            AlgebraicExpression::Reference(reference) => Some(reference.poly_id),
                            // For receives of permutations, we would have complex expressions here.
                            _ => None,
                        })
                } else {
                    // For permutations, the selector is already generated by "normal" witgen.
                    None
                };
                Some(Connection {
                    id: send.identity_id,
                    left: &send.selected_payload,
                    right: &receive.selected_payload,
                    kind: if receive.has_arbitrary_multiplicity() {
                        ConnectionKind::Lookup
                    } else {
                        ConnectionKind::Permutation
                    },
                    multiplicity_column,
                })
            }
            _ => None,
        }
    }

    fn is_permutation(&self) -> bool {
        self.kind == ConnectionKind::Permutation
    }

    fn is_lookup(&self) -> bool {
        self.kind == ConnectionKind::Lookup
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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
        let intermediate_definitions = self.fixed_data.analyzed.intermediate_definitions();
        let identities_with_next_reference = self
            .identities
            .iter()
            .filter_map(|identity| {
                identity
                    .contains_next_ref(&intermediate_definitions)
                    .then_some(*identity)
            })
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
