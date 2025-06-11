//! To support an abstracted autoprecompile layer, this module stores type implementations specific to OpenVM
use crate::bus_map::{BusMap, BusType};

pub const DEFAULT_EXECUTION_BRIDGE: u64 = 0;
pub const DEFAULT_MEMORY: u64 = 1;
pub const DEFAULT_PC_LOOKUP: u64 = 2;
pub const DEFAULT_VARIABLE_RANGE_CHECKER: u64 = 3;
pub const DEFAULT_BITWISE_LOOKUP: u64 = 6;
pub const DEFAULT_TUPLE_RANGE_CHECKER: u64 = 7;
pub const COPY_CONSTRAINT_LOOKUP: u64 = 10;

pub fn default_openvm_bus_map() -> BusMap {
    let bus_ids = [
        (DEFAULT_EXECUTION_BRIDGE, BusType::ExecutionBridge),
        (DEFAULT_MEMORY, BusType::Memory),
        (DEFAULT_PC_LOOKUP, BusType::PcLookup),
        (
            DEFAULT_VARIABLE_RANGE_CHECKER,
            BusType::VariableRangeChecker,
        ),
        (DEFAULT_BITWISE_LOOKUP, BusType::BitwiseLookup),
        (DEFAULT_TUPLE_RANGE_CHECKER, BusType::TupleRangeChecker),
    ]
    .into_iter()
    .collect();
    BusMap::new(bus_ids)
}

pub fn openvm_base_with_copy_constraint() -> BusMap {
    let bus_ids = [
        (DEFAULT_EXECUTION_BRIDGE, BusType::ExecutionBridge),
        (DEFAULT_MEMORY, BusType::Memory),
        (DEFAULT_PC_LOOKUP, BusType::PcLookup),
        (
            DEFAULT_VARIABLE_RANGE_CHECKER,
            BusType::VariableRangeChecker,
        ),
        (DEFAULT_BITWISE_LOOKUP, BusType::BitwiseLookup),
        (DEFAULT_TUPLE_RANGE_CHECKER, BusType::TupleRangeChecker),
        (COPY_CONSTRAINT_LOOKUP, BusType::CopyConstraintLookup),
    ]
    .into_iter()
    .collect();
    BusMap::new(bus_ids)
}
