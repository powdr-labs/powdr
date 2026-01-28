//! To support an abstracted autoprecompile layer, this module stores type implementations specific to OpenVM
use std::fmt::Display;

use crate::bus_map::BusType;
use serde::{Deserialize, Serialize};

pub const DEFAULT_EXECUTION_BRIDGE: u64 = 0;
pub const DEFAULT_MEMORY: u64 = 1;
pub const DEFAULT_PC_LOOKUP: u64 = 2;
pub const DEFAULT_VARIABLE_RANGE_CHECKER: u64 = 3;
pub const DEFAULT_BITWISE_LOOKUP: u64 = 6;
pub const DEFAULT_TUPLE_RANGE_CHECKER: u64 = 7;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum OpenVmBusType {
    VariableRangeChecker,
    TupleRangeChecker,
    BitwiseLookup,
}

pub type BusMap = crate::bus_map::BusMap<OpenVmBusType>;

impl Display for OpenVmBusType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenVmBusType::VariableRangeChecker => write!(f, "VARIABLE_RANGE_CHECKER"),
            OpenVmBusType::TupleRangeChecker => write!(f, "TUPLE_RANGE_CHECKER"),
            OpenVmBusType::BitwiseLookup => write!(f, "BITWISE_LOOKUP"),
        }
    }
}

pub fn default_openvm_bus_map() -> BusMap {
    let bus_ids = [
        (DEFAULT_EXECUTION_BRIDGE, BusType::ExecutionBridge),
        (DEFAULT_MEMORY, BusType::Memory),
        (DEFAULT_PC_LOOKUP, BusType::PcLookup),
        (
            DEFAULT_VARIABLE_RANGE_CHECKER,
            BusType::Other(OpenVmBusType::VariableRangeChecker),
        ),
        (
            DEFAULT_BITWISE_LOOKUP,
            BusType::Other(OpenVmBusType::BitwiseLookup),
        ),
        (
            DEFAULT_TUPLE_RANGE_CHECKER,
            BusType::Other(OpenVmBusType::TupleRangeChecker),
        ),
    ];
    BusMap::from_id_type_pairs(bus_ids)
}
