use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Copy, Clone, Deserialize, Serialize, PartialEq)]
pub enum BusType {
    ExecutionBridge,
    Memory,
    PcLookup,
    VariableRangeChecker,
    BitwiseLookup,
    TupleRangeChecker,
    Sha,
}

impl std::fmt::Display for BusType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BusType::ExecutionBridge => "EXECUTION_BRIDGE",
            BusType::Memory => "MEMORY",
            BusType::PcLookup => "PC_LOOKUP",
            BusType::VariableRangeChecker => "VARIABLE_RANGE_CHECKER",
            BusType::BitwiseLookup => "BITWISE_LOOKUP",
            BusType::TupleRangeChecker => "TUPLE_RANGE_CHECKER",
            BusType::Sha => "SHA",
        };
        write!(f, "{name}")
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct BusMap {
    bus_ids: BTreeMap<u64, BusType>,
}

impl BusMap {
    pub fn new(bus_ids: BTreeMap<u64, BusType>) -> Self {
        BusMap { bus_ids }
    }

    pub fn bus_type(&self, bus_id: u64) -> BusType {
        self.bus_ids[&bus_id]
    }

    pub fn with_sha(mut self, id: u64) -> Self {
        self.bus_ids.insert(id, BusType::Sha);
        self
    }

    pub fn with_bus_type(mut self, id: u64, bus_type: BusType) -> Self {
        self.bus_ids.insert(id, bus_type);
        self
    }

    pub fn with_bus_map(mut self, bus_map: BusMap) -> Self {
        self.bus_ids.extend(bus_map.bus_ids);
        self
    }

    pub fn inner(&self) -> &BTreeMap<u64, BusType> {
        &self.bus_ids
    }

    pub fn get_bus_id(&self, bus_type: &BusType) -> Option<u64> {
        self.bus_ids
            .iter()
            .find_map(|(id, bus)| if bus == bus_type { Some(*id) } else { None })
    }
}
