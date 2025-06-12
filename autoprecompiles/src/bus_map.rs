use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Copy, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
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
    /// Construct a new `BusMap`, ensuring no duplicate `BusType` values and no duplicate ids
    pub fn from_id_type_pairs(pairs: impl IntoIterator<Item = (u64, BusType)>) -> Self {
        let mut bus_ids = BTreeMap::new();
        let mut seen_values = BTreeSet::new();
        let mut seen_keys = BTreeSet::new();

        for (k, v) in pairs.into_iter() {
            if !seen_keys.insert(k) {
                panic!("Duplicate key: {k}");
            }
            if !seen_values.insert(v) {
                panic!("Duplicate value: {v}");
            }
            bus_ids.insert(k, v);
        }

        BusMap { bus_ids }
    }

    /// Lookup the `BusType` for a given ID.
    pub fn bus_type(&self, bus_id: u64) -> BusType {
        self.bus_ids[&bus_id]
    }

    /// Insert a new bus type under the given ID, ensuring no other ID
    /// already maps to the same `BusType` value.
    pub fn with_bus_type(mut self, id: u64, bus_type: BusType) -> Self {
        if let Some(existing) = self.get_bus_id(&bus_type) {
            panic!("BusType `{bus_type:?}` already exists under ID `{existing}`");
        }
        self.bus_ids.insert(id, bus_type);
        self
    }

    /// Extend with another `BusMap`, ensuring no duplicate `BusType`s between them.
    pub fn with_bus_map(mut self, other: BusMap) -> Self {
        for (id, bus_type) in other.bus_ids.into_iter() {
            self = self.with_bus_type(id, bus_type);
        }
        self
    }

    /// View the entire map.
    pub fn all_types_by_id(&self) -> &BTreeMap<u64, BusType> {
        &self.bus_ids
    }

    /// Find the ID for a given `BusType` (if any).
    pub fn get_bus_id(&self, bus_type: &BusType) -> Option<u64> {
        self.bus_ids
            .iter()
            .find_map(|(id, bus)| if bus == bus_type { Some(*id) } else { None })
    }
}
