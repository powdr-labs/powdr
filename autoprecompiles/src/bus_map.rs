use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt::Display};

#[derive(Copy, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum BusType<T> {
    ExecutionBridge,
    Memory,
    PcLookup,
    OpenVmBitwiseLookup,
    Other(T),
}

impl<T: Display> std::fmt::Display for BusType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BusType::ExecutionBridge => "EXECUTION_BRIDGE",
            BusType::Memory => "MEMORY",
            BusType::PcLookup => "PC_LOOKUP",
            BusType::OpenVmBitwiseLookup => "BITWISE_LOOKUP",
            BusType::Other(other_type) => &other_type.to_string(),
        };
        write!(f, "{name}")
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct BusMap<T> {
    bus_ids: BTreeMap<u64, BusType<T>>,
}

impl<T: PartialEq + Eq + Clone + Display> BusMap<T> {
    /// Construct a new `BusMap`, ensuring the same id is not used for different `BusType`s
    pub fn from_id_type_pairs(pairs: impl IntoIterator<Item = (u64, BusType<T>)>) -> Self {
        let mut bus_ids = BTreeMap::new();
        for (k, v) in pairs.into_iter() {
            bus_ids.entry(k).and_modify(|existing| {
                if existing != &v {
                    panic!("BusType `{v}` already exists under ID `{existing}`, cannot map to `{v}`");
                }
            }).or_insert(v);
        }

        BusMap { bus_ids }
    }

    /// Lookup the `BusType` for a given ID.
    pub fn bus_type(&self, bus_id: u64) -> BusType<T> {
        self.bus_ids.get(&bus_id).cloned().unwrap_or_else(|| {
            panic!("No bus type found for ID: {bus_id}");
        })
    }

    /// Insert a new bus type under the given ID, ensuring no other ID
    /// already maps to the same `BusType` value.
    pub fn with_bus_type(mut self, id: u64, bus_type: BusType<T>) -> Self {
        if let Some(existing) = self.get_bus_id(&bus_type) {
            panic!("BusType `{bus_type}` already exists under ID `{existing}`");
        }
        self.bus_ids.insert(id, bus_type);
        self
    }

    /// Extend with another `BusMap`, ensuring no duplicate `BusType`s between them.
    pub fn with_bus_map(mut self, other: BusMap<T>) -> Self {
        for (id, bus_type) in other.bus_ids.into_iter() {
            self = self.with_bus_type(id, bus_type);
        }
        self
    }

    /// View the entire map.
    pub fn all_types_by_id(&self) -> &BTreeMap<u64, BusType<T>> {
        &self.bus_ids
    }

    /// Find the ID for a given `BusType` (if any).
    pub fn get_bus_id(&self, bus_type: &BusType<T>) -> Option<u64> {
        self.bus_ids
            .iter()
            .find_map(|(id, bus)| if bus == bus_type { Some(*id) } else { None })
    }
}
