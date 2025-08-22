use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt::Display};

#[derive(Copy, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum BusType<C> {
    /// In a no-CPU architecture, instruction AIRs receive the current state and send the next state.
    /// Typically the state would include the current time stamp and program counter, but powdr does
    /// not make any assumptions about the state.
    ExecutionBridge,
    /// Memory bus for reading and writing memory.
    Memory,
    /// A lookup to fetch the instruction arguments for a given PC.
    PcLookup,
    /// Other types, specific to the VM integration. Powdr largely ignores these.
    Other(C),
}

impl<C: Display> std::fmt::Display for BusType<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            BusType::ExecutionBridge => "EXECUTION_BRIDGE",
            BusType::Memory => "MEMORY",
            BusType::PcLookup => "PC_LOOKUP",
            BusType::Other(other_type) => &other_type.to_string(),
        };
        write!(f, "{name}")
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct BusMap<C> {
    bus_ids: BTreeMap<u64, BusType<C>>,
}

impl<C: PartialEq + Eq + Clone + Display> BusMap<C> {
    /// Construct a new `BusMap`, ensuring the same id is not used for different `BusType`s
    pub fn from_id_type_pairs(pairs: impl IntoIterator<Item = (u64, BusType<C>)>) -> Self {
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
    pub fn bus_type(&self, bus_id: u64) -> BusType<C> {
        self.bus_ids.get(&bus_id).cloned().unwrap_or_else(|| {
            panic!("No bus type found for ID: {bus_id}");
        })
    }

    /// View the entire map.
    pub fn all_types_by_id(&self) -> &BTreeMap<u64, BusType<C>> {
        &self.bus_ids
    }

    /// Find the ID for a given `BusType` (if any).
    pub fn get_bus_id(&self, bus_type: &BusType<C>) -> Option<u64> {
        self.bus_ids
            .iter()
            .find_map(|(id, bus)| if bus == bus_type { Some(*id) } else { None })
    }
}
