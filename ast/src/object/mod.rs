use std::collections::BTreeMap;

use crate::parsed::PilStatement;

mod display;

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    path: Vec<String>,
}

impl Location {
    pub fn join<S: Into<String>>(mut self, limb: S) -> Self {
        self.path.push(limb.into());
        self
    }
}

pub struct PILGraph<T> {
    pub objects: BTreeMap<Location, Object<T>>,
}

pub struct Object<T> {
    pub degree: u64,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement<T>>,
}
