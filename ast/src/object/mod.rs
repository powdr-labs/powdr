use std::collections::BTreeMap;

use crate::parsed::{asm::Params, PilStatement};

mod display;

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    limbs: Vec<String>,
}

impl Location {
    pub fn join<S: Into<String>>(mut self, limb: S) -> Self {
        self.limbs.push(limb.into());
        self
    }
}

pub struct PILGraph<T> {
    // the main operation in the main state machine
    pub entry_point: Operation,
    pub objects: BTreeMap<Location, Object<T>>,
}

pub struct Object<T> {
    pub degree: u64,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement<T>>,
    /// the links from this machine to its children
    pub links: Vec<Link>,
}

pub struct Link {
    pub from: LinkFrom,
    pub to: LinkTo,
}

pub struct LinkFrom {
    pub instr: Instr,
}

pub struct LinkTo {
    pub machine_ty: String,
    pub operation: Operation,
    pub loc: String,
    pub latch: Option<String>,
}

pub struct Instr {
    pub flag: String,
    pub name: String,
    pub params: Params,
}

pub struct Operation {
    pub name: String,
    pub index: Option<usize>,
    pub params: Option<Params>,
}
