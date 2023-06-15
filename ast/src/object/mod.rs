use std::collections::BTreeMap;

use crate::parsed::{asm::InstructionParams, PilStatement};

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

#[derive(Default)]
pub struct PILGraph<T> {
    pub objects: BTreeMap<Location, Object<T>>,
}

pub struct Object<T> {
    pub degree: u64,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement<T>>,
    /// not sure yet what this is, right now just references to other machines but this is instruction-level stuff, not machine level
    pub links: Vec<Link>,
}

pub struct Link {
    pub from: LinkFrom,
    pub to: LinkTo,
}

// todo: deal with reset flag

pub struct LinkFrom {
    pub instr: ExtInstr,
}

pub struct LinkTo {
    pub machine_ty: String,
    pub instr: Instr,
    pub loc: String,
}

pub struct ExtInstr {
    pub flag: String,
    pub name: String,
    pub params: InstructionParams,
}

pub struct Instr {
    pub latch: (),
    pub name: String,
    pub params: Option<InstructionParams>,
}
