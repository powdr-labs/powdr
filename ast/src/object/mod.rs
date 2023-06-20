use std::collections::BTreeMap;

use crate::parsed::{
    asm::{InstructionParams, Latch},
    PilStatement,
};

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
    pub links: Vec<Link<T>>,
}

pub struct Link<T> {
    pub from: LinkFrom,
    pub to: LinkTo<T>,
}

// todo: deal with reset flag

pub struct LinkFrom {
    pub instr: ExtInstr,
}

pub struct LinkTo<T> {
    pub machine_ty: String,
    pub instr: Instr<T>,
    pub loc: String,
}

pub struct ExtInstr {
    pub flag: String,
    pub name: String,
    pub params: InstructionParams,
}

pub struct Instr<T> {
    pub latch: Option<Latch<T>>,
    pub name: String,
    pub params: Option<InstructionParams>,
}
