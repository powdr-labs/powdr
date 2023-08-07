use std::collections::BTreeMap;

use crate::parsed::{asm::Params, PilStatement};

mod display;

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
    pub degree: Option<u64>,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement<T>>,
    /// the links from this machine to its children
    pub links: Vec<Link<T>>,
}

#[derive(Clone)]
pub struct Link<T> {
    pub from: LinkFrom,
    pub to: LinkTo<T>,
}

#[derive(Clone)]
pub struct LinkFrom {
    pub instr: Instr,
}

#[derive(Clone)]
pub struct LinkTo<T> {
    /// the location of the machine we link to
    pub loc: Location,
    /// its latch
    pub latch: String,
    /// its function id
    pub function_id: String,
    /// the function we link to
    pub function: Function<T>,
}

#[derive(Clone)]
pub struct Instr {
    pub name: String,
    pub flag: String,
    pub params: Params,
}

#[derive(Clone)]
pub struct Function<T> {
    pub name: String,
    pub id: T,
    pub params: Params,
}
