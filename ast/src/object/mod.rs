use std::collections::BTreeMap;

use crate::parsed::{asm::Params, Expression, PilStatement};

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

pub struct PILGraph<T> {
    pub entry_points: Vec<LinkTo<T>>,
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
    pub from: LinkFrom<T>,
    pub to: LinkTo<T>,
}

#[derive(Clone)]
pub struct LinkFrom<T> {
    pub flag: Expression<T>,
    pub params: Params,
}

#[derive(Clone)]
pub struct LinkTo<T> {
    /// the location of the machine we link to
    pub loc: Location,
    /// its latch
    pub latch: Expression<T>,
    /// its function id
    pub function_id: Expression<T>,
    /// the function we link to
    pub function: Function<T>,
}

#[derive(Clone)]
pub struct Function<T> {
    pub name: String,
    pub id: T,
    pub params: Params,
}
