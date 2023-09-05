use std::collections::BTreeMap;

use crate::parsed::{asm::Params, Expression, PilStatement};

mod display;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Location {
    limbs: Vec<String>,
}

impl Location {
    pub fn main() -> Self {
        Self {
            limbs: vec!["main".into()],
        }
    }

    pub fn join<S: Into<String>>(mut self, limb: S) -> Self {
        self.limbs.push(limb.into());
        self
    }
}

pub struct PILGraph<T> {
    pub main: Machine,
    pub entry_points: Vec<Operation<T>>,
    pub objects: BTreeMap<Location, Object<T>>,
}

#[derive(Default)]
pub struct Object<T> {
    pub degree: Option<u64>,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement<T>>,
    /// the links from this machine to its children
    pub links: Vec<Link<T>>,
}

impl<T> Object<T> {
    pub fn with_degree(mut self, degree: Option<u64>) -> Self {
        self.degree = degree;
        self
    }
}

#[derive(Clone)]
/// A link between two machines
pub struct Link<T> {
    /// the link source, i.e. a flag and some arguments
    pub from: LinkFrom<T>,
    /// the link target, i.e. a callable in some machine
    pub to: LinkTo<T>,
}

#[derive(Clone)]
pub struct LinkFrom<T> {
    pub flag: Expression<T>,
    pub params: Params,
}

#[derive(Clone)]
pub struct LinkTo<T> {
    /// the machine we link to
    pub machine: Machine,
    /// the operation we link to
    pub operation: Operation<T>,
}

#[derive(Clone)]
pub struct Machine {
    /// the location of this instance
    pub location: Location,
    /// its latch
    pub latch: String,
    /// its operation id
    pub operation_id: String,
}

#[derive(Clone)]
pub struct Operation<T> {
    /// the name of the operation
    pub name: String,
    /// the value of the operation id of this machine which activates this operation
    pub id: T,
    /// the parameters
    pub params: Params,
}
