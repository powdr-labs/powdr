use std::collections::BTreeMap;

use powdr_number::BigUint;

use crate::parsed::{
    asm::{AbsoluteSymbolPath, CallableParams, OperationParams},
    Expression, PilStatement, TypedExpression,
};

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

#[derive(Clone)]
pub struct PILGraph {
    pub main: Machine,
    pub entry_points: Vec<Operation>,
    pub objects: BTreeMap<Location, Object>,
    pub definitions: BTreeMap<AbsoluteSymbolPath, TypedExpression>,
}

#[derive(Default, Clone)]
pub struct Object {
    pub degree: Option<u64>,
    /// the pil identities for this machine
    pub pil: Vec<PilStatement>,
    /// the links from this machine to its children
    pub links: Vec<Link>,
}

impl Object {
    pub fn with_degree(mut self, degree: Option<u64>) -> Self {
        self.degree = degree;
        self
    }
}

#[derive(Clone)]
/// A link between two machines
pub struct Link {
    /// the link source, i.e. a flag and some arguments
    pub from: LinkFrom,
    /// the link target, i.e. a callable in some machine
    pub to: LinkTo,
}

#[derive(Clone)]
pub struct LinkFrom {
    pub flag: Expression,
    pub params: CallableParams,
}

#[derive(Clone)]
pub struct LinkTo {
    /// the machine we link to
    pub machine: Machine,
    /// the operation we link to
    pub operation: Operation,
}

#[derive(Clone)]
pub struct Machine {
    /// the location of this instance
    pub location: Location,
    /// its latch
    pub latch: Option<String>,
    /// its operation id
    pub operation_id: Option<String>,
}

#[derive(Clone)]
pub struct Operation {
    /// the name of the operation
    pub name: String,
    /// the value of the operation id of this machine which activates this operation
    pub id: Option<BigUint>,
    /// the parameters
    pub params: OperationParams,
}
