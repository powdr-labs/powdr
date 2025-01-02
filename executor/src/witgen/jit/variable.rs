use std::{
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
};

use powdr_ast::analyzed::{AlgebraicReference, PolyID, PolynomialType};
use powdr_number::FieldElement;

use super::effect::MachineCallArgument;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
/// A variable that can be used in the inference engine.
pub enum Variable {
    /// A witness cell in the current block machine.
    Cell(Cell),
    /// A parameter (input or output) of the machine.
    #[allow(dead_code)]
    Param(usize),
    /// The return value of a machine call on a certain
    /// identity on a certain row offset.
    MachineCallReturnValue(MachineCallReturnVariable),
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Cell(cell) => write!(f, "{cell}"),
            Variable::Param(i) => write!(f, "params[{i}]"),
            Variable::MachineCallReturnValue(ret) => {
                write!(
                    f,
                    "ret({}, {}, {})",
                    ret.identity_id, ret.row_offset, ret.index
                )
            }
        }
    }
}

impl Variable {
    /// Create a variable from an algebraic reference.
    pub fn from_reference(r: &AlgebraicReference, row_offset: i32) -> Self {
        assert!(r.is_witness());
        Self::Cell(Cell {
            column_name: r.name.clone(),
            id: r.poly_id.id,
            row_offset: r.next as i32 + row_offset,
        })
    }

    /// If this variable corresponds to a witness cell, return the corresponding polynomial ID.
    pub fn try_to_witness_poly_id(&self) -> Option<powdr_ast::analyzed::PolyID> {
        match self {
            Variable::Cell(cell) => Some(PolyID {
                id: cell.id,
                ptype: PolynomialType::Committed,
            }),
            Variable::Param(_) | Variable::MachineCallReturnValue(_) => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct MachineCallReturnVariable {
    pub identity_id: u64,
    pub row_offset: i32,
    pub index: usize,
}

impl MachineCallReturnVariable {
    pub fn into_argument<T: FieldElement>(self) -> MachineCallArgument<T, Variable> {
        MachineCallArgument::Unknown(Variable::MachineCallReturnValue(self))
    }
}

/// The identifier of a witness cell in the trace table.
/// The `row_offset` is relative to a certain "zero row" defined
/// by the component that uses this data structure.
#[derive(Debug, Clone, Eq)]
pub struct Cell {
    /// Name of the column, used only for display purposes.
    pub column_name: String,
    pub id: u64,
    pub row_offset: i32,
}

impl Hash for Cell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.row_offset.hash(state);
    }
}

impl PartialEq for Cell {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.row_offset == other.row_offset
    }
}

impl Ord for Cell {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.id, self.row_offset).cmp(&(other.id, other.row_offset))
    }
}

impl PartialOrd for Cell {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.column_name, self.row_offset)
    }
}
