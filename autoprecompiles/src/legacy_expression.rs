//! In this module, we instantiate `powdr_expression::AlgebraicExpression` using a
//! custom `AlgebraicReference` type. This makes the type very similar to the
//! `powdr_ast::analyzed::AlgebraicExpression`, which we've used historically.
//! Going forward, we will simplify the code and remove this module eventually.
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};

pub type AlgebraicExpression<T> = powdr_expression::AlgebraicExpression<T, AlgebraicReference>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PolynomialType {
    Committed,
    Constant,
    Intermediate,
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize)]
pub struct PolyID {
    pub id: u64,
    pub ptype: PolynomialType,
}

impl Hash for PolyID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // single call to hash is faster
        ((self.id << 2) + self.ptype as u64).hash(state);
    }
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct AlgebraicReference {
    /// Name of the polynomial - just for informational purposes.
    /// Comparisons are based on polynomial ID.
    /// In case of an array element, this ends in `[i]`.
    pub name: String,
    /// Identifier for a polynomial reference, already contains
    /// the element offset in case of an array element.
    pub poly_id: PolyID,
}

impl std::fmt::Display for AlgebraicReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialOrd for AlgebraicReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AlgebraicReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.poly_id.cmp(&other.poly_id)
    }
}

impl PartialEq for AlgebraicReference {
    fn eq(&self, other: &Self) -> bool {
        self.poly_id == other.poly_id
    }
}

impl Hash for AlgebraicReference {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.poly_id.hash(state);
    }
}

/// Tries to convert a `powdr_expression::AlgebraicExpression<T, R>` into a
/// `powdr_expression::AlgebraicExpression<T, AlgebraicReference>`.
pub fn try_convert<T, R: TryInto<AlgebraicReference>>(
    expr: powdr_expression::AlgebraicExpression<T, R>,
) -> Result<AlgebraicExpression<T>, R::Error> {
    match expr {
        powdr_expression::AlgebraicExpression::Reference(reference) => Ok(
            powdr_expression::AlgebraicExpression::Reference(reference.try_into()?),
        ),
        powdr_expression::AlgebraicExpression::Number(n) => {
            Ok(powdr_expression::AlgebraicExpression::Number(n))
        }
        powdr_expression::AlgebraicExpression::BinaryOperation(binary) => {
            Ok(powdr_expression::AlgebraicExpression::BinaryOperation(
                powdr_expression::AlgebraicBinaryOperation {
                    left: Box::new(try_convert(*binary.left)?),
                    op: binary.op,
                    right: Box::new(try_convert(*binary.right)?),
                },
            ))
        }
        powdr_expression::AlgebraicExpression::UnaryOperation(unary) => {
            Ok(powdr_expression::AlgebraicExpression::UnaryOperation(
                powdr_expression::AlgebraicUnaryOperation {
                    op: unary.op,
                    expr: Box::new(try_convert(*unary.expr)?),
                },
            ))
        }
    }
}
