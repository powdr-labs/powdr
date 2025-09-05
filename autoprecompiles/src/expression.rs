//! In this module, we instantiate `powdr_expression::AlgebraicExpression` using a
//! custom `AlgebraicReference` type.
use serde::{Deserialize, Serialize};
use core::fmt;
use std::{hash::Hash, sync::Arc};

pub type AlgebraicExpression<T> = powdr_expression::AlgebraicExpression<T, AlgebraicReference>;


#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Hash, Eq, Serialize, Deserialize)]
pub enum AlgebraicReference {
    IsValid,
    Original(AlgebraicReferenceOriginal),
}

impl AlgebraicReference {
    /// Create an algebraic reference from an id and name.
    /// This is used to create the original airs
    /// The namespace is set to 0 and will be modified when processing a block
    /// This is a bit hacky, but avoids introducing generics to differenciate the original airs
    /// and the apc air. We basically represent the original airs as apcs which have no `is_valid` and all references share namespace 0.
    pub fn new(id: u64, name: Arc<String>) -> Self {
        AlgebraicReference::Original(AlgebraicReferenceOriginal {
            id: NamespacedReferenceIdentifier {
                namespace: 0,
                id: id,
            },
            name,
        })
    }
}

impl fmt::Display for AlgebraicReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AlgebraicReference::IsValid => write!(f, "is_valid"),
            AlgebraicReference::Original(original) => write!(f, "{original}"),
        }
    }
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct AlgebraicReferenceOriginal {
    /// Name of the polynomial - just for informational purposes.
    /// Comparisons are based on the ID.
    pub name: Arc<String>,
    /// Identifier for a reference.
    pub id: NamespacedReferenceIdentifier,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct NamespacedReferenceIdentifier {
    /// The namespace of the reference. This is the index of the original instruction in the basic block
    pub namespace: usize,
    /// The identifier of the reference within its namespace.
    pub id: u64,
}

impl std::fmt::Display for AlgebraicReferenceOriginal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", self.name, self.id.namespace)
    }
}

impl PartialOrd for AlgebraicReferenceOriginal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AlgebraicReferenceOriginal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}


impl PartialEq for AlgebraicReferenceOriginal {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for AlgebraicReferenceOriginal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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
