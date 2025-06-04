use powdr_expression::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator,
    AlgebraicExpression as ActualAlgebraicExpression, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};

pub type AlgebraicExpression<T> = ActualAlgebraicExpression<T, AlgebraicReference>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PolynomialType {
    Committed = 0,
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
    pub next: bool,
}

impl AlgebraicReference {
    #[inline]
    pub fn is_witness(&self) -> bool {
        self.poly_id.ptype == PolynomialType::Committed
    }
    #[inline]
    pub fn is_fixed(&self) -> bool {
        self.poly_id.ptype == PolynomialType::Constant
    }
}

impl std::fmt::Display for AlgebraicReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.name, if self.next { "'" } else { "" },)
    }
}

impl PartialOrd for AlgebraicReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AlgebraicReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.poly_id, &self.next).cmp(&(&other.poly_id, &other.next))
    }
}

impl PartialEq for AlgebraicReference {
    fn eq(&self, other: &Self) -> bool {
        self.poly_id == other.poly_id && self.next == other.next
    }
}

impl Hash for AlgebraicReference {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.poly_id.hash(state);
        self.next.hash(state);
    }
}

impl From<AlgebraicReference> for powdr_ast::analyzed::AlgebraicReference {
    fn from(reference: AlgebraicReference) -> Self {
        powdr_ast::analyzed::AlgebraicReference {
            name: reference.name,
            poly_id: powdr_ast::analyzed::PolyID {
                id: reference.poly_id.id,
                ptype: match reference.poly_id.ptype {
                    PolynomialType::Committed => powdr_ast::analyzed::PolynomialType::Committed,
                    PolynomialType::Constant => powdr_ast::analyzed::PolynomialType::Constant,
                    PolynomialType::Intermediate => {
                        powdr_ast::analyzed::PolynomialType::Intermediate
                    }
                },
            },
            next: reference.next,
        }
    }
}

impl From<powdr_ast::analyzed::AlgebraicReference> for AlgebraicReference {
    fn from(reference: powdr_ast::analyzed::AlgebraicReference) -> Self {
        AlgebraicReference {
            name: reference.name,
            poly_id: PolyID {
                id: reference.poly_id.id,
                ptype: match reference.poly_id.ptype {
                    powdr_ast::analyzed::PolynomialType::Committed => PolynomialType::Committed,
                    powdr_ast::analyzed::PolynomialType::Constant => PolynomialType::Constant,
                    powdr_ast::analyzed::PolynomialType::Intermediate => {
                        PolynomialType::Intermediate
                    }
                },
            },
            next: reference.next,
        }
    }
}

pub trait CompatibleWithAstExpression<T>: Sized {
    /// Converts the expression into an AST expression.
    fn into_ast_expression(self) -> powdr_ast::analyzed::AlgebraicExpression<T>;

    /// Attempts to convert an AST expression into this expression type.
    /// Fails if there is a public reference, a reference to a challenge, or an exponentiation.
    fn try_from_ast_expression(
        expr: powdr_ast::analyzed::AlgebraicExpression<T>,
    ) -> Result<Self, ()>;
}

impl<T> CompatibleWithAstExpression<T> for AlgebraicExpression<T> {
    fn into_ast_expression(self) -> powdr_ast::analyzed::AlgebraicExpression<T> {
        match self {
            AlgebraicExpression::Number(n) => powdr_ast::analyzed::AlgebraicExpression::Number(n),
            AlgebraicExpression::Reference(reference) => {
                powdr_ast::analyzed::AlgebraicExpression::Reference(reference.into())
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let left = left.into_ast_expression();
                let right = right.into_ast_expression();
                let op = match op {
                    AlgebraicBinaryOperator::Add => {
                        powdr_ast::analyzed::AlgebraicBinaryOperator::Add
                    }
                    AlgebraicBinaryOperator::Sub => {
                        powdr_ast::analyzed::AlgebraicBinaryOperator::Sub
                    }
                    AlgebraicBinaryOperator::Mul => {
                        powdr_ast::analyzed::AlgebraicBinaryOperator::Mul
                    }
                };
                powdr_ast::analyzed::AlgebraicExpression::BinaryOperation(
                    powdr_ast::analyzed::AlgebraicBinaryOperation {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                )
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
                powdr_ast::analyzed::AlgebraicExpression::UnaryOperation(
                    powdr_ast::analyzed::AlgebraicUnaryOperation {
                        op: match op {
                            AlgebraicUnaryOperator::Minus => {
                                powdr_ast::analyzed::AlgebraicUnaryOperator::Minus
                            }
                        },
                        expr: Box::new(expr.into_ast_expression()),
                    },
                )
            }
        }
    }

    fn try_from_ast_expression(
        expr: powdr_ast::analyzed::AlgebraicExpression<T>,
    ) -> Result<Self, ()> {
        match expr {
            powdr_ast::analyzed::AlgebraicExpression::Number(n) => {
                Ok(AlgebraicExpression::Number(n))
            }
            powdr_ast::analyzed::AlgebraicExpression::Reference(reference) => {
                Ok(AlgebraicExpression::Reference(reference.into()))
            }
            powdr_ast::analyzed::AlgebraicExpression::BinaryOperation(
                powdr_ast::analyzed::AlgebraicBinaryOperation { left, op, right },
            ) => {
                let left = AlgebraicExpression::try_from_ast_expression(*left)?;
                let right = AlgebraicExpression::try_from_ast_expression(*right)?;
                let op = match op {
                    powdr_ast::analyzed::AlgebraicBinaryOperator::Add => {
                        AlgebraicBinaryOperator::Add
                    }
                    powdr_ast::analyzed::AlgebraicBinaryOperator::Sub => {
                        AlgebraicBinaryOperator::Sub
                    }
                    powdr_ast::analyzed::AlgebraicBinaryOperator::Mul => {
                        AlgebraicBinaryOperator::Mul
                    }
                    // Can't be represented, return an error.
                    powdr_ast::analyzed::AlgebraicBinaryOperator::Pow => return Err(()),
                };
                Ok(AlgebraicExpression::new_binary(left, op, right))
            }
            powdr_ast::analyzed::AlgebraicExpression::UnaryOperation(
                powdr_ast::analyzed::AlgebraicUnaryOperation { op, expr },
            ) => {
                let expr = AlgebraicExpression::try_from_ast_expression(*expr)?;
                let op = match op {
                    powdr_ast::analyzed::AlgebraicUnaryOperator::Minus => {
                        AlgebraicUnaryOperator::Minus
                    }
                };
                Ok(AlgebraicExpression::new_unary(op, expr))
            }
            // Can't be represented, return an error.
            powdr_ast::analyzed::AlgebraicExpression::PublicReference(_)
            | powdr_ast::analyzed::AlgebraicExpression::Challenge(_) => Err(()),
        }
    }
}
