use std::fmt::{self, Debug};

use powdr_number::FieldElement;

use super::{affine_expression::AlgebraicVariable, range_constraints::RangeConstraint};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IncompleteCause<K = usize> {
    /// In a VM, the latch value could not be figured out after a row was processed.
    UnknownLatch,
    /// Some parts of an expression are not bit constrained. Example: `x + y == 0x3` with `x | 0x1`. Arguments: the indices of the unconstrained variables.
    BitUnconstrained(Vec<K>),
    /// Some bit constraints are overlapping. Example: `x + y == 0x3` with `x | 0x3` and `y | 0x3`
    OverlappingBitConstraints,
    /// There are bit constraints, but they might over-flow the field.
    /// Example: `some_field_element == 2**64 * x` with `x | 0x3` and a 64-bit field.
    OverflowingBitConstraints,
    /// Multiple rows match a lookup query. Example: `{x, 1} in [{1, 1}, {2, 1}]`
    MultipleLookupMatches,
    /// A linear constraint does not have a unique solution. Example: `x + y == 0`
    MultipleLinearSolutions,
    /// No progress transferring. TODO: not sure this could not be covered by other cases
    NoProgressTransferring,
    /// Quadratic term found trying to detect an affine expression. Example: `a*b + 2c + d`
    QuadraticTerm,
    /// Division term found trying to detect an affine expression. Example: `a/b + 2c + d`
    DivisionTerm,
    /// Exponentiation term found trying to detect an affine expression. Example: `a**b + 2c + d`
    ExponentiationTerm,
    /// No query answer. Example: we ask the prover for `a` and receive no value. Arguments: the query and the column name
    NoQueryAnswer(String, String),
    /// Query match scrutinee is not constant, so the query fails. Example: evaluate `match x { 1 => 1, _ => 0}` but `x` is not constant.
    NonConstantQueryMatchScrutinee,
    /// Query element is not constant.
    NonConstantQueryElement,
    /// Bus ID is not constant.
    NonConstantBusID,
    /// A required argument was not provided
    NonConstantRequiredArgument(&'static str),
    /// The left selector in a lookup is not constant. Example: `x * {1} in [{1}]` where `x` is not constant.
    NonConstantLeftSelector,
    /// A lookup into a block machine was not able to assign all variables in the query. It could be that we just need to re-run it.
    BlockMachineLookupIncomplete,
    /// We could not (yet) read some data
    DataNotYetAvailable,
    /// Last resort error when all possible solving approaches have failed. TODO: make this more precise or use another variant
    SolvingFailed,
    /// We tried to symbolically evaluate a challenge, which is not supported.
    SymbolicEvaluationOfChallenge,
    /// Some knowledge was learnt, but not a concrete value. Example: `Y = X` if we know that `Y` is boolean. We learn that `X` is boolean, but not its exact value.
    NotConcrete,
    Multiple(Vec<IncompleteCause<K>>),
}

impl<K> IncompleteCause<K> {
    pub fn combine(self, right: IncompleteCause<K>) -> IncompleteCause<K> {
        match (self, right) {
            (IncompleteCause::Multiple(mut l), IncompleteCause::Multiple(r)) => {
                if l.is_empty() {
                    IncompleteCause::Multiple(r)
                } else {
                    l.extend(r);
                    IncompleteCause::Multiple(l)
                }
            }
            (IncompleteCause::Multiple(mut causes), other)
            | (other, IncompleteCause::Multiple(mut causes)) => {
                causes.push(other);
                IncompleteCause::Multiple(causes)
            }
            (l, r) => IncompleteCause::Multiple(vec![l, r]),
        }
    }
}

pub type Constraints<K, T> = Vec<(K, Constraint<T>)>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalStatus<K = usize> {
    Complete,
    Incomplete(IncompleteCause<K>),
}

impl<K> From<IncompleteCause<K>> for EvalStatus<K> {
    fn from(value: IncompleteCause<K>) -> Self {
        Self::Incomplete(value)
    }
}

impl<K> EvalStatus<K> {
    pub fn combine<C: Into<EvalStatus<K>>>(self, other: C) -> Self {
        use self::EvalStatus::*;
        let other = other.into();
        match (self, other) {
            (Complete, Complete) => Complete,
            (Incomplete(left), Incomplete(right)) => Incomplete(left.combine(right)),
            (Complete, Incomplete(i)) | (Incomplete(i), Complete) => Incomplete(i),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// The result of solving a constraint (polynomial identity, lookup, or permutation).
pub struct EvalValue<K, T: FieldElement> {
    /// Assignments and range constraint updates resulting from the solving.
    pub constraints: Constraints<K, T>,
    /// The status of the solving. If complete, all variables are known after applying the constraints.
    pub status: EvalStatus<K>,
    /// Whether the solving had side effects. For example, a block might be added to another machine.
    pub side_effect: bool,
}

impl<K, T: FieldElement> EvalValue<K, T> {
    pub fn is_complete(&self) -> bool {
        match self.status {
            EvalStatus::Complete => true,
            EvalStatus::Incomplete(_) => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    pub fn incomplete(cause: IncompleteCause<K>) -> Self {
        Self::new(vec![], EvalStatus::Incomplete(cause))
    }

    pub fn incomplete_with_constraints(
        constraints: Vec<(K, Constraint<T>)>,
        cause: IncompleteCause<K>,
    ) -> Self {
        Self::new(constraints, EvalStatus::Incomplete(cause))
    }

    pub fn complete(constraints: Vec<(K, Constraint<T>)>) -> Self {
        Self::new(constraints, EvalStatus::Complete)
    }

    fn new(constraints: Vec<(K, Constraint<T>)>, status: EvalStatus<K>) -> Self {
        Self {
            constraints,
            side_effect: false,
            status,
        }
    }

    pub fn combine(&mut self, other: Self) {
        // reserve more space?
        self.constraints.extend(other.constraints);
        self.status =
            std::mem::replace(&mut self.status, EvalStatus::Complete).combine(other.status);
        self.side_effect |= other.side_effect;
    }

    pub fn report_side_effect(mut self) -> Self {
        self.side_effect = true;
        self
    }
}

/// Result of evaluating an expression / lookup.
/// New assignments or constraints for witness columns identified by an ID.
pub type EvalResult<'a, T, K = AlgebraicVariable<'a>> = Result<EvalValue<K, T>, EvalError<T>>;

/// A fatal error for witness generation.
#[derive(Clone, PartialEq)]
pub enum EvalError<T: FieldElement> {
    /// We ran out of rows
    RowsExhausted(String),
    /// A constraint that cannot be satisfied (i.e. 2 = 1).
    ConstraintUnsatisfiable(String),
    /// Conflicting bit- or range constraints in an equation, i.e. for X = 0x100, where X is known to be at most 0xff.
    ConflictingRangeConstraints,
    /// A division pattern was recognized but the solution does not satisfy the range constraints.
    InvalidDivision,
    /// Fixed lookup failed
    FixedLookupFailed(Vec<(String, T)>),
    /// Error getting information from the prover.
    ProverQueryError(String),
    /// Machines depend on each other recursively.
    RecursiveMachineCalls(String),
    Generic(String),
    Multiple(Vec<EvalError<T>>),
}

impl<T: FieldElement> Debug for EvalError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl<T: FieldElement> From<String> for EvalError<T> {
    fn from(value: String) -> Self {
        Self::Generic(value)
    }
}

impl<T: FieldElement> EvalError<T> {
    pub fn combine(self, other: EvalError<T>) -> EvalError<T> {
        match (self, other) {
            (EvalError::Multiple(l), EvalError::Multiple(r)) => {
                EvalError::Multiple(l.into_iter().chain(r).collect())
            }
            (m @ EvalError::Multiple(_), other) | (other, m @ EvalError::Multiple(_)) => {
                m.combine(EvalError::Multiple(vec![other]))
            }
            (l, r) => EvalError::Multiple(vec![l, r]),
        }
    }
}

impl<T: FieldElement> fmt::Display for EvalError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::ConstraintUnsatisfiable(e) => {
                write!(f, "Linear constraint is not satisfiable: {e} != 0",)
            }
            EvalError::Multiple(errors) => {
                for e in errors {
                    write!(f, "{e}")?;
                }
                write!(f, "")
            }
            EvalError::ConflictingRangeConstraints => {
                write!(f, "Range constraints in the expression are conflicting or do not match the constant / offset.",)
            }
            EvalError::InvalidDivision => {
                write!(f, "A division pattern was recognized but the range constraints are conflicting with the solution.",)
            }
            EvalError::RowsExhausted(machine_name) => {
                write!(f, "Table rows exhausted for machine {machine_name}")
            }
            EvalError::FixedLookupFailed(input_assignment) => {
                let query = input_assignment
                    .iter()
                    .map(|(poly_name, v)| format!("{poly_name} = {v}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "Lookup into fixed columns failed: no match for query: {query}"
                )
            }
            EvalError::ProverQueryError(s) => {
                write!(f, "Error getting external information from the prover: {s}")
            }
            EvalError::RecursiveMachineCalls(err) => {
                write!(f, "Recursive machine dependency: {err}")
            }
            EvalError::Generic(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constraint<T: FieldElement> {
    Assignment(T),
    RangeConstraint(RangeConstraint<T>),
}

impl<T: FieldElement> fmt::Display for Constraint<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Assignment(a) => write!(f, " = {a}"),
            Constraint::RangeConstraint(bc) => write!(f, " : {bc}"),
        }
    }
}
