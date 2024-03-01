pub mod asm;
pub mod build;
pub mod display;
pub mod folder;
pub mod utils;
pub mod visitor;

use std::{
    collections::BTreeSet,
    iter::{empty, once},
    ops,
};

use powdr_number::{DegreeType, FieldElement};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use self::asm::{Part, SymbolPath};
use crate::SourceRef;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PILFile<T>(pub Vec<PilStatement<T>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PilStatement<T> {
    /// File name
    Include(SourceRef, String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(SourceRef, SymbolPath, Expression<T>),
    LetStatement(
        SourceRef,
        String,
        Option<TypeScheme<Expression<T>>>,
        Option<Expression<T>>,
    ),
    PolynomialDefinition(SourceRef, String, Expression<T>),
    PublicDeclaration(
        SourceRef,
        /// The name of the public value.
        String,
        /// The polynomial/column that contains the public value.
        NamespacedPolynomialReference,
        /// If the polynomial is an array, this is the array element index.
        Option<Expression<T>>,
        /// The row number of the public value.
        Expression<T>,
    ),
    PolynomialConstantDeclaration(SourceRef, Vec<PolynomialName<T>>),
    PolynomialConstantDefinition(SourceRef, String, FunctionDefinition<T>),
    PolynomialCommitDeclaration(
        SourceRef,
        Vec<PolynomialName<T>>,
        Option<FunctionDefinition<T>>,
    ),
    PlookupIdentity(
        SourceRef,
        SelectedExpressions<Expression<T>>,
        SelectedExpressions<Expression<T>>,
    ),
    PermutationIdentity(
        SourceRef,
        SelectedExpressions<Expression<T>>,
        SelectedExpressions<Expression<T>>,
    ),
    ConnectIdentity(SourceRef, Vec<Expression<T>>, Vec<Expression<T>>),
    ConstantDefinition(SourceRef, String, Expression<T>),
    Expression(SourceRef, Expression<T>),
}

impl<T> PilStatement<T> {
    /// If the statement is a symbol definition, returns all (local) names of defined symbols.
    pub fn symbol_definition_names(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        match self {
            PilStatement::PolynomialDefinition(_, name, _)
            | PilStatement::PolynomialConstantDefinition(_, name, _)
            | PilStatement::ConstantDefinition(_, name, _)
            | PilStatement::PublicDeclaration(_, name, _, _, _)
            | PilStatement::LetStatement(_, name, _, _) => Box::new(once(name)),
            PilStatement::PolynomialConstantDeclaration(_, polynomials)
            | PilStatement::PolynomialCommitDeclaration(_, polynomials, _) => {
                Box::new(polynomials.iter().map(|p| &p.name))
            }

            PilStatement::Include(_, _)
            | PilStatement::Namespace(_, _, _)
            | PilStatement::PlookupIdentity(_, _, _)
            | PilStatement::PermutationIdentity(_, _, _)
            | PilStatement::ConnectIdentity(_, _, _)
            | PilStatement::Expression(_, _) => Box::new(empty()),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this statement.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &Expression<T>> + '_> {
        match self {
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => {
                Box::new(left.expressions().chain(right.expressions()))
            }
            PilStatement::ConnectIdentity(_start, left, right) => {
                Box::new(left.iter().chain(right.iter()))
            }
            PilStatement::Expression(_, e)
            | PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::ConstantDefinition(_, _, e) => Box::new(once(e)),

            PilStatement::LetStatement(_, _, type_scheme, value) => Box::new(
                type_scheme
                    .iter()
                    .flat_map(|t| t.type_name.expressions())
                    .chain(value),
            ),

            PilStatement::PublicDeclaration(_, _, _, i, e) => Box::new(i.iter().chain(once(e))),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => fundef.expressions(),
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _) => Box::new(empty()),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this statement.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<T>> + '_> {
        match self {
            PilStatement::PlookupIdentity(_, left, right)
            | PilStatement::PermutationIdentity(_, left, right) => {
                Box::new(left.expressions_mut().chain(right.expressions_mut()))
            }
            PilStatement::ConnectIdentity(_start, left, right) => {
                Box::new(left.iter_mut().chain(right.iter_mut()))
            }
            PilStatement::Expression(_, e)
            | PilStatement::Namespace(_, _, e)
            | PilStatement::PolynomialDefinition(_, _, e)
            | PilStatement::ConstantDefinition(_, _, e) => Box::new(once(e)),

            PilStatement::LetStatement(_, _, type_name, value) => Box::new(
                type_name
                    .iter_mut()
                    .flat_map(|t| t.type_name.expressions_mut())
                    .chain(value),
            ),

            PilStatement::PublicDeclaration(_, _, _, i, e) => Box::new(i.iter_mut().chain(once(e))),

            PilStatement::PolynomialConstantDefinition(_, _, fundef)
            | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => {
                fundef.expressions_mut()
            }
            PilStatement::PolynomialCommitDeclaration(_, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::PolynomialConstantDeclaration(_, _) => Box::new(empty()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SelectedExpressions<Expr> {
    pub selector: Option<Expr>,
    pub expressions: Vec<Expr>,
}

impl<Expr> Default for SelectedExpressions<Expr> {
    fn default() -> Self {
        Self {
            selector: Default::default(),
            expressions: Default::default(),
        }
    }
}

impl<Expr> SelectedExpressions<Expr> {
    /// Returns an iterator over all (top-level) expressions in this SelectedExpressions.
    pub fn expressions(&self) -> impl Iterator<Item = &Expr> {
        self.selector.iter().chain(self.expressions.iter())
    }

    /// Returns an iterator over all (top-level) expressions in this SelectedExpressions.
    pub fn expressions_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        self.selector.iter_mut().chain(self.expressions.iter_mut())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum Expression<T, Ref = NamespacedPolynomialReference> {
    Reference(Ref),
    PublicReference(String),
    // A number literal and its type. The type is always elementary, so we use NoArrayLengths for the generic param.
    Number(T, Option<TypeName<NoArrayLengths>>),
    String(String),
    Tuple(Vec<Expression<T, Ref>>),
    LambdaExpression(LambdaExpression<T, Ref>),
    ArrayLiteral(ArrayLiteral<T, Ref>),
    BinaryOperation(
        Box<Expression<T, Ref>>,
        BinaryOperator,
        Box<Expression<T, Ref>>,
    ),
    UnaryOperation(UnaryOperator, Box<Expression<T, Ref>>),
    IndexAccess(IndexAccess<T, Ref>),
    FunctionCall(FunctionCall<T, Ref>),
    FreeInput(Box<Expression<T, Ref>>),
    MatchExpression(Box<Expression<T, Ref>>, Vec<MatchArm<T, Ref>>),
    IfExpression(IfExpression<T, Ref>),
}

impl<T, Ref> Expression<T, Ref> {
    pub fn new_binary(left: Self, op: BinaryOperator, right: Self) -> Self {
        Expression::BinaryOperation(Box::new(left), op, Box::new(right))
    }

    /// Visits this expression and all of its sub-expressions and returns true
    /// if `f` returns true on any of them.
    pub fn any(&self, mut f: impl FnMut(&Self) -> bool) -> bool {
        use std::ops::ControlFlow;
        use visitor::ExpressionVisitable;
        self.pre_visit_expressions_return(&mut |e| {
            if f(e) {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        })
        .is_break()
    }
}

impl<T, Ref> ops::Add for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Add, rhs)
    }
}

impl<T, Ref> ops::Sub for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Sub, rhs)
    }
}
impl<T, Ref> ops::Mul for Expression<T, Ref> {
    type Output = Expression<T, Ref>;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Mul, rhs)
    }
}

impl<T: FieldElement, Ref> std::iter::Sum for Expression<T, Ref> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|a, b| a + b)
            .unwrap_or_else(|| T::zero().into())
    }
}

impl<T: FieldElement, Ref> From<T> for Expression<T, Ref> {
    fn from(value: T) -> Self {
        Expression::Number(value, None)
    }
}

impl<T> From<NamespacedPolynomialReference> for Expression<T> {
    fn from(value: NamespacedPolynomialReference) -> Self {
        Self::Reference(value)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialName<T> {
    pub name: String,
    pub array_size: Option<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial with an optional namespace
/// This is different from SymbolPath mainly due to different formatting.
pub struct NamespacedPolynomialReference {
    pub path: SymbolPath,
}

impl From<SymbolPath> for NamespacedPolynomialReference {
    fn from(value: SymbolPath) -> Self {
        Self { path: value }
    }
}

impl NamespacedPolynomialReference {
    pub fn from_identifier(name: String) -> Self {
        SymbolPath::from_parts(vec![Part::Named(name)]).into()
    }

    pub fn try_to_identifier(&self) -> Option<&String> {
        self.path.try_to_identifier()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
pub struct LambdaExpression<T, Ref = NamespacedPolynomialReference> {
    pub params: Vec<String>,
    pub body: Box<Expression<T, Ref>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
pub struct ArrayLiteral<T, Ref = NamespacedPolynomialReference> {
    pub items: Vec<Expression<T, Ref>>,
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, JsonSchema,
)]
pub enum UnaryOperator {
    Minus,
    LogicalNot,
    Next,
}

impl UnaryOperator {
    /// Returns true if the operator is a prefix-operator and false if it is a postfix operator.
    pub fn is_prefix(&self) -> bool {
        match self {
            UnaryOperator::Minus | UnaryOperator::LogicalNot => true,
            UnaryOperator::Next => false,
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, JsonSchema,
)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BinaryAnd,
    BinaryXor,
    BinaryOr,
    ShiftLeft,
    ShiftRight,
    LogicalOr,
    LogicalAnd,
    Less,
    LessEqual,
    Equal,
    Identity,
    NotEqual,
    GreaterEqual,
    Greater,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct IndexAccess<T, Ref = NamespacedPolynomialReference> {
    pub array: Box<Expression<T, Ref>>,
    pub index: Box<Expression<T, Ref>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FunctionCall<T, Ref = NamespacedPolynomialReference> {
    pub function: Box<Expression<T, Ref>>,
    pub arguments: Vec<Expression<T, Ref>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MatchArm<T, Ref = NamespacedPolynomialReference> {
    pub pattern: MatchPattern<T, Ref>,
    pub value: Expression<T, Ref>,
}

/// A pattern for a match arm. We could extend this in the future.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum MatchPattern<T, Ref = NamespacedPolynomialReference> {
    CatchAll,
    Pattern(Expression<T, Ref>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct IfExpression<T, Ref = NamespacedPolynomialReference> {
    pub condition: Box<Expression<T, Ref>>,
    pub body: Box<Expression<T, Ref>>,
    pub else_body: Box<Expression<T, Ref>>,
}

/// The definition of a function (excluding its name):
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionDefinition<T> {
    /// Array expression.
    Array(ArrayExpression<T>),
    /// Prover query. The Expression usually is a LambdaExpression.
    Query(Expression<T>),
    /// Generic expression
    Expression(Expression<T>),
}

impl<T> FunctionDefinition<T> {
    /// Returns an iterator over all (top-level) expressions.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &Expression<T>> + '_> {
        match self {
            FunctionDefinition::Array(ae) => ae.expressions(),
            FunctionDefinition::Query(e) | FunctionDefinition::Expression(e) => Box::new(once(e)),
        }
    }

    /// Returns an iterator over all (top-level) expressions.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<T>> + '_> {
        match self {
            FunctionDefinition::Array(ae) => ae.expressions_mut(),
            FunctionDefinition::Query(e) | FunctionDefinition::Expression(e) => Box::new(once(e)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ArrayExpression<T> {
    Value(Vec<Expression<T>>),
    RepeatedValue(Vec<Expression<T>>),
    Concat(Box<ArrayExpression<T>>, Box<ArrayExpression<T>>),
}

impl<T: FieldElement> ArrayExpression<T> {
    pub fn value(v: Vec<Expression<T>>) -> Self {
        Self::Value(v)
    }

    pub fn repeated_value(v: Vec<Expression<T>>) -> Self {
        Self::RepeatedValue(v)
    }

    pub fn concat(self, other: Self) -> Self {
        Self::Concat(Box::new(self), Box::new(other))
    }

    fn pad_with(self, pad: Expression<T>) -> Self {
        Self::concat(self, Self::repeated_value(vec![pad]))
    }

    pub fn pad_with_zeroes(self) -> Self {
        self.pad_with(Expression::Number(0.into(), None))
    }

    fn last(&self) -> Option<&Expression<T>> {
        match self {
            ArrayExpression::Value(v) => v.last(),
            ArrayExpression::RepeatedValue(v) => v.last(),
            ArrayExpression::Concat(_, right) => right.last(),
        }
    }

    // return None if `self` is empty
    pub fn pad_with_last(self) -> Option<Self> {
        self.last().cloned().map(|last| self.pad_with(last))
    }
}

impl<T> ArrayExpression<T> {
    /// solve for `*`
    pub fn solve(&self, degree: DegreeType) -> DegreeType {
        assert!(
            self.number_of_repetitions() <= 1,
            "`*` can be used only once in rhs of array definition"
        );
        let len = self.constant_length();
        assert!(
            len <= degree,
            "Array literal is too large ({len}) for degree ({degree})."
        );
        // Fill up the remaining space with the repeated array
        degree - len
    }

    /// Returns an iterator over all (top-level) expressions.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &Expression<T>> + '_> {
        match self {
            ArrayExpression::Value(v) | ArrayExpression::RepeatedValue(v) => Box::new(v.iter()),
            ArrayExpression::Concat(left, right) => {
                Box::new(left.expressions().chain(right.expressions()))
            }
        }
    }

    /// Returns all (top-level) expressions.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<T>> + '_> {
        match self {
            ArrayExpression::Value(v) | ArrayExpression::RepeatedValue(v) => Box::new(v.iter_mut()),
            ArrayExpression::Concat(left, right) => {
                Box::new(left.expressions_mut().chain(right.expressions_mut()))
            }
        }
    }

    /// The number of times the `*` operator is used
    fn number_of_repetitions(&self) -> usize {
        match self {
            ArrayExpression::RepeatedValue(_) => 1,
            ArrayExpression::Value(_) => 0,
            ArrayExpression::Concat(left, right) => {
                left.number_of_repetitions() + right.number_of_repetitions()
            }
        }
    }

    /// The combined length of the constant-size parts of the array expression.
    fn constant_length(&self) -> DegreeType {
        match self {
            ArrayExpression::RepeatedValue(_) => 0,
            ArrayExpression::Value(e) => e.len() as DegreeType,
            ArrayExpression::Concat(left, right) => {
                left.constant_length() + right.constant_length()
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum TypeName<E> {
    /// The bottom type `!`, which cannot have a value but is
    /// compatible with all other types.
    Bottom,
    /// Boolean
    Bool,
    /// Integer (arbitrary precision)
    Int,
    /// Field element (unspecified field)
    Fe,
    /// String
    String,
    /// Column
    Col,
    /// Algebraic expression
    Expr,
    /// Polynomial identity
    Constr,
    Array(ArrayTypeName<E>),
    Tuple(TupleTypeName<E>),
    Function(FunctionTypeName<E>),
    TypeVar(String),
}

// This type is used as TypeName<NoArrayLengths> if we only expect elementary types.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum NoArrayLengths {}

impl<E> TypeName<E> {
    /// Returns true if it is a non-complex type.
    /// Type variables are not considered elementary.
    pub fn is_elementary(&self) -> bool {
        match self {
            TypeName::Bottom
            | TypeName::Bool
            | TypeName::Int
            | TypeName::Fe
            | TypeName::String
            | TypeName::Col
            | TypeName::Expr
            | TypeName::Constr => true,
            TypeName::Array(_)
            | TypeName::Tuple(_)
            | TypeName::Function(_)
            | TypeName::TypeVar(_) => false,
        }
    }
    /// Returns true if the type name needs parentheses during formatting
    /// when used inside a complex expression.
    pub fn needs_parentheses(&self) -> bool {
        match self {
            _ if self.is_elementary() => false,
            TypeName::Array(_) | TypeName::Tuple(_) | TypeName::TypeVar(_) => false,
            TypeName::Function(_) => true,
            _ => unreachable!(),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        match self {
            _ if self.is_elementary() => Box::new(empty()),
            TypeName::TypeVar(_) => Box::new(empty()),
            TypeName::Array(a) => a.expressions(),
            TypeName::Tuple(t) => t.expressions(),
            TypeName::Function(f) => f.expressions(),
            _ => unreachable!(),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        match self {
            _ if self.is_elementary() => Box::new(empty()),
            TypeName::TypeVar(_) => Box::new(empty()),
            TypeName::Array(a) => a.expressions_mut(),
            TypeName::Tuple(t) => t.expressions_mut(),
            TypeName::Function(f) => f.expressions_mut(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ArrayTypeName<E> {
    pub base: Box<TypeName<E>>,
    pub length: Option<E>,
}

impl<E> ArrayTypeName<E> {
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(self.base.expressions().chain(self.length.iter()))
    }
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(self.base.expressions_mut().chain(self.length.iter_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TupleTypeName<E> {
    pub items: Vec<TypeName<E>>,
}

impl<E> TupleTypeName<E> {
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(self.items.iter().flat_map(|t| t.expressions()))
    }
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(self.items.iter_mut().flat_map(|t| t.expressions_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FunctionTypeName<E> {
    pub params: Vec<TypeName<E>>,
    pub value: Box<TypeName<E>>,
}

impl<E> FunctionTypeName<E> {
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(
            self.params
                .iter()
                .flat_map(|t| t.expressions())
                .chain(self.value.expressions()),
        )
    }
    /// Returns an iterator over all (top-level) expressions in this type name.
    pub fn expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(
            self.params
                .iter_mut()
                .flat_map(|t| t.expressions_mut())
                .chain(self.value.expressions_mut()),
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TypeScheme<E> {
    /// Type variables and their trait bounds.
    pub type_vars: TypeBounds,
    pub type_name: TypeName<E>,
}

impl<E> TypeScheme<E> {
    pub fn type_vars_to_string(&self) -> String {
        if self.type_vars.is_empty() {
            String::new()
        } else {
            format!("<{}>", self.type_vars)
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default, Serialize, Deserialize, JsonSchema,
)]
// TODO bounds should be SymbolPaths in the future.
pub struct TypeBounds(Vec<(String, BTreeSet<String>)>);

impl TypeBounds {
    pub fn new<J: Into<BTreeSet<String>>, I: Iterator<Item = (String, J)>>(vars: I) -> Self {
        Self(vars.map(|(n, x)| (n, x.into())).collect::<Vec<_>>())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.0.iter().map(|(n, _)| n)
    }

    pub fn bounds(&self) -> impl Iterator<Item = (&String, &BTreeSet<String>)> {
        self.0.iter().map(|(n, x)| (n, x))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ExpressionWithTypeScheme<T, Ref = NamespacedPolynomialReference> {
    pub e: Expression<T, Ref>,
    pub type_scheme: Option<TypeScheme<Expression<T, Ref>>>,
}
