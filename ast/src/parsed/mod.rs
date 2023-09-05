pub mod asm;
pub mod build;
pub mod display;
use std::{iter::once, ops::ControlFlow};

use number::{DegreeType, FieldElement};

use self::asm::FunctionCall;

#[derive(Debug, PartialEq, Eq)]
pub struct PILFile<T>(pub Vec<PilStatement<T>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PilStatement<T> {
    /// File name
    Include(usize, String),
    /// Name of namespace and polynomial degree (constant)
    Namespace(usize, String, Expression<T>),
    PolynomialDefinition(usize, String, Expression<T>),
    PublicDeclaration(
        usize,
        String,
        NamespacedPolynomialReference<T>,
        Expression<T>,
    ),
    PolynomialConstantDeclaration(usize, Vec<PolynomialName<T>>),
    PolynomialConstantDefinition(usize, String, FunctionDefinition<T>),
    PolynomialCommitDeclaration(usize, Vec<PolynomialName<T>>, Option<FunctionDefinition<T>>),
    PolynomialIdentity(usize, Expression<T>),
    PlookupIdentity(usize, SelectedExpressions<T>, SelectedExpressions<T>),
    PermutationIdentity(usize, SelectedExpressions<T>, SelectedExpressions<T>),
    ConnectIdentity(usize, Vec<Expression<T>>, Vec<Expression<T>>),
    ConstantDefinition(usize, String, Expression<T>),
    MacroDefinition(
        usize,
        String,
        Vec<String>,
        Vec<PilStatement<T>>,
        Option<Expression<T>>,
    ),
    FunctionCall(usize, String, Vec<Expression<T>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SelectedExpressions<T> {
    pub selector: Option<Expression<T>>,
    pub expressions: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression<T> {
    /// Reference to a constant, "%ConstantName"
    Constant(String),
    PolynomialReference(ShiftedPolynomialReference<T>),
    PublicReference(String),
    Number(T),
    String(String),
    Tuple(Vec<Expression<T>>),
    BinaryOperation(Box<Expression<T>>, BinaryOperator, Box<Expression<T>>),
    UnaryOperation(UnaryOperator, Box<Expression<T>>),
    FunctionCall(FunctionCall<T>),
    FreeInput(Box<Expression<T>>),
    MatchExpression(
        Box<Expression<T>>,
        Vec<(Option<Expression<T>>, Expression<T>)>,
    ),
}

impl<T> From<ShiftedPolynomialReference<T>> for Expression<T> {
    fn from(value: ShiftedPolynomialReference<T>) -> Self {
        Self::PolynomialReference(value)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialName<T> {
    pub name: String,
    pub array_size: Option<Expression<T>>,
}

/// A polynomial with an optional shift
#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
pub struct ShiftedPolynomialReference<T> {
    /// Whether we shift or not
    is_next: bool,
    /// The underlying polynomial
    pol: NamespacedPolynomialReference<T>,
}

impl<T> ShiftedPolynomialReference<T> {
    /// Returns the underlying namespaced polynomial
    pub fn into_namespaced(self) -> NamespacedPolynomialReference<T> {
        self.pol
    }

    /// Returns the shift of this polynomial
    pub fn shift(&self) -> bool {
        self.is_next
    }

    /// Returns the optional namespace of the underlying polynomial
    pub fn namespace(&self) -> &Option<String> {
        self.pol.namespace()
    }

    /// Returns the optional index of the underlying polynomial in its declaration array
    pub fn index(&self) -> &Option<Box<Expression<T>>> {
        self.pol.index()
    }

    /// Returns the name of the declared polynomial or array of polynomials
    pub fn name(&self) -> &str {
        self.pol.name()
    }

    /// Returns a mutable reference to the declared polynomial or array of polynomials
    pub fn name_mut(&mut self) -> &mut String {
        self.pol.name_mut()
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial with an optional namespace
pub struct NamespacedPolynomialReference<T> {
    /// The optional namespace, if `None` then this polynomial inherits the next enclosing namespace, if any
    namespace: Option<String>,
    /// The underlying polynomial
    pol: IndexedPolynomialReference<T>,
}

impl<T> NamespacedPolynomialReference<T> {
    /// Return a shifted polynomial based on this namespaced polynomial and a boolean shift
    pub fn with_shift(self, next: bool) -> ShiftedPolynomialReference<T> {
        ShiftedPolynomialReference {
            is_next: next,
            pol: self,
        }
    }

    /// Returns the optional namespace of this polynomial
    pub fn namespace(&self) -> &Option<String> {
        &self.namespace
    }

    /// Returns the optional index of the underlying polynomial in its declaration array
    pub fn index(&self) -> &Option<Box<Expression<T>>> {
        self.pol.index()
    }

    /// Returns the name of the declared polynomial or array of polynomials
    pub fn name(&self) -> &str {
        self.pol.name()
    }

    /// Returns a mutable reference to the declared polynomial or array of polynomials
    pub fn name_mut(&mut self) -> &mut String {
        self.pol.name_mut()
    }

    /// Return a shifted polynomial based on this namespaced polynomial with a shift of 1
    pub fn next(self) -> ShiftedPolynomialReference<T> {
        self.with_shift(true)
    }

    /// Return a shifted polynomial based on this namespaced polynomial with a shift of 0
    pub fn current(self) -> ShiftedPolynomialReference<T> {
        self.with_shift(false)
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial with an optional index to support unidimensional arrays of polynomials
pub struct IndexedPolynomialReference<T> {
    /// The optional index, is `Some` iff the declaration of this polynomial is an array
    index: Option<Box<Expression<T>>>,
    /// The underlying polynomial
    pol: PolynomialReference,
}

impl<T> IndexedPolynomialReference<T> {
    /// Return a namespaced polynomial based on this polynomial and an optional namespace
    pub fn with_namespace(self, namespace: Option<String>) -> NamespacedPolynomialReference<T> {
        NamespacedPolynomialReference {
            pol: self,
            namespace,
        }
    }

    /// Returns a mutable reference to the name of the declared polynomial or array of polynomials
    pub fn name_mut(&mut self) -> &mut String {
        self.pol.name_mut()
    }

    /// Returns the optional index of this polynomial in its declaration array
    pub fn index(&self) -> &Option<Box<Expression<T>>> {
        &self.index
    }

    /// Returns the name of the declared polynomial or array of polynomials
    pub fn name(&self) -> &str {
        self.pol.name()
    }

    /// Return a namespaced polynomial based on this polynomial and a namespace
    pub fn namespaced(self, namespace: String) -> NamespacedPolynomialReference<T> {
        self.with_namespace(Some(namespace))
    }

    /// Return a namespaced polynomial based on this polynomial and no namespace, defaulting to the closest enclosing namespace, if any
    pub fn local(self) -> NamespacedPolynomialReference<T> {
        self.with_namespace(None)
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial or array of polynomials
pub struct PolynomialReference {
    /// The name of this polynomial or array of polynomials
    name: String,
}

impl PolynomialReference {
    /// Returns an indexed polynomial using this polynomial and an optional index
    pub fn with_index<T>(self, index: Option<Expression<T>>) -> IndexedPolynomialReference<T> {
        IndexedPolynomialReference {
            pol: self,
            index: index.map(Box::new),
        }
    }

    /// Returns the name of this polynomial or array of polynomials
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns a mutable reference to the name of this polynomial or array of polynomials
    pub fn name_mut(&mut self) -> &mut String {
        &mut self.name
    }

    /// Returns a new polynomial or array of polynomials given a name
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self { name: name.into() }
    }

    /// Returns an indexed polynomial using this polynomial and an index. Used for polynomial array members.
    pub fn indexed<T>(self, index: Expression<T>) -> IndexedPolynomialReference<T> {
        self.with_index(Some(index))
    }

    /// Returns an indexed polynomial using this polynomial and an index. Used for polynomials which are not declared in arrays.
    pub fn single<T>(self) -> IndexedPolynomialReference<T> {
        self.with_index(None)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
}

/// The definition of a function (excluding its name):
/// Either a param-value mapping or an array expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionDefinition<T> {
    /// Parameter-value-mapping.
    Mapping(Vec<String>, Expression<T>),
    /// Array expression.
    Array(ArrayExpression<T>),
    /// Prover query.
    Query(Vec<String>, Expression<T>),
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
        self.pad_with(Expression::Number(0.into()))
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

/// Traverses the expression tree and calls `f` in post-order.
pub fn postvisit_expression_mut<T, F, B>(e: &mut Expression<T>, f: &mut F) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match e {
        Expression::PolynomialReference(_)
        | Expression::Constant(_)
        | Expression::PublicReference(_)
        | Expression::Number(_)
        | Expression::String(_) => {}
        Expression::BinaryOperation(left, _, right) => {
            postvisit_expression_mut(left, f)?;
            postvisit_expression_mut(right, f)?;
        }
        Expression::UnaryOperation(_, e) => postvisit_expression_mut(e.as_mut(), f)?,
        Expression::Tuple(items)
        | Expression::FunctionCall(FunctionCall {
            arguments: items, ..
        }) => items
            .iter_mut()
            .try_for_each(|item| postvisit_expression_mut(item, f))?,
        Expression::FreeInput(query) => postvisit_expression_mut(query.as_mut(), f)?,
        Expression::MatchExpression(scrutinee, arms) => {
            once(scrutinee.as_mut())
                .chain(arms.iter_mut().map(|(_n, e)| e))
                .try_for_each(|item| postvisit_expression_mut(item, f))?;
        }
    };
    f(e)
}

/// Traverses the expression trees of the statement and calls `f` in post-order.
/// Does not enter macro definitions.
pub fn postvisit_expression_in_statement_mut<T, F, B>(
    statement: &mut PilStatement<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match statement {
        PilStatement::FunctionCall(_, _, arguments) => arguments
            .iter_mut()
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::PlookupIdentity(_, left, right)
        | PilStatement::PermutationIdentity(_, left, right) => left
            .selector
            .iter_mut()
            .chain(left.expressions.iter_mut())
            .chain(right.selector.iter_mut())
            .chain(right.expressions.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),
        PilStatement::ConnectIdentity(_start, left, right) => left
            .iter_mut()
            .chain(right.iter_mut())
            .try_for_each(|e| postvisit_expression_mut(e, f)),

        PilStatement::Namespace(_, _, e)
        | PilStatement::PolynomialDefinition(_, _, e)
        | PilStatement::PolynomialIdentity(_, e)
        | PilStatement::PublicDeclaration(_, _, _, e)
        | PilStatement::ConstantDefinition(_, _, e) => postvisit_expression_mut(e, f),

        PilStatement::PolynomialConstantDefinition(_, _, fundef)
        | PilStatement::PolynomialCommitDeclaration(_, _, Some(fundef)) => match fundef {
            FunctionDefinition::Query(_, e) | FunctionDefinition::Mapping(_, e) => {
                postvisit_expression_mut(e, f)
            }
            FunctionDefinition::Array(ae) => postvisit_expression_in_array_expression_mut(ae, f),
        },
        PilStatement::PolynomialCommitDeclaration(_, _, None)
        | PilStatement::Include(_, _)
        | PilStatement::PolynomialConstantDeclaration(_, _)
        | PilStatement::MacroDefinition(_, _, _, _, _) => ControlFlow::Continue(()),
    }
}

fn postvisit_expression_in_array_expression_mut<T, F, B>(
    ae: &mut ArrayExpression<T>,
    f: &mut F,
) -> ControlFlow<B>
where
    F: FnMut(&mut Expression<T>) -> ControlFlow<B>,
{
    match ae {
        ArrayExpression::Value(expressions) | ArrayExpression::RepeatedValue(expressions) => {
            expressions
                .iter_mut()
                .try_for_each(|e| postvisit_expression_mut(e, f))
        }
        ArrayExpression::Concat(a1, a2) => [a1, a2]
            .iter_mut()
            .try_for_each(|e| postvisit_expression_in_array_expression_mut(e, f)),
    }
}
