pub mod asm;
pub mod build;
pub mod display;
pub mod folder;
pub mod types;
pub mod visitor;

use std::{
    collections::HashMap,
    iter::{empty, once},
    ops,
    str::FromStr,
    sync::Arc,
    vec,
};

use auto_enums::auto_enum;
use derive_more::Display;
use powdr_number::{BigInt, BigUint, DegreeType};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use powdr_parser_util::SourceRef;
use types::ArrayType;

use crate::analyzed::Reference;

use self::{
    asm::{Part, SymbolPath},
    types::{FunctionType, Type, TypeBounds, TypeScheme},
    visitor::{Children, ExpressionVisitable},
};

use crate::parsed::types::TupleType;

#[derive(Display, Clone, Copy, PartialEq, Eq)]
pub enum SymbolCategory {
    /// A value, which has a type and can be referenced in expressions (a variable, function, constant, ...).
    Value,
    /// A type, for example the name of an enum or other user-defined type.
    Type,
    /// A type constructor, i.e. an enum variant, which can be used as a function or constant inside an expression
    /// or to deconstruct a value in a pattern.
    TypeConstructor,
    /// A trait declaration, which can be used as a type.
    TraitDeclaration,
}
impl SymbolCategory {
    /// Returns if a symbol of a given category can satisfy a request for a certain category.
    pub fn compatible_with_request(&self, request: SymbolCategory) -> bool {
        match self {
            SymbolCategory::Value => request == SymbolCategory::Value,
            SymbolCategory::Type => request == SymbolCategory::Type,
            SymbolCategory::TypeConstructor => {
                // Type constructors can also satisfy requests for values.
                request == SymbolCategory::TypeConstructor || request == SymbolCategory::Value
            }
            SymbolCategory::TraitDeclaration => request == SymbolCategory::TraitDeclaration,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PILFile(pub Vec<PilStatement>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct NamespaceDegree {
    pub min: Expression,
    pub max: Expression,
}

impl Children<Expression> for NamespaceDegree {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression> + '_> {
        Box::new(once(&self.min).chain(once(&self.max)))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression> + '_> {
        Box::new(once(&mut self.min).chain(once(&mut self.max)))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PilStatement {
    /// File name
    Include(SourceRef, String),
    /// Name of namespace and optional polynomial degree (constant)
    Namespace(SourceRef, SymbolPath, Option<NamespaceDegree>),
    LetStatement(
        SourceRef,
        String,
        Option<TypeScheme<Expression>>,
        Option<Expression>,
    ),
    PolynomialDefinition(SourceRef, PolynomialName, Expression),
    PublicDeclaration(
        SourceRef,
        /// The name of the public value.
        String,
        /// The polynomial/column that contains the public value.
        NamespacedPolynomialReference,
        /// If the polynomial is an array, this is the array element index.
        Option<Expression>,
        /// The row number of the public value.
        Expression,
    ),
    PolynomialConstantDeclaration(SourceRef, Vec<PolynomialName>),
    PolynomialConstantDefinition(SourceRef, String, FunctionDefinition),
    PolynomialCommitDeclaration(
        SourceRef,
        // Stage
        Option<u32>,
        // Names
        Vec<PolynomialName>,
        // Value (prover query / hint)
        Option<FunctionDefinition>,
    ),
    EnumDeclaration(SourceRef, EnumDeclaration<Expression>),
    TraitImplementation(SourceRef, TraitImplementation<Expression>),
    TraitDeclaration(SourceRef, TraitDeclaration<Expression>),
    Expression(SourceRef, Expression),
}

impl PilStatement {
    /// If the statement is a symbol definition, returns all (local) names of defined symbols
    /// and their category.
    /// Note it does not return nested definitions (for an enum for example).
    pub fn symbol_definition_names(&self) -> impl Iterator<Item = (&String, SymbolCategory)> + '_ {
        self.symbol_definition_names_and_contained()
            .filter_map(|(name, sub_name, category)| match sub_name {
                Some(_) => None,
                None => Some((name, category)),
            })
    }

    /// If the statement is a symbol definition, returns all (local) names of defined symbols
    /// and their category.
    /// For an enum, returns the name of the enum and all the variants, where the first
    /// component is the name of the enum and the second the name of the variant.
    pub fn symbol_definition_names_and_contained(
        &self,
    ) -> Box<dyn Iterator<Item = (&String, Option<&String>, SymbolCategory)> + '_> {
        match self {
            PilStatement::PolynomialDefinition(_, PolynomialName { name, .. }, _)
            | PilStatement::PolynomialConstantDefinition(_, name, _)
            | PilStatement::PublicDeclaration(_, name, _, _, _)
            | PilStatement::LetStatement(_, name, _, _) => {
                Box::new(once((name, None, SymbolCategory::Value)))
            }
            PilStatement::EnumDeclaration(_, EnumDeclaration { name, variants, .. }) => Box::new(
                once((name, None, SymbolCategory::Type)).chain(
                    variants
                        .iter()
                        .map(move |v| (name, Some(&v.name), SymbolCategory::TypeConstructor)),
                ),
            ),
            PilStatement::TraitDeclaration(
                _,
                TraitDeclaration {
                    name, functions, ..
                },
            ) => Box::new(
                once((name, None, SymbolCategory::TraitDeclaration)).chain(
                    functions
                        .iter()
                        .map(move |f| (name, Some(&f.name), SymbolCategory::Value)),
                ),
            ),
            PilStatement::PolynomialConstantDeclaration(_, polynomials)
            | PilStatement::PolynomialCommitDeclaration(_, _, polynomials, _) => Box::new(
                polynomials
                    .iter()
                    .map(|p| (&p.name, None, SymbolCategory::Value)),
            ),

            PilStatement::Include(_, _)
            | PilStatement::Namespace(_, _, _)
            | PilStatement::Expression(_, _)
            | PilStatement::TraitImplementation(_, _) => Box::new(empty()),
        }
    }
}

impl Children<Expression> for PilStatement {
    /// Returns an iterator over all (top-level) expressions in this statement.
    fn children(&self) -> Box<dyn Iterator<Item = &Expression> + '_> {
        match self {
            PilStatement::Expression(_, e) => Box::new(once(e)),
            PilStatement::Namespace(_, _, Some(d)) => d.children(),
            PilStatement::PolynomialDefinition(_, PolynomialName { array_size, .. }, e) => {
                Box::new(array_size.iter().chain(once(e)))
            }

            PilStatement::EnumDeclaration(_, enum_decl) => enum_decl.children(),
            PilStatement::TraitImplementation(_, trait_impl) => trait_impl.children(),
            PilStatement::TraitDeclaration(_, trait_decl) => trait_decl.children(),

            PilStatement::LetStatement(_, _, type_scheme, value) => Box::new(
                type_scheme
                    .iter()
                    .flat_map(|t| t.ty.children())
                    .chain(value),
            ),

            PilStatement::PublicDeclaration(_, _, _, i, e) => Box::new(i.iter().chain(once(e))),

            PilStatement::PolynomialConstantDefinition(_, _, def)
            | PilStatement::PolynomialCommitDeclaration(_, _, _, Some(def)) => def.children(),
            PilStatement::PolynomialCommitDeclaration(_, _, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::Namespace(_, _, None)
            | PilStatement::PolynomialConstantDeclaration(_, _) => Box::new(empty()),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this statement.
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression> + '_> {
        match self {
            PilStatement::Expression(_, e) => Box::new(once(e)),
            PilStatement::Namespace(_, _, Some(d)) => d.children_mut(),
            PilStatement::PolynomialDefinition(_, PolynomialName { array_size, .. }, e) => {
                Box::new(array_size.iter_mut().chain(once(e)))
            }

            PilStatement::EnumDeclaration(_, enum_decl) => enum_decl.children_mut(),
            PilStatement::TraitImplementation(_, trait_impl) => trait_impl.children_mut(),
            PilStatement::TraitDeclaration(_, trait_decl) => trait_decl.children_mut(),

            PilStatement::LetStatement(_, _, ty, value) => {
                Box::new(ty.iter_mut().flat_map(|t| t.ty.children_mut()).chain(value))
            }

            PilStatement::PublicDeclaration(_, _, _, i, e) => Box::new(i.iter_mut().chain(once(e))),

            PilStatement::PolynomialConstantDefinition(_, _, def)
            | PilStatement::PolynomialCommitDeclaration(_, _, _, Some(def)) => def.children_mut(),
            PilStatement::PolynomialCommitDeclaration(_, _, _, None)
            | PilStatement::Include(_, _)
            | PilStatement::Namespace(_, _, None)
            | PilStatement::PolynomialConstantDeclaration(_, _) => Box::new(empty()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EnumDeclaration<E = u64> {
    pub name: String,
    pub type_vars: TypeBounds,
    pub variants: Vec<EnumVariant<E>>,
}

impl<R> Children<Expression<R>> for EnumDeclaration<u64> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(empty())
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(empty())
    }
}

impl<R> Children<Expression<R>> for EnumDeclaration<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(self.variants.iter().flat_map(|v| v.children()))
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(self.variants.iter_mut().flat_map(|v| v.children_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EnumVariant<E = u64> {
    pub name: String,
    pub fields: Option<Vec<Type<E>>>,
}

impl<E: Clone> EnumVariant<E> {
    /// Returns the type of the constructor function for this variant
    /// given the enum type.
    pub fn constructor_type(&self, enum_decl: &EnumDeclaration) -> TypeScheme<E> {
        let name = SymbolPath::from_str(&enum_decl.name).unwrap();
        let vars = enum_decl.type_vars.clone();
        let generic_args =
            (!vars.is_empty()).then(|| vars.vars().cloned().map(Type::TypeVar).collect::<Vec<_>>());

        let named_type = Type::NamedType(name, generic_args);

        let ty = match &self.fields {
            None => named_type,
            Some(fields) => Type::Function(FunctionType {
                params: (*fields).clone(),
                value: named_type.into(),
            }),
        };

        TypeScheme { vars, ty }
    }
}

impl<R> Children<Expression<R>> for EnumVariant<u64> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(empty())
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(empty())
    }
}

impl<R> Children<Expression<R>> for EnumVariant<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(
            self.fields
                .iter()
                .flat_map(|f| f.iter())
                .flat_map(|f| f.children()),
        )
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(
            self.fields
                .iter_mut()
                .flat_map(|f| f.iter_mut())
                .flat_map(|f| f.children_mut()),
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TraitImplementation<Expr> {
    pub name: SymbolPath,
    pub source_ref: SourceRef,
    pub type_scheme: TypeScheme,
    pub functions: Vec<NamedExpression<Arc<Expr>>>,
}

impl<R> TraitImplementation<Expression<R>> {
    pub fn function_by_name(&self, name: &str) -> Option<&NamedExpression<Arc<Expression<R>>>> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn type_of_function(&self, trait_decl: &TraitDeclaration, fn_name: &str) -> Type {
        let Type::Tuple(TupleType { items }) = &self.type_scheme.ty else {
            panic!("Expected tuple type for trait implementation");
        };

        let type_var_mapping: HashMap<String, Type> = trait_decl
            .type_vars
            .iter()
            .cloned()
            .zip(items.iter().cloned())
            .collect();

        let trait_fn = trait_decl
            .function_by_name(fn_name)
            .expect("Function not found in trait declaration");

        let mut trait_type = trait_fn.ty.clone();
        trait_type.substitute_type_vars(&type_var_mapping);
        trait_type
    }
}

impl<R> Children<Expression<R>> for TraitImplementation<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(self.functions.iter().flat_map(|m| m.body.children()))
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(
            self.functions
                .iter_mut()
                .map(|named_expr| Arc::get_mut(&mut named_expr.body).unwrap()),
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct NamedExpression<Expr> {
    pub name: String,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TraitDeclaration<E = u64> {
    pub name: String,
    pub type_vars: Vec<String>,
    pub functions: Vec<NamedType<E>>,
}

impl TraitDeclaration<u64> {
    pub fn function_by_name(&self, name: &str) -> Option<&NamedType> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn function_by_name_mut(&mut self, name: &str) -> Option<&mut NamedType> {
        self.functions.iter_mut().find(|f| f.name == name)
    }
}

impl<R> Children<Expression<R>> for TraitDeclaration<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(self.functions.iter().flat_map(|f| f.children()))
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(self.functions.iter_mut().flat_map(|f| f.children_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct NamedType<E = u64> {
    pub name: String,
    pub ty: Type<E>,
}

impl<R> Children<Expression<R>> for NamedType<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        self.ty.children()
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        self.ty.children_mut()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SelectedExpressions<E = Expression<NamespacedPolynomialReference>> {
    pub selector: Option<E>,
    pub expressions: Box<E>,
}

impl<T> Default for SelectedExpressions<Expression<T>> {
    fn default() -> Self {
        Self {
            selector: Default::default(),
            expressions: Box::new(ArrayLiteral { items: vec![] }.into()),
        }
    }
}

impl<T> Children<Expression<T>> for SelectedExpressions<Expression<T>> {
    /// Returns an iterator over all (top-level) expressions in this SelectedExpressions.
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<T>> + '_> {
        Box::new(self.selector.iter().chain(self.expressions.children()))
    }

    /// Returns an iterator over all (top-level) expressions in this SelectedExpressions.
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<T>> + '_> {
        Box::new(
            self.selector
                .iter_mut()
                .chain(self.expressions.children_mut()),
        )
    }
}

#[derive(Debug, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum Expression<Ref = NamespacedPolynomialReference> {
    Reference(SourceRef, Ref),
    PublicReference(SourceRef, String),
    // A number literal and its type.
    Number(SourceRef, Number),
    String(SourceRef, String),
    Tuple(SourceRef, Vec<Self>),
    LambdaExpression(SourceRef, LambdaExpression<Self>),
    ArrayLiteral(SourceRef, ArrayLiteral<Self>),
    UnaryOperation(SourceRef, UnaryOperation<Self>),
    BinaryOperation(SourceRef, BinaryOperation<Self>),
    IndexAccess(SourceRef, IndexAccess<Self>),
    FunctionCall(SourceRef, FunctionCall<Self>),
    FreeInput(SourceRef, Box<Self>),
    MatchExpression(SourceRef, MatchExpression<Self>),
    IfExpression(SourceRef, IfExpression<Self>),
    BlockExpression(SourceRef, BlockExpression<Self>),
}

/// Comparison function for expressions that ignore source information.
macro_rules! impl_partial_eq_for_expression {
    ($($variant:ident),*) => {
        impl<Ref: PartialEq> PartialEq for Expression<Ref> {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (Expression::$variant(_, a), Expression::$variant(_, b)) => a == b,
                    )*
                    // This catches the case where variants are different and returns false
                    $(
                        (Expression::$variant(_, _), _) => false,
                    )*
                }
            }
        }
    }
}

impl_partial_eq_for_expression!(
    Reference,
    PublicReference,
    Number,
    String,
    Tuple,
    LambdaExpression,
    ArrayLiteral,
    BinaryOperation,
    UnaryOperation,
    IndexAccess,
    FunctionCall,
    FreeInput,
    MatchExpression,
    IfExpression,
    BlockExpression
);

pub trait SourceReference {
    fn source_reference(&self) -> &SourceRef;
    fn source_reference_mut(&mut self) -> &mut SourceRef;
}

macro_rules! impl_source_reference {
    ($enum:ident, $($variant:ident),*) => {
        impl<E> SourceReference for $enum<E> {
            fn source_reference(&self) -> &SourceRef {
                match self {
                    $( $enum::$variant(src, _) => src, )*
                }
            }

            fn source_reference_mut(&mut self) -> &mut SourceRef {
                match self {
                    $( $enum::$variant(src, _) => src, )*
                }
            }
        }
    }
}

impl_source_reference!(
    Expression,
    Reference,
    PublicReference,
    Number,
    String,
    Tuple,
    LambdaExpression,
    ArrayLiteral,
    BinaryOperation,
    UnaryOperation,
    IndexAccess,
    FunctionCall,
    FreeInput,
    MatchExpression,
    IfExpression,
    BlockExpression
);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct UnaryOperation<E = Expression<NamespacedPolynomialReference>> {
    pub op: UnaryOperator,
    pub expr: Box<E>,
}

impl<Ref> From<UnaryOperation<Expression<Ref>>> for Expression<Ref> {
    fn from(operation: UnaryOperation<Expression<Ref>>) -> Self {
        Expression::UnaryOperation(SourceRef::unknown(), operation)
    }
}

impl<E> Children<E> for UnaryOperation<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(once(self.expr.as_ref()))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(once(self.expr.as_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct BinaryOperation<E = Expression<NamespacedPolynomialReference>> {
    pub left: Box<E>,
    pub op: BinaryOperator,
    pub right: Box<E>,
}

impl<Ref> From<BinaryOperation<Expression<Ref>>> for Expression<Ref> {
    fn from(operation: BinaryOperation<Expression<Ref>>) -> Self {
        Expression::BinaryOperation(SourceRef::unknown(), operation)
    }
}

impl<E> Children<E> for BinaryOperation<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new([self.left.as_ref(), self.right.as_ref()].into_iter())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new([self.left.as_mut(), self.right.as_mut()].into_iter())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Number {
    #[schemars(skip)]
    pub value: BigUint,
    pub type_: Option<Type>,
}

impl<Ref> From<Number> for Expression<Ref> {
    fn from(number: Number) -> Self {
        Expression::Number(SourceRef::unknown(), number)
    }
}

impl<Ref> From<BigUint> for Expression<Ref> {
    fn from(value: BigUint) -> Self {
        Number { value, type_: None }.into()
    }
}

impl<Ref> From<u32> for Expression<Ref> {
    fn from(value: u32) -> Self {
        BigUint::from(value).into()
    }
}
pub type ExpressionPrecedence = u64;

impl<Ref> Expression<Ref> {
    pub fn new_binary(left: Self, op: BinaryOperator, right: Self) -> Self {
        Expression::BinaryOperation(
            SourceRef::unknown(),
            BinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
        )
    }

    /// Visits this expression and all of its sub-expressions and returns true
    /// if `f` returns true on any of them.
    pub fn any(&self, mut f: impl FnMut(&Self) -> bool) -> bool {
        use std::ops::ControlFlow;
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MatchExpression<E = Expression<NamespacedPolynomialReference>> {
    pub scrutinee: Box<E>,
    pub arms: Vec<MatchArm<E>>,
}

impl<Ref> From<MatchExpression<Expression<Ref>>> for Expression<Ref> {
    fn from(match_expr: MatchExpression<Expression<Ref>>) -> Self {
        Expression::MatchExpression(SourceRef::unknown(), match_expr)
    }
}

impl<E> Children<E> for MatchExpression<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(
            once(self.scrutinee.as_ref()).chain(self.arms.iter().flat_map(|arm| arm.children())),
        )
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(
            once(self.scrutinee.as_mut())
                .chain(self.arms.iter_mut().flat_map(|arm| arm.children_mut())),
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct BlockExpression<E> {
    pub statements: Vec<StatementInsideBlock<E>>,
    pub expr: Option<Box<E>>,
}

impl<Ref> From<BlockExpression<Expression<Ref>>> for Expression<Ref> {
    fn from(block: BlockExpression<Expression<Ref>>) -> Self {
        Expression::BlockExpression(SourceRef::unknown(), block)
    }
}

impl<E> Children<E> for BlockExpression<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(
            self.statements
                .iter()
                .flat_map(|s| s.children())
                .chain(self.expr.iter().map(|boxed| &**boxed)),
        )
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(
            self.statements
                .iter_mut()
                .flat_map(|s| s.children_mut())
                .chain(self.expr.iter_mut().map(|boxed| &mut **boxed)),
        )
    }
}

impl Expression<NamespacedPolynomialReference> {
    pub fn try_to_identifier(&self) -> Option<&String> {
        if let Expression::Reference(_, r) = self {
            r.try_to_identifier()
        } else {
            None
        }
    }
}

impl<Ref> ops::Add for Expression<Ref> {
    type Output = Expression<Ref>;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Add, rhs)
    }
}

impl<Ref> ops::Sub for Expression<Ref> {
    type Output = Expression<Ref>;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Sub, rhs)
    }
}
impl<Ref> ops::Mul for Expression<Ref> {
    type Output = Expression<Ref>;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, BinaryOperator::Mul, rhs)
    }
}

impl<Ref> std::iter::Sum for Expression<Ref> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|a, b| a + b).unwrap_or_else(|| 0u32.into())
    }
}

impl From<NamespacedPolynomialReference> for Expression {
    fn from(value: NamespacedPolynomialReference) -> Self {
        Self::Reference(SourceRef::unknown(), value)
    }
}

impl<R> Expression<R> {
    /// Returns an iterator over all (top-level) expressions in this expression.
    /// This specifically does not implement Children because otherwise it would
    /// have a wrong implementation of ExpressionVisitable (which is implemented
    /// generically for all types that implement Children<Expr>).
    #[auto_enum(Iterator)]
    pub fn children(&self) -> impl Iterator<Item = &Expression<R>> + '_ {
        match self {
            Expression::Reference(_, _)
            | Expression::PublicReference(_, _)
            | Expression::String(_, _) => empty(),
            Expression::Number(_, _) => empty(),
            Expression::Tuple(_, v) => v.iter(),
            Expression::LambdaExpression(_, lambda) => lambda.children(),
            Expression::ArrayLiteral(_, array) => array.children(),
            Expression::BinaryOperation(_, binary_op) => binary_op.children(),
            Expression::UnaryOperation(_, unary_op) => unary_op.children(),
            Expression::IndexAccess(_, index_access) => index_access.children(),
            Expression::FunctionCall(_, function_call) => function_call.children(),
            Expression::FreeInput(_, e) => once(e.as_ref()),
            Expression::MatchExpression(_, match_expr) => match_expr.children(),
            Expression::IfExpression(_, if_expr) => if_expr.children(),
            Expression::BlockExpression(_, block_expr) => block_expr.children(),
        }
    }

    /// Returns an iterator over all (top-level) expressions in this expression.
    /// This specifically does not implement Children because otherwise it would
    /// have a wrong implementation of ExpressionVisitable (which is implemented
    /// generically for all types that implement Children<Expr>).
    #[auto_enum(Iterator)]
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut Expression<R>> + '_ {
        match self {
            Expression::Reference(_, _)
            | Expression::PublicReference(_, _)
            | Expression::String(_, _) => empty(),
            Expression::Number(_, _) => empty(),
            Expression::Tuple(_, v) => v.iter_mut(),
            Expression::LambdaExpression(_, lambda) => lambda.children_mut(),
            Expression::ArrayLiteral(_, array) => array.children_mut(),
            Expression::BinaryOperation(_, binary_op) => binary_op.children_mut(),
            Expression::UnaryOperation(_, unary_op) => unary_op.children_mut(),
            Expression::IndexAccess(_, index_access) => index_access.children_mut(),
            Expression::FunctionCall(_, function_call) => function_call.children_mut(),
            Expression::FreeInput(_, e) => once(e.as_mut()),
            Expression::MatchExpression(_, match_expr) => match_expr.children_mut(),
            Expression::IfExpression(_, if_expr) => if_expr.children_mut(),
            Expression::BlockExpression(_, block_expr) => block_expr.children_mut(),
        }
    }

    /// Returns true if the expression contains a reference to a next value
    pub fn contains_next_ref(&self) -> bool {
        self.expr_any(|e| {
            matches!(
                e,
                Expression::UnaryOperation(
                    _,
                    UnaryOperation {
                        op: UnaryOperator::Next,
                        ..
                    }
                )
            )
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
pub struct PolynomialName {
    pub name: String,
    pub array_size: Option<Expression>,
}

impl From<String> for PolynomialName {
    fn from(name: String) -> Self {
        Self {
            name,
            array_size: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, PartialOrd, Ord)]
/// A polynomial with an optional namespace
/// This is different from SymbolPath mainly due to different formatting.
pub struct NamespacedPolynomialReference {
    pub path: SymbolPath,
    pub type_args: Option<Vec<Type<Expression>>>,
}

impl From<SymbolPath> for NamespacedPolynomialReference {
    fn from(value: SymbolPath) -> Self {
        Self {
            path: value,
            type_args: Default::default(),
        }
    }
}

impl NamespacedPolynomialReference {
    pub fn from_identifier(name: String) -> Self {
        SymbolPath::from_parts(vec![Part::Named(name)]).into()
    }

    pub fn try_to_identifier(&self) -> Option<&String> {
        if self.type_args.is_none() {
            self.path.try_to_identifier()
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
pub struct LambdaExpression<E = Expression<NamespacedPolynomialReference>> {
    pub kind: FunctionKind,
    pub params: Vec<Pattern>,
    pub body: Box<E>,
}

impl<Ref> From<LambdaExpression<Expression<Ref>>> for Expression<Ref> {
    fn from(lambda: LambdaExpression<Expression<Ref>>) -> Self {
        Expression::LambdaExpression(SourceRef::unknown(), lambda)
    }
}

impl<E> Children<E> for LambdaExpression<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(once(self.body.as_ref()))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(once(self.body.as_mut()))
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub enum FunctionKind {
    Pure,
    Constr,
    Query,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema)]
pub struct ArrayLiteral<E = Expression<NamespacedPolynomialReference>> {
    pub items: Vec<E>,
}

impl<Ref> From<ArrayLiteral<Expression<Ref>>> for Expression<Ref> {
    fn from(array: ArrayLiteral<Expression<Ref>>) -> Self {
        Expression::ArrayLiteral(SourceRef::unknown(), array)
    }
}

impl<E> Children<E> for ArrayLiteral<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(self.items.iter())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(self.items.iter_mut())
    }
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
    In,
    Is,
    Connect,
    Select, // $
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperatorAssociativity {
    Left,
    Right,
    RequireParentheses,
}

trait Precedence {
    fn precedence(&self) -> Option<ExpressionPrecedence>;
}

impl Precedence for UnaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        use UnaryOperator::*;
        let precedence = match self {
            // NOTE: Any modification must be done with care to not overlap with BinaryOperator's precedence
            Next => 1,
            Minus | LogicalNot => 2,
        };

        Some(precedence)
    }
}

impl Precedence for BinaryOperator {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        use BinaryOperator::*;
        let precedence = match self {
            // NOTE: Any modification must be done with care to not overlap with LambdaExpression's precedence
            // Unary Oprators
            // **
            Pow => 3,
            // * / %
            Mul | Div | Mod => 4,
            // + -
            Add | Sub => 5,
            // << >>
            ShiftLeft | ShiftRight => 6,
            // &
            BinaryAnd => 7,
            // ^
            BinaryXor => 8,
            // |
            BinaryOr => 9,
            // = == != < > <= >=
            Identity | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual => 10,
            // &&
            LogicalAnd => 11,
            // ||
            LogicalOr => 12,
            // $
            Select => 14,
            // in is connect
            In | Is | Connect => 15,
        };

        Some(precedence)
    }
}

impl<E> Precedence for LambdaExpression<E> {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        Some(13)
    }
}

impl<E> Precedence for Expression<E> {
    fn precedence(&self) -> Option<ExpressionPrecedence> {
        match self {
            Expression::UnaryOperation(_, operation) => operation.op.precedence(),
            Expression::BinaryOperation(_, operation) => operation.op.precedence(),
            Expression::LambdaExpression(_, lambda) => lambda.precedence(),
            _ => None,
        }
    }
}

impl BinaryOperator {
    pub fn associativity(&self) -> BinaryOperatorAssociativity {
        use BinaryOperator::*;
        use BinaryOperatorAssociativity::*;
        match self {
            Identity | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual => {
                RequireParentheses
            }
            Pow => Right,

            // .. ..= => RequireParentheses,
            _ => Left,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct IndexAccess<E = Expression<NamespacedPolynomialReference>> {
    pub array: Box<E>,
    pub index: Box<E>,
}

impl<Ref> From<IndexAccess<Expression<Ref>>> for Expression<Ref> {
    fn from(ia: IndexAccess<Expression<Ref>>) -> Self {
        Expression::IndexAccess(SourceRef::unknown(), ia)
    }
}

impl<E> Children<E> for IndexAccess<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(once(self.array.as_ref()).chain(once(self.index.as_ref())))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(once(self.array.as_mut()).chain(once(self.index.as_mut())))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FunctionCall<E = Expression<NamespacedPolynomialReference>> {
    pub function: Box<E>,
    pub arguments: Vec<E>,
}

impl<Ref> From<FunctionCall<Expression<Ref>>> for Expression<Ref> {
    fn from(call: FunctionCall<Expression<Ref>>) -> Self {
        Expression::FunctionCall(SourceRef::unknown(), call)
    }
}

impl<E> Children<E> for FunctionCall<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(once(self.function.as_ref()).chain(self.arguments.iter()))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(once(self.function.as_mut()).chain(self.arguments.iter_mut()))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MatchArm<E = Expression<NamespacedPolynomialReference>> {
    pub pattern: Pattern,
    pub value: E,
}

impl<E> Children<E> for MatchArm<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(once(&self.value))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(once(&mut self.value))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct IfExpression<E = Expression<NamespacedPolynomialReference>> {
    pub condition: Box<E>,
    pub body: Box<E>,
    pub else_body: Box<E>,
}

impl<Ref> From<IfExpression<Expression<Ref>>> for Expression<Ref> {
    fn from(ifexpr: IfExpression<Expression<Ref>>) -> Self {
        Expression::IfExpression(SourceRef::unknown(), ifexpr)
    }
}

impl<E> Children<E> for IfExpression<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(
            once(&self.condition)
                .chain(once(&self.body))
                .chain(once(&self.else_body))
                .map(|e| e.as_ref()),
        )
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(
            once(&mut self.condition)
                .chain(once(&mut self.body))
                .chain(once(&mut self.else_body))
                .map(|e| e.as_mut()),
        )
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum StatementInsideBlock<E = Expression<NamespacedPolynomialReference>> {
    // TODO add a source ref here.
    LetStatement(LetStatementInsideBlock<E>),
    Expression(E),
}

impl<E> Children<E> for StatementInsideBlock<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        match self {
            StatementInsideBlock::LetStatement(l) => Box::new(l.children()),
            StatementInsideBlock::Expression(e) => Box::new(once(e)),
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        match self {
            StatementInsideBlock::LetStatement(l) => Box::new(l.children_mut()),
            StatementInsideBlock::Expression(e) => Box::new(once(e)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct LetStatementInsideBlock<E = Expression<NamespacedPolynomialReference>> {
    pub pattern: Pattern,
    pub ty: Option<Type<u64>>,
    pub value: Option<E>,
}

impl<E> Children<E> for LetStatementInsideBlock<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &E> + '_> {
        Box::new(self.value.iter())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut E> + '_> {
        Box::new(self.value.iter_mut())
    }
}

impl<E> From<LetStatementInsideBlock<E>> for StatementInsideBlock<E> {
    fn from(let_statement: LetStatementInsideBlock<E>) -> Self {
        StatementInsideBlock::LetStatement(let_statement)
    }
}

/// The definition of a function (excluding its name):
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum FunctionDefinition {
    /// Array expression.
    Array(ArrayExpression),
    /// Generic expression
    Expression(Expression),
    /// A type declaration.
    TypeDeclaration(EnumDeclaration<Expression>),
    /// A trait declaration.
    TraitDeclaration(TraitDeclaration<Expression>),
}

impl Children<Expression> for FunctionDefinition {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression> + '_> {
        match self {
            FunctionDefinition::Array(ae) => ae.children(),
            FunctionDefinition::Expression(e) => Box::new(once(e)),
            FunctionDefinition::TypeDeclaration(_enum_declaration) => todo!(),
            FunctionDefinition::TraitDeclaration(trait_declaration) => trait_declaration.children(),
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression> + '_> {
        match self {
            FunctionDefinition::Array(ae) => ae.children_mut(),
            FunctionDefinition::Expression(e) => Box::new(once(e)),
            FunctionDefinition::TypeDeclaration(_enum_declaration) => todo!(),
            FunctionDefinition::TraitDeclaration(trait_declaration) => {
                trait_declaration.children_mut()
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum ArrayExpression<Ref = NamespacedPolynomialReference> {
    Value(Vec<Expression<Ref>>),
    RepeatedValue(Vec<Expression<Ref>>),
    Concat(Box<ArrayExpression<Ref>>, Box<ArrayExpression<Ref>>),
}

impl ArrayExpression {
    pub fn value(v: Vec<Expression>) -> Self {
        Self::Value(v)
    }

    pub fn repeated_value(v: Vec<Expression>) -> Self {
        Self::RepeatedValue(v)
    }

    pub fn concat(self, other: Self) -> Self {
        Self::Concat(Box::new(self), Box::new(other))
    }

    fn pad_with(self, pad: Expression) -> Self {
        Self::concat(self, Self::repeated_value(vec![pad]))
    }

    pub fn pad_with_zeroes(self) -> Self {
        self.pad_with(0u32.into())
    }

    fn last(&self) -> Option<&Expression> {
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

impl<Ref> ArrayExpression<Ref> {
    /// solve for `*`
    fn solve(&self, degree: DegreeType) -> DegreeType {
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

/// An array of elements that might be repeated.
pub struct RepeatedArray<'a> {
    /// The pattern to be repeated
    pattern: &'a [Expression<Reference>],
    /// The number of values to be filled by repeating the pattern, possibly truncating it at the end
    size: DegreeType,
}

impl<'a> RepeatedArray<'a> {
    pub fn new(pattern: &'a [Expression<Reference>], size: DegreeType) -> Self {
        if pattern.is_empty() {
            assert!(
                size == 0,
                "impossible to fill {size} values with an empty pattern"
            )
        }
        Self { pattern, size }
    }

    /// Returns the number of elements in this array (including repetitions).
    pub fn size(&self) -> DegreeType {
        self.size
    }

    /// Returns the pattern to be repeated
    pub fn pattern(&self) -> &'a [Expression<Reference>] {
        self.pattern
    }
}

impl ArrayExpression<Reference> {
    pub fn to_repeated_arrays<'a>(
        &'a self,
        degree: DegreeType,
    ) -> Box<dyn Iterator<Item = RepeatedArray<'a>> + 'a> {
        let size_of_repeated_part = self.solve(degree);
        self.to_repeated_arrays_rec(size_of_repeated_part)
    }

    fn to_repeated_arrays_rec<'a>(
        &'a self,
        size_of_repeated_part: DegreeType,
    ) -> Box<dyn Iterator<Item = RepeatedArray<'a>> + 'a> {
        match self {
            ArrayExpression::Value(pattern) => {
                Box::new(once(RepeatedArray::new(pattern, pattern.len() as u64)))
            }
            ArrayExpression::RepeatedValue(pattern) => {
                Box::new(once(RepeatedArray::new(pattern, size_of_repeated_part)))
            }
            ArrayExpression::Concat(left, right) => Box::new(
                left.to_repeated_arrays_rec(size_of_repeated_part)
                    .chain(right.to_repeated_arrays_rec(size_of_repeated_part)),
            ),
        }
    }
}

impl<Ref> Children<Expression<Ref>> for ArrayExpression<Ref> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<Ref>> + '_> {
        match self {
            ArrayExpression::Value(v) | ArrayExpression::RepeatedValue(v) => Box::new(v.iter()),
            ArrayExpression::Concat(left, right) => {
                Box::new(left.children().chain(right.children()))
            }
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<Ref>> + '_> {
        match self {
            ArrayExpression::Value(v) | ArrayExpression::RepeatedValue(v) => Box::new(v.iter_mut()),
            ArrayExpression::Concat(left, right) => {
                Box::new(left.children_mut().chain(right.children_mut()))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum Pattern {
    CatchAll(SourceRef), // "_", matches a single value
    Ellipsis(SourceRef), // "..", matches a series of values, only valid inside array patterns
    #[schemars(skip)]
    Number(SourceRef, BigInt),
    String(SourceRef, String),
    Tuple(SourceRef, Vec<Pattern>),
    Array(SourceRef, Vec<Pattern>),
    // A pattern that binds a variable. Variable references are parsed as
    // Enum and are then re-mapped to Variable if they do not reference
    // an enum variant.
    Variable(SourceRef, String),
    Enum(SourceRef, SymbolPath, Option<Vec<Pattern>>),
}

impl Pattern {
    /// Returns an iterator over all variables in this pattern.
    pub fn variables(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        match self {
            Pattern::Variable(_, v) => Box::new(once(v)),
            _ => Box::new(self.children().flat_map(|p| p.variables())),
        }
    }

    /// Return true if the pattern is irrefutable, i.e. matches all possible values of its type.
    pub fn is_irrefutable(&self) -> bool {
        match self {
            Pattern::Ellipsis(_) => unreachable!(),
            Pattern::CatchAll(_) | Pattern::Variable(_, _) => true,
            Pattern::Number(_, _) | Pattern::String(_, _) | Pattern::Enum(_, _, _) => false,
            Pattern::Array(_, items) => {
                // Only "[..]"" is irrefutable
                matches!(&items[..], [Pattern::Ellipsis(_)])
            }
            Pattern::Tuple(_, p) => p.iter().all(|p| p.is_irrefutable()),
        }
    }
}

impl Children<Pattern> for Pattern {
    fn children(&self) -> Box<dyn Iterator<Item = &Pattern> + '_> {
        match self {
            Pattern::CatchAll(_)
            | Pattern::Ellipsis(_)
            | Pattern::Number(_, _)
            | Pattern::String(_, _)
            | Pattern::Variable(_, _) => Box::new(empty()),
            Pattern::Tuple(_, p) | Pattern::Array(_, p) => Box::new(p.iter()),
            Pattern::Enum(_, _, fields) => Box::new(fields.iter().flatten()),
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Pattern> + '_> {
        match self {
            Pattern::CatchAll(_)
            | Pattern::Ellipsis(_)
            | Pattern::Number(_, _)
            | Pattern::String(_, _)
            | Pattern::Variable(_, _) => Box::new(empty()),
            Pattern::Tuple(_, p) | Pattern::Array(_, p) => Box::new(p.iter_mut()),
            Pattern::Enum(_, _, fields) => Box::new(fields.iter_mut().flatten()),
        }
    }
}

impl SourceReference for Pattern {
    fn source_reference(&self) -> &SourceRef {
        match self {
            Pattern::CatchAll(s)
            | Pattern::Ellipsis(s)
            | Pattern::Number(s, _)
            | Pattern::String(s, _)
            | Pattern::Variable(s, _)
            | Pattern::Tuple(s, _)
            | Pattern::Array(s, _)
            | Pattern::Enum(s, _, _) => s,
        }
    }
    fn source_reference_mut(&mut self) -> &mut SourceRef {
        match self {
            Pattern::CatchAll(s)
            | Pattern::Ellipsis(s)
            | Pattern::Number(s, _)
            | Pattern::String(s, _)
            | Pattern::Variable(s, _)
            | Pattern::Tuple(s, _)
            | Pattern::Array(s, _)
            | Pattern::Enum(s, _, _) => s,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TypedExpression<Ref = NamespacedPolynomialReference, E = Expression<Ref>> {
    pub e: Expression<Ref>,
    pub type_scheme: Option<TypeScheme<E>>,
}

#[derive(Debug)]
pub struct MatchAnalysisReport {
    pub is_exhaustive: bool,
    pub redundant_patterns: Vec<usize>,
}
#[derive(Debug, Clone)]
enum PatternSpace {
    Any,
    Contained(Vec<PatternSpace>, bool), // ellipsis and variable-size array not implemented
    Finite(FinitePatternSpace),
    Infinite(InfinitePatternSpace, bool),
}

#[derive(Debug, Clone, PartialEq)]
enum FinitePatternSpace {
    Enum(Vec<(String, Option<Vec<PatternSpace>>)>),
    Bool(Option<bool>),
}

#[derive(Debug, Clone, PartialEq)]
enum InfinitePatternSpace {
    String(Vec<String>),
    Number(Vec<BigInt>),
}

impl PartialEq for PatternSpace {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Contained(a, b1), Self::Contained(c, b2)) => {
                b1 == b2 && a.len() == c.len() && a.iter().all(|item| c.contains(item))
            }
            (Self::Finite(a), Self::Finite(b)) => a == b,
            (Self::Infinite(a, f1), Self::Infinite(b, f2)) => a == b && f1 == f2,
            _ => false,
        }
    }
}

impl PatternSpace {
    fn substract(&self, other: &Pattern) -> Self {
        match (self, other) {
            (p, Pattern::CatchAll(_)) | (p, Pattern::Variable(_, _)) => p.cover_all_space(),
            (PatternSpace::Any, _) => self.clone(),
            (PatternSpace::Contained(ps, b), Pattern::Array(_, items))
            | (PatternSpace::Contained(ps, b), Pattern::Tuple(_, items)) => {
                let new_ps = ps.iter().zip(items).map(|(p, o)| p.substract(o)).collect();
                PatternSpace::Contained(new_ps, *b)
            }
            (
                PatternSpace::Finite(FinitePatternSpace::Enum(enums)),
                Pattern::Enum(_, symbol, variants),
            ) => {
                let result: Vec<_> = enums
                    .iter()
                    .filter_map(|(name, inner)| {
                        if name == &symbol.to_string() {
                            match (inner, variants) {
                                (None, None) => None,
                                (Some(inner_spaces), Some(variant_patterns)) => {
                                    let subtracted: Vec<_> = inner_spaces
                                        .iter()
                                        .zip(variant_patterns)
                                        .map(|(space, pattern)| space.substract(pattern))
                                        .collect();
                                    if !subtracted.is_empty() {
                                        Some((name.clone(), Some(subtracted)))
                                    } else {
                                        None
                                    }
                                }
                                (Some(_), None) | (None, Some(_)) => unreachable!(),
                            }
                        } else {
                            Some((name.clone(), inner.clone()))
                        }
                    })
                    .collect();
                PatternSpace::Finite(FinitePatternSpace::Enum(result))
            }
            (
                PatternSpace::Infinite(InfinitePatternSpace::String(strings), full),
                Pattern::String(_, o_string),
            ) => {
                if !strings.contains(&o_string.to_string()) && !full {
                    let mut new_strings = strings.to_vec();
                    new_strings.push(o_string.to_string());
                    PatternSpace::Infinite(InfinitePatternSpace::String(new_strings), *full)
                } else {
                    PatternSpace::Infinite(InfinitePatternSpace::String(strings.to_vec()), *full)
                }
            }
            (
                PatternSpace::Infinite(InfinitePatternSpace::Number(numbers), full),
                Pattern::Number(_, o_number),
            ) => {
                if !numbers.contains(o_number) && !full {
                    let mut new_numbers = numbers.to_vec();
                    new_numbers.push(o_number.clone());
                    PatternSpace::Infinite(InfinitePatternSpace::Number(new_numbers), *full)
                } else {
                    PatternSpace::Infinite(InfinitePatternSpace::Number(numbers.to_vec()), *full)
                }
            }
            (p1, p2) => {
                panic!("Cannot substract {:?} from {:?}", p2, p1);
            }
        }
    }

    fn union(self, other: &Self) -> Self {
        match (self, other) {
            (p, PatternSpace::Any) => p,
            (PatternSpace::Any, p) => p.clone(),
            (PatternSpace::Contained(ps, b), PatternSpace::Contained(os, _)) => {
                let mut new_ps = Vec::new();
                for (o, p) in os.into_iter().zip(ps.iter()) {
                    new_ps.push(p.clone().union(o));
                }

                PatternSpace::Contained(new_ps, b)
            }
            (
                PatternSpace::Infinite(InfinitePatternSpace::Number(ns), full1),
                PatternSpace::Infinite(InfinitePatternSpace::Number(ns2), full2),
            ) => {
                let mut ns = ns.into_iter().chain(ns2.clone()).collect::<Vec<_>>();
                ns.sort();
                ns.dedup();
                PatternSpace::Infinite(InfinitePatternSpace::Number(ns), full1 || *full2)
            }
            (
                PatternSpace::Infinite(InfinitePatternSpace::String(ss), full1),
                PatternSpace::Infinite(InfinitePatternSpace::String(ss2), full2),
            ) => {
                let mut ss = ss.into_iter().chain(ss2.clone()).collect::<Vec<_>>();
                ss.sort();
                ss.dedup();
                PatternSpace::Infinite(InfinitePatternSpace::String(ss), full1 || *full2)
            }
            (s, _) => s.clone(), // enums are already merged
        }
    }

    fn all_covered(&self) -> bool {
        match self {
            PatternSpace::Any => false,
            PatternSpace::Infinite(_, covered) => *covered,
            PatternSpace::Finite(FinitePatternSpace::Enum(variants)) => {
                variants.is_empty()
                    || variants.iter().all(|(_, inner_space)| {
                        inner_space.as_ref().map_or(true, |spaces| {
                            spaces.iter().all(|space| space.all_covered())
                        })
                    })
            }
            PatternSpace::Contained(items, _) => items.iter().all(|item| item.all_covered()),
            _ => unimplemented!(),
        }
    }

    fn cover_all_space(&self) -> PatternSpace {
        match self {
            PatternSpace::Infinite(infinite, _) => PatternSpace::Infinite(infinite.clone(), true),

            PatternSpace::Finite(FinitePatternSpace::Enum(variants)) => {
                let mut new_variants = Vec::new();
                for (name, inner_space) in variants {
                    let covered_inner_space = inner_space.as_ref().map(|spaces| {
                        spaces
                            .into_iter()
                            .map(|space| space.cover_all_space())
                            .collect()
                    });
                    new_variants.push((name.clone(), covered_inner_space));
                }
                PatternSpace::Finite(FinitePatternSpace::Enum(new_variants))
            }
            PatternSpace::Contained(spaces, b) => {
                let processed_spaces: Vec<PatternSpace> =
                    spaces.iter().map(|space| space.cover_all_space()).collect();
                PatternSpace::Contained(processed_spaces, *b)
            }
            _ => self.clone(),
        }
    }
}

pub fn analyze_match_patterns(
    patterns: &[Pattern],
    enums: &HashMap<&str, Vec<(&str, Option<Vec<Type>>)>>,
) -> MatchAnalysisReport {
    let mut redundant_patterns = Vec::new();
    let mut needed_patterns = Vec::new();

    let mut covered_space = compute_covered_space(patterns, enums);
    for (pattern_index, pattern) in patterns.iter().enumerate() {
        let substracted_space = covered_space.substract(pattern);
        if substracted_space == covered_space {
            redundant_patterns.push(pattern_index);
        } else {
            needed_patterns.push(pattern);
            covered_space = substracted_space;
        }
    }

    MatchAnalysisReport {
        is_exhaustive: covered_space.all_covered(),
        redundant_patterns,
    }
}

fn compute_covered_space(
    patterns: &[Pattern],
    enums: &HashMap<&str, Vec<(&str, Option<Vec<Type>>)>>,
) -> PatternSpace {
    patterns
        .iter()
        .map(|pattern| create_pattern_space(pattern))
        .flat_map(|processed| expand_pattern_space(processed, enums))
        .fold(PatternSpace::Any, |acc, space| acc.union(&space))
}

fn create_pattern_space(pattern: &Pattern) -> PatternSpace {
    match pattern {
        Pattern::CatchAll(_) => PatternSpace::Any,
        Pattern::Ellipsis(_) => unreachable!(),
        Pattern::Number(_, _) => {
            PatternSpace::Infinite(InfinitePatternSpace::Number(vec![]), false)
        }
        Pattern::String(_, _) => {
            PatternSpace::Infinite(InfinitePatternSpace::String(vec![]), false)
        }
        Pattern::Variable(_, _) => PatternSpace::Any,
        Pattern::Tuple(_, p) => {
            let inner_space = p.iter().map(|p| create_pattern_space(p)).collect();
            PatternSpace::Contained(inner_space, false)
        }
        Pattern::Array(_, p) => {
            let inner_space = p.iter().map(|p| create_pattern_space(p)).collect();
            PatternSpace::Contained(inner_space, false)
        }
        Pattern::Enum(_, name, fields) => {
            let inner_space = fields
                .as_ref()
                .map(|patterns| patterns.iter().map(|p| create_pattern_space(p)).collect());
            PatternSpace::Finite(FinitePatternSpace::Enum(vec![(
                name.to_string(),
                inner_space,
            )]))
        }
    }
}

fn expand_pattern_space(
    pattern: PatternSpace,
    enums: &HashMap<&str, Vec<(&str, Option<Vec<Type>>)>>,
) -> Vec<PatternSpace> {
    let vec = match pattern {
        PatternSpace::Contained(inner, _) => {
            let mut expanded_enums = Vec::new();
            for inner_space in inner {
                let expanded = expand_pattern_space(inner_space, enums);
                expanded_enums.push(expanded);
            }
            let product = cartesian_product(expanded_enums);

            product
                .iter()
                .map(|p| PatternSpace::Contained(p.to_vec(), false))
                .collect()
        }
        PatternSpace::Finite(FinitePatternSpace::Enum(variants)) => {
            let (enum_name, _) = variants[0].0.rsplit_once("::").unwrap();
            let expanded_variants = enums
                .get(enum_name)
                .unwrap()
                .iter()
                .map(|(variant_name, types)| {
                    let processed_ty = process_variant_type(types, enums);
                    match processed_ty {
                        Some(spaces) => {
                            let expanded = spaces
                                .into_iter()
                                .map(|space| expand_pattern_space(space, enums))
                                .flatten()
                                .collect::<Vec<_>>();
                            (format!("{}::{}", enum_name, variant_name), Some(expanded))
                        }
                        None => (format!("{}::{}", enum_name, variant_name), None),
                    }
                })
                .collect();

            vec![PatternSpace::Finite(FinitePatternSpace::Enum(
                expanded_variants,
            ))]
        }
        PatternSpace::Finite(FinitePatternSpace::Bool(_)) => {
            unreachable!("did you say bool patterns? really?")
        }
        PatternSpace::Infinite(_, _) | PatternSpace::Any => vec![pattern],
    };

    vec
}

fn process_variant_type(
    variant: &Option<Vec<Type>>,
    enums: &HashMap<&str, Vec<(&str, Option<Vec<Type>>)>>,
) -> Option<Vec<PatternSpace>> {
    match variant {
        None => None,
        Some(types) => {
            let mut pattern_space = Vec::with_capacity(types.len());
            for ty in types {
                let new_pattern = match ty {
                    Type::Bottom | Type::Col | Type::Expr | Type::Function(_) | Type::Inter => {
                        unreachable!()
                    }
                    Type::Int | Type::Fe => {
                        PatternSpace::Infinite(InfinitePatternSpace::Number(vec![]), false)
                    }
                    Type::Bool => PatternSpace::Finite(FinitePatternSpace::Bool(None)),
                    Type::String => {
                        PatternSpace::Infinite(InfinitePatternSpace::String(vec![]), false)
                    }
                    Type::Array(ArrayType { base, length }) => {
                        let items = match length {
                            Some(length) => {
                                vec![base.as_ref().clone(); *length as usize]
                            }
                            None => {
                                vec![base.as_ref().clone()]
                            }
                        };

                        let expanded = process_variant_type(&Some(items), enums)?;
                        PatternSpace::Contained(expanded, false)
                    }
                    Type::Tuple(TupleType { items }) => {
                        let expanded = process_variant_type(&Some(items.to_vec()), enums)?;
                        PatternSpace::Contained(expanded, false)
                    }
                    Type::TypeVar(_) => PatternSpace::Any,
                    Type::NamedType(name, vars) => {
                        let single_enum = match vars {
                            None => PatternSpace::Finite(FinitePatternSpace::Enum(vec![(
                                name.to_string(),
                                None,
                            )])),
                            Some(_) => {
                                let new_vars = process_variant_type(vars, enums);
                                PatternSpace::Finite(FinitePatternSpace::Enum(vec![(
                                    name.to_string(),
                                    new_vars,
                                )]))
                            }
                        };

                        let expanded = expand_pattern_space(single_enum, enums);
                        expanded[0].clone()
                    }
                };

                pattern_space.push(new_pattern);
            }

            Some(pattern_space)
        }
    }
}

fn cartesian_product(patterns: Vec<Vec<PatternSpace>>) -> Vec<Vec<PatternSpace>> {
    patterns.into_iter().fold(vec![vec![]], |acc, patterns| {
        acc.into_iter()
            .flat_map(|v| {
                patterns.iter().map(move |p| {
                    let mut new_v = v.clone();
                    new_v.push(p.clone());
                    new_v
                })
            })
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use powdr_parser_util::SourceRef;

    fn dummy_sr() -> SourceRef {
        SourceRef::unknown()
    }

    #[test]
    fn test_basic_match_analysis() {
        let patterns = vec![
            Pattern::String(dummy_sr(), "A".to_string()),
            Pattern::String(dummy_sr(), "B".to_string()),
        ];
        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }

    #[test]
    fn test_match_analysis_repeated_pattern() {
        let patterns = vec![
            Pattern::String(dummy_sr(), "A".to_string()),
            Pattern::String(dummy_sr(), "A".to_string()),
            Pattern::String(dummy_sr(), "A".to_string()),
        ];

        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns, vec![1, 2]);
    }

    #[test]
    fn test_match_analysis_repeated_pattern_mixed() {
        let patterns = vec![
            Pattern::String(SourceRef::unknown(), "A".to_string()),
            Pattern::String(SourceRef::unknown(), "B".to_string()),
            Pattern::String(SourceRef::unknown(), "B".to_string()),
        ];
        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns, vec![2]);
    }

    #[test]
    fn test_match_analysis_exhaustive_patterns() {
        let patterns = vec![
            Pattern::String(SourceRef::unknown(), "A".to_string()),
            Pattern::String(SourceRef::unknown(), "A".to_string()),
            Pattern::CatchAll(SourceRef::unknown()),
        ];
        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, true);
        assert_eq!(report.redundant_patterns, vec![1]);
    }

    #[test]
    fn test_match_analysis_tuples() {
        let patterns = vec![
            Pattern::Tuple(
                SourceRef::unknown(),
                vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 2.into()),
                ],
            ),
            Pattern::Tuple(
                SourceRef::unknown(),
                vec![
                    Pattern::Number(SourceRef::unknown(), 9.into()),
                    Pattern::Number(SourceRef::unknown(), 8.into()),
                ],
            ),
        ];
        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }

    #[test]
    fn test_match_analysis_tuples_partial_catchall() {
        let patterns = vec![
            Pattern::Tuple(
                SourceRef::unknown(),
                vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 3.into()),
                ],
            ),
            Pattern::Tuple(
                SourceRef::unknown(),
                vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::CatchAll(SourceRef::unknown()),
                ],
            ),
            Pattern::Tuple(
                SourceRef::unknown(),
                vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 4.into()),
                ],
            ),
        ];
        let enums = HashMap::new();
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns, vec![2]);
    }

    #[test]
    fn test_match_analysis_basic_enums() {
        let patterns = vec![
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("X".to_string()),
                ]),
                Some(vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 2.into()),
                ]),
            ),
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("Y".to_string()),
                ]),
                Some(vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 2.into()),
                ]),
            ),
        ];
        let enums = {
            let mut map = HashMap::new();
            map.insert(
                "A",
                vec![
                    ("X", Some(vec![Type::Int, Type::Int])),
                    ("Y", Some(vec![Type::Int, Type::Int])),
                ],
            );
            map
        };
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }

    #[test]
    fn test_match_analysis_basic_enums_catchall() {
        let patterns = vec![
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("X".to_string()),
                ]),
                Some(vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 2.into()),
                ]),
            ),
            Pattern::CatchAll(SourceRef::unknown()),
        ];
        let enums = {
            let mut map = HashMap::new();
            map.insert(
                "A",
                vec![
                    ("X", Some(vec![Type::Int, Type::Int])),
                    ("Y", Some(vec![Type::Int, Type::Int])),
                ],
            );
            map
        };
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, true);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }

    #[test]
    fn test_match_analysis_no_variants() {
        let patterns = vec![
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("X".to_string()),
                ]),
                None,
            ),
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("Y".to_string()),
                ]),
                Some(vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 2.into()),
                ]),
            ),
        ];
        let enums = {
            let mut map = HashMap::new();
            map.insert(
                "A",
                vec![("X", None), ("Y", Some(vec![Type::Int, Type::Int]))],
            );
            map
        };
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }

    #[test]
    fn test_match_analysis_enums_inner_catchall() {
        let patterns = vec![
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("X".to_string()),
                ]),
                Some(vec![
                    Pattern::CatchAll(SourceRef::unknown()),
                    Pattern::CatchAll(SourceRef::unknown()),
                ]),
            ),
            Pattern::Enum(
                SourceRef::unknown(),
                SymbolPath::from_parts(vec![
                    Part::Named("A".to_string()),
                    Part::Named("X".to_string()),
                ]),
                Some(vec![
                    Pattern::Number(SourceRef::unknown(), 1.into()),
                    Pattern::Number(SourceRef::unknown(), 3.into()),
                ]),
            ),
        ];
        let enums = {
            let mut map = HashMap::new();
            map.insert("A", vec![("X", Some(vec![Type::Int, Type::Int]))]);
            map
        };
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, true);
        assert_eq!(report.redundant_patterns, vec![1]);
    }

    #[test]
    fn test_usefullness_tuples_and_enums() {
        // ((_, None), _)
        // (_, (_, None))
        // (('l_short', Some('l_last')), ('r_short', Some('r_last'))
        let elem1 = Pattern::Tuple(
            SourceRef::unknown(),
            vec![
                Pattern::CatchAll(SourceRef::unknown()),
                Pattern::Enum(
                    SourceRef::unknown(),
                    SymbolPath::from_parts(vec![
                        Part::Named("Option".to_string()),
                        Part::Named("None".to_string()),
                    ]),
                    None,
                ),
            ],
        );
        let elem2 = Pattern::CatchAll(SourceRef::unknown());
        let arm1 = Pattern::Tuple(SourceRef::unknown(), vec![elem1, elem2]);

        let elem1 = Pattern::CatchAll(SourceRef::unknown());
        let elem2 = Pattern::Tuple(
            SourceRef::unknown(),
            vec![
                Pattern::CatchAll(SourceRef::unknown()),
                Pattern::Enum(
                    SourceRef::unknown(),
                    SymbolPath::from_parts(vec![
                        Part::Named("Option".to_string()),
                        Part::Named("None".to_string()),
                    ]),
                    None,
                ),
            ],
        );
        let arm2 = Pattern::Tuple(SourceRef::unknown(), vec![elem1, elem2]);

        let elem1 = Pattern::Tuple(
            SourceRef::unknown(),
            vec![
                Pattern::Variable(SourceRef::unknown(), "l_short".to_string()),
                Pattern::Enum(
                    SourceRef::unknown(),
                    SymbolPath::from_parts(vec![
                        Part::Named("Option".to_string()),
                        Part::Named("Some".to_string()),
                    ]),
                    Some(vec![Pattern::Variable(
                        SourceRef::unknown(),
                        "l_last".to_string(),
                    )]),
                ),
            ],
        );
        let elem2 = Pattern::Tuple(
            SourceRef::unknown(),
            vec![
                Pattern::Variable(SourceRef::unknown(), "r_short".to_string()),
                Pattern::Enum(
                    SourceRef::unknown(),
                    SymbolPath::from_parts(vec![
                        Part::Named("Option".to_string()),
                        Part::Named("Some".to_string()),
                    ]),
                    Some(vec![Pattern::Variable(
                        SourceRef::unknown(),
                        "r_last".to_string(),
                    )]),
                ),
            ],
        );
        let arm3 = Pattern::Tuple(SourceRef::unknown(), vec![elem1, elem2]);

        let patterns = vec![arm1, arm2, arm3];
        let mut enums = HashMap::new();
        enums.insert(
            "Option",
            vec![("None", None), ("Some", Some(vec![Type::String]))],
        );
        let report = analyze_match_patterns(&patterns, &enums);
        assert_eq!(report.is_exhaustive, false);
        assert_eq!(report.redundant_patterns.is_empty(), true);
    }
}
