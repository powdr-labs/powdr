use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    iter::empty,
};

use itertools::Itertools;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::{asm::SymbolPath, visitor::Children, Expression, Number};

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize, JsonSchema,
)]
pub enum Type<E = u64> {
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
    /// Intermediate column
    Inter,
    /// Algebraic expression
    Expr,
    Array(ArrayType<E>),
    Tuple(TupleType<E>),
    Function(FunctionType<E>),
    TypeVar(String),
    /// A named type like an enum, including generic arguments.
    /// Directly after parsing, type variables are also
    /// represented as NamedTypes, because the parser cannot distinguish.
    NamedType(SymbolPath, Option<Vec<Type<E>>>),
}

impl<E> Type<E> {
    /// Returns true if it is a non-complex type.
    /// Type variables are not considered elementary.
    pub fn is_elementary(&self) -> bool {
        match self {
            Type::Bottom
            | Type::Bool
            | Type::Int
            | Type::Fe
            | Type::String
            | Type::Col
            | Type::Inter
            | Type::Expr => true,
            Type::Array(_)
            | Type::Tuple(_)
            | Type::Function(_)
            | Type::TypeVar(_)
            | Type::NamedType(_, _) => false,
        }
    }
    /// Returns true if the type name needs parentheses during formatting
    /// when used inside a complex expression.
    pub fn needs_parentheses(&self) -> bool {
        match self {
            _ if self.is_elementary() => false,
            Type::Array(_) | Type::Tuple(_) | Type::TypeVar(_) | Type::NamedType(_, _) => false,
            Type::Function(_) => true,
            _ => unreachable!(),
        }
    }

    /// Turns all NamedTypes that are single identifiers in the given set
    /// to TypeVars. Also removes empty lists of generic args.
    pub fn map_to_type_vars(&mut self, type_vars: &HashSet<&String>) {
        match self {
            Type::NamedType(n, None) => {
                if let Some(identifier) = n.try_to_identifier() {
                    if type_vars.contains(identifier) {
                        *self = Type::TypeVar(identifier.clone());
                    }
                }
            }
            Type::NamedType(n, Some(tv)) if tv.is_empty() => {
                *self = Type::NamedType(std::mem::take(n), None)
            }
            _ => self
                .children_mut()
                .for_each(|t| t.map_to_type_vars(type_vars)),
        }
    }

    pub fn contained_named_types(&self) -> Box<dyn Iterator<Item = &SymbolPath> + '_> {
        let names = match self {
            Type::NamedType(n, _) => Some(n),
            _ => None,
        };
        Box::new(
            names
                .into_iter()
                .chain(self.children().flat_map(|t| t.contained_named_types())),
        )
    }

    pub fn contained_named_types_mut(&mut self) -> Box<dyn Iterator<Item = &mut SymbolPath> + '_> {
        match self {
            Type::NamedType(n, Some(args)) => Box::new(
                std::iter::once(n)
                    .chain(args.iter_mut().flat_map(|t| t.contained_named_types_mut())),
            ),
            Type::NamedType(n, None) => Box::new(std::iter::once(n)),
            _ => Box::new(
                self.children_mut()
                    .flat_map(|t| t.contained_named_types_mut()),
            ),
        }
    }

    pub fn is_concrete_type(&self) -> bool {
        self.contained_type_vars_with_repetitions().next().is_none()
    }

    pub fn contains_type_var(&self, name: &str) -> bool {
        self.contained_type_vars_with_repetitions()
            .any(|n| n == name)
    }

    /// Returns the list of contained type vars in order of first occurrence.
    pub fn contained_type_vars(&self) -> impl Iterator<Item = &String> {
        self.contained_type_vars_with_repetitions().unique()
    }

    pub fn empty_tuple() -> Type<E> {
        Type::Tuple(TupleType { items: vec![] })
    }
}

impl<E: ArrayLength> Type<E> {
    pub fn contained_expressions_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression> + '_> {
        match self {
            Type::Array(ArrayType { base, length }) => Box::new(
                length
                    .as_mut()
                    .and_then(|l| l.try_to_expression_mut())
                    .into_iter()
                    .chain(base.contained_expressions_mut()),
            ),
            t => Box::new(t.children_mut().flat_map(|t| t.contained_expressions_mut())),
        }
    }

    pub fn contained_expressions(&self) -> Box<dyn Iterator<Item = &Expression> + '_> {
        match self {
            Type::Array(ArrayType { base, length }) => Box::new(
                length
                    .as_ref()
                    .and_then(|l| l.try_to_expression())
                    .into_iter()
                    .chain(base.contained_expressions()),
            ),
            t => Box::new(t.children().flat_map(|t| t.contained_expressions())),
        }
    }
}

/// A trait to operate the possible types for the array type lengths
pub trait ArrayLength: std::fmt::Display + std::fmt::Debug {
    fn try_to_expression_mut(&mut self) -> Option<&mut Expression>;

    fn try_to_expression(&self) -> Option<&Expression>;
}

impl ArrayLength for Expression {
    fn try_to_expression_mut(&mut self) -> Option<&mut Expression> {
        Some(self)
    }

    fn try_to_expression(&self) -> Option<&Expression> {
        Some(self)
    }
}

impl ArrayLength for u64 {
    fn try_to_expression_mut(&mut self) -> Option<&mut Expression> {
        None
    }

    fn try_to_expression(&self) -> Option<&Expression> {
        None
    }
}

impl<E: Clone> Type<E> {
    /// Substitutes all occurrences of the given type variables with the given types.
    /// Does not apply the substitutions inside the replacements.
    pub fn substitute_type_vars(&mut self, substitutions: &HashMap<String, Type<E>>) {
        match self {
            Type::TypeVar(n) => {
                if let Some(t) = substitutions.get(n) {
                    *self = t.clone();
                }
            }
            _ => self
                .children_mut()
                .for_each(|t| t.substitute_type_vars(substitutions)),
        }
    }
}

impl<E> Type<E> {
    fn contained_type_vars_with_repetitions(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        match self {
            Type::TypeVar(n) => Box::new(std::iter::once(n)),
            _ => Box::new(
                self.children()
                    .flat_map(|t| t.contained_type_vars_with_repetitions()),
            ),
        }
    }
}

/// Returns iterators over all direct sub-types for this type.
impl<E> Children<Type<E>> for Type<E> {
    fn children(&self) -> Box<dyn Iterator<Item = &Type<E>> + '_> {
        match self {
            Type::Array(ar) => Box::new(std::iter::once(&*ar.base)),
            Type::Tuple(tu) => Box::new(tu.items.iter()),
            Type::Function(fun) => Box::new(fun.params.iter().chain(std::iter::once(&*fun.value))),
            Type::TypeVar(_) | Type::NamedType(_, None) => Box::new(std::iter::empty()),
            Type::NamedType(_, Some(args)) => Box::new(args.iter()),
            _ => {
                assert!(self.is_elementary());
                Box::new(std::iter::empty())
            }
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Type<E>> + '_> {
        match self {
            Type::Array(ar) => Box::new(std::iter::once(&mut *ar.base)),
            Type::Tuple(tu) => Box::new(tu.items.iter_mut()),
            Type::Function(fun) => Box::new(
                fun.params
                    .iter_mut()
                    .chain(std::iter::once(&mut *fun.value)),
            ),
            Type::TypeVar(_) | Type::NamedType(_, None) => Box::new(std::iter::empty()),
            Type::NamedType(_, Some(args)) => Box::new(args.iter_mut()),
            _ => {
                assert!(self.is_elementary());
                Box::new(std::iter::empty())
            }
        }
    }
}

/// Returns iterators over all direct expressions inside this type.
impl<R> Children<Expression<R>> for Type<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        match self {
            _ if self.is_elementary() => Box::new(empty()),
            Type::TypeVar(_) | Type::NamedType(_, None) => Box::new(empty()),
            Type::Array(a) => a.children(),
            Type::Tuple(t) => t.children(),
            Type::Function(f) => f.children(),
            Type::NamedType(_, Some(args)) => Box::new(args.iter().flat_map(|arg| arg.children())),
            _ => unreachable!(),
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        match self {
            _ if self.is_elementary() => Box::new(empty()),
            Type::TypeVar(_) | Type::NamedType(_, None) => Box::new(empty()),
            Type::Array(a) => a.children_mut(),
            Type::Tuple(t) => t.children_mut(),
            Type::Function(f) => f.children_mut(),
            Type::NamedType(_, Some(args)) => {
                Box::new(args.iter_mut().flat_map(|arg| arg.children_mut()))
            }
            _ => unreachable!(),
        }
    }
}

impl<R> Children<Expression<R>> for Type<u64> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(empty())
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(empty())
    }
}

impl<R: Display> From<Type<Expression<R>>> for Type<u64> {
    fn from(value: Type<Expression<R>>) -> Self {
        match value {
            Type::Bottom => Type::Bottom,
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::Fe => Type::Fe,
            Type::String => Type::String,
            Type::Col => Type::Col,
            Type::Inter => Type::Inter,
            Type::Expr => Type::Expr,
            Type::Array(a) => Type::Array(a.into()),
            Type::Tuple(t) => Type::Tuple(t.into()),
            Type::Function(f) => Type::Function(f.into()),
            Type::TypeVar(n) => Type::TypeVar(n),
            Type::NamedType(n, None) => Type::NamedType(n, None),
            Type::NamedType(n, Some(args)) => {
                Type::NamedType(n, Some(args.into_iter().map(|a| a.into()).collect()))
            }
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize, JsonSchema,
)]
pub struct ArrayType<E = u64> {
    pub base: Box<Type<E>>,
    pub length: Option<E>,
}

impl<R: Display> From<ArrayType<Expression<R>>> for ArrayType<u64> {
    fn from(value: ArrayType<Expression<R>>) -> Self {
        let length = value.length.as_ref().map(|l| {
            if let Expression::Number(_, Number {value: n, type_: ty}) = l {
                assert!(ty.is_none(), "Literal inside type name has assigned type. This should be done during analysis on the types instead.");
                n.try_into().expect("Array length expression too large.")
            } else {
                panic!(
                    "Array length expression not resolved in type name prior to conversion: {value}"
                );
            }
        });
        ArrayType {
            base: Box::new(Type::from(*value.base)),
            length,
        }
    }
}

impl<R> Children<Expression<R>> for ArrayType<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(self.base.children().chain(self.length.as_ref()))
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(self.base.children_mut().chain(self.length.as_mut()))
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize, JsonSchema,
)]
pub struct TupleType<E = u64> {
    pub items: Vec<Type<E>>,
}

impl<R> Children<Expression<R>> for TupleType<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(self.items.iter().flat_map(|t| t.children()))
    }
    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(self.items.iter_mut().flat_map(|t| t.children_mut()))
    }
}

impl<R: Display> From<TupleType<Expression<R>>> for TupleType<u64> {
    fn from(value: TupleType<Expression<R>>) -> Self {
        TupleType {
            items: value.items.into_iter().map(|t| t.into()).collect(),
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize, JsonSchema,
)]
pub struct FunctionType<E = u64> {
    pub params: Vec<Type<E>>,
    pub value: Box<Type<E>>,
}

impl<R> Children<Expression<R>> for FunctionType<Expression<R>> {
    fn children(&self) -> Box<dyn Iterator<Item = &Expression<R>> + '_> {
        Box::new(
            self.params
                .iter()
                .flat_map(|t| t.children())
                .chain(self.value.children()),
        )
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Expression<R>> + '_> {
        Box::new(
            self.params
                .iter_mut()
                .flat_map(|t| t.children_mut())
                .chain(self.value.children_mut()),
        )
    }
}

impl<R: Display> From<FunctionType<Expression<R>>> for FunctionType<u64> {
    fn from(value: FunctionType<Expression<R>>) -> Self {
        FunctionType {
            params: value.params.into_iter().map(|t| t.into()).collect(),
            value: Box::new((*value.value).into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TypeScheme<E = u64> {
    /// Type variables and their trait bounds.
    pub vars: TypeBounds,
    /// The actual type (using the type variables from `vars` but potentially also other type variables)
    pub ty: Type<E>,
}

impl<E: Clone> TypeScheme<E> {
    /// Returns a new type scheme with type variables renamed to `T1`, `T2`, ...
    /// (or just `T` if it is a single type variable).
    pub fn simplify_type_vars(self) -> TypeScheme<E> {
        let name_substitutions: HashMap<_, _> = match self.vars.len() {
            0 => return self,
            1 => {
                let var = self.vars.vars().next().unwrap();
                [(var.clone(), "T".to_string())].into()
            }
            _ => self
                .vars
                .vars()
                .enumerate()
                .map(|(i, v)| ((*v).clone(), format!("T{}", i + 1)))
                .collect(),
        };
        assert!(name_substitutions.len() == self.vars.len());
        let mut ty = self.ty;
        ty.substitute_type_vars(
            &name_substitutions
                .iter()
                .map(|(n, s)| (n.clone(), Type::TypeVar(s.clone())))
                .collect(),
        );
        TypeScheme {
            vars: TypeBounds::new(
                self.vars
                    .bounds()
                    .map(|(v, b)| (name_substitutions[v].clone(), b.clone())),
            ),
            ty,
        }
    }
}
impl<E> TypeScheme<E> {
    pub fn type_vars_to_string(&self) -> String {
        if self.vars.is_empty() {
            String::new()
        } else {
            format!("<{}>", self.vars)
        }
    }
}

impl From<Type> for TypeScheme {
    fn from(value: Type) -> Self {
        TypeScheme {
            vars: Default::default(),
            ty: value,
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

    pub fn format_vars_with_nonempty_bounds(&self) -> String {
        self.0
            .iter()
            .filter(|(_, b)| !b.is_empty())
            .map(|(var, b)| Self::format_var_bound(var, b))
            .join(", ")
    }

    pub fn format_var_bound(var: &String, bounds: &BTreeSet<String>) -> String {
        if bounds.is_empty() {
            var.clone()
        } else {
            format!("{var}: {}", bounds.iter().join(" + "))
        }
    }
}
