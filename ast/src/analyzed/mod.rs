mod display;
pub mod util;

use core::hash::Hash;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::ops::ControlFlow;

use number::DegreeType;

use crate::analyzed::util::previsit_expressions_in_pil_file_mut;
pub use crate::parsed::BinaryOperator;
pub use crate::parsed::UnaryOperator;

use util::expr_any;

#[derive(Debug)]
pub enum StatementIdentifier {
    Definition(String),
    PublicDeclaration(String),
    Identity(usize),
}

#[derive(Debug)]
pub struct Analyzed<T> {
    /// Constants are not namespaced!
    pub constants: HashMap<String, T>,
    pub definitions: HashMap<String, (Polynomial, Option<FunctionValueDefinition<T>>)>,
    pub public_declarations: HashMap<String, PublicDeclaration>,
    pub identities: Vec<Identity<T>>,
    /// The order in which definitions and identities
    /// appear in the source.
    pub source_order: Vec<StatementIdentifier>,
}

impl<T> Analyzed<T> {
    /// @returns the number of committed polynomials (with multiplicities for arrays)
    pub fn commitment_count(&self) -> usize {
        self.declaration_type_count(PolynomialType::Committed)
    }
    /// @returns the number of intermediate polynomials (with multiplicities for arrays)
    pub fn intermediate_count(&self) -> usize {
        self.declaration_type_count(PolynomialType::Intermediate)
    }
    /// @returns the number of constant polynomials (with multiplicities for arrays)
    pub fn constant_count(&self) -> usize {
        self.declaration_type_count(PolynomialType::Constant)
    }

    pub fn constant_polys_in_source_order(
        &self,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)> {
        self.definitions_in_source_order(PolynomialType::Constant)
    }

    pub fn committed_polys_in_source_order(
        &self,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)> {
        self.definitions_in_source_order(PolynomialType::Committed)
    }

    pub fn definitions_in_source_order(
        &self,
        poly_type: PolynomialType,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)> {
        self.source_order
            .iter()
            .filter_map(move |statement| {
                if let StatementIdentifier::Definition(name) = statement {
                    let definition = &self.definitions[name];
                    if definition.0.poly_type == poly_type {
                        return Some(definition);
                    }
                }
                None
            })
            .collect()
    }

    fn declaration_type_count(&self, poly_type: PolynomialType) -> usize {
        self.definitions
            .iter()
            .filter_map(move |(_name, (poly, _))| {
                if poly.poly_type == poly_type {
                    Some(poly.length.unwrap_or(1) as usize)
                } else {
                    None
                }
            })
            .sum()
    }

    /// Removes the specified polynomials and updates the IDs of the other polynomials
    /// so that they are contiguous again.
    /// There must not be any reference to the removed polynomials left.
    pub fn remove_polynomials(&mut self, to_remove: &BTreeSet<PolyID>) {
        // TODO intermediate polys
        let replacements: BTreeMap<PolyID, PolyID> = [
            // We have to do it separately because we need to re-start the counter
            // for each kind.
            self.committed_polys_in_source_order(),
            self.constant_polys_in_source_order(),
        ]
        .map(|polys| {
            polys
                .iter()
                .enumerate()
                .fold(
                    (0, BTreeMap::new()),
                    |(shift, mut replacements), (i, (poly, _def))| {
                        assert_eq!(i as u64, poly.id);
                        let poly_id = poly.into();
                        if to_remove.contains(&poly_id) {
                            (shift + 1, replacements)
                        } else {
                            replacements.insert(
                                poly_id,
                                PolyID {
                                    id: poly_id.id - shift,
                                    ..poly_id
                                },
                            );
                            (shift, replacements)
                        }
                    },
                )
                .1
        })
        .into_iter()
        .flatten()
        .collect();

        let mut names_to_remove: HashSet<String> = Default::default();
        self.definitions.retain(|name, (poly, _def)| {
            if to_remove.contains(&(poly as &Polynomial).into()) {
                names_to_remove.insert(name.clone());
                false
            } else {
                true
            }
        });
        self.source_order.retain(|s| {
            if let StatementIdentifier::Definition(name) = s {
                if names_to_remove.contains(name) {
                    return true;
                }
            }
            false
        });
        self.definitions.values_mut().for_each(|(poly, _def)| {
            let poly_id = PolyID::from(poly as &Polynomial);
            assert!(!to_remove.contains(&poly_id));
            poly.id = replacements[&poly_id].id;
        });
        previsit_expressions_in_pil_file_mut(self, &mut |expr| {
            if let Expression::PolynomialReference(poly) = expr {
                let poly_id = poly.poly_id.unwrap();
                assert!(!to_remove.contains(&poly_id));
                poly.poly_id = Some(replacements[&poly_id]);
            }
            ControlFlow::Continue::<()>(())
        });
    }

    /// Adds a polynomial identity and returns the ID.
    pub fn append_polynomial_identity(
        &mut self,
        identity: Expression<T>,
        source: SourceRef,
    ) -> u64 {
        let id = self.identities.len() as u64;
        self.identities.push(Identity {
            id,
            kind: IdentityKind::Polynomial,
            source,
            left: SelectedExpressions {
                selector: Some(identity),
                expressions: vec![],
            },
            right: SelectedExpressions {
                selector: None,
                expressions: vec![],
            },
        });
        self.source_order
            .push(StatementIdentifier::Identity(id as usize));
        id
    }
}

#[derive(Debug, Clone)]
pub struct Polynomial {
    pub id: u64,
    pub source: SourceRef,
    pub absolute_name: String,
    pub poly_type: PolynomialType,
    pub degree: DegreeType,
    pub length: Option<DegreeType>,
}

impl Polynomial {
    pub fn is_array(&self) -> bool {
        self.length.is_some()
    }
}

#[derive(Debug)]
pub enum FunctionValueDefinition<T> {
    Mapping(Expression<T>),
    Array(Vec<RepeatedArray<T>>),
    Query(Expression<T>),
}

/// An array of elements that might be repeated (the whole list is repeated).
#[derive(Debug)]
pub struct RepeatedArray<T> {
    pub values: Vec<Expression<T>>,
    pub repetitions: DegreeType,
}

impl<T> RepeatedArray<T> {
    /// Returns the number of elements in this array (including repetitions).
    pub fn size(&self) -> DegreeType {
        if self.repetitions == 0 {
            assert!(self.values.is_empty());
        }
        if self.values.is_empty() {
            assert!(self.repetitions <= 1)
        }
        self.values.len() as DegreeType * self.repetitions
    }
}

#[derive(Debug)]
pub struct PublicDeclaration {
    pub id: u64,
    pub source: SourceRef,
    pub name: String,
    pub polynomial: PolynomialReference,
    /// The evaluation point of the polynomial, not the array index.
    pub index: DegreeType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identity<T> {
    /// The ID is specific to the kind.
    pub id: u64,
    pub kind: IdentityKind,
    pub source: SourceRef,
    /// For a simple polynomial identity, the selector contains
    /// the actual expression (see expression_for_poly_id).
    pub left: SelectedExpressions<T>,
    pub right: SelectedExpressions<T>,
}

impl<T> Identity<T> {
    /// Returns the expression in case this is a polynomial identity.
    pub fn expression_for_poly_id(&self) -> &Expression<T> {
        assert_eq!(self.kind, IdentityKind::Polynomial);
        self.left.selector.as_ref().unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum IdentityKind {
    Polynomial,
    Plookup,
    Permutation,
    Connect,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct SelectedExpressions<T> {
    pub selector: Option<Expression<T>>,
    pub expressions: Vec<Expression<T>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression<T> {
    Constant(String),
    PolynomialReference(PolynomialReference),
    LocalVariableReference(u64),
    PublicReference(String),
    Number(T),
    String(String),
    Tuple(Vec<Expression<T>>),
    BinaryOperation(Box<Expression<T>>, BinaryOperator, Box<Expression<T>>),
    UnaryOperation(UnaryOperator, Box<Expression<T>>),
    /// Call to a non-macro function (like a constant polynomial)
    FunctionCall(String, Vec<Expression<T>>),
    MatchExpression(Box<Expression<T>>, Vec<(Option<T>, Expression<T>)>),
}

impl<T> Expression<T> {
    /// @returns true if the expression contains a reference to a next value of a
    /// (witness or fixed) column
    pub fn contains_next_ref(&self) -> bool {
        expr_any(self, |e| match e {
            Expression::PolynomialReference(poly) => poly.next,
            _ => false,
        })
    }

    /// @returns true if the expression contains a reference to a next value of a witness column.
    pub fn contains_next_witness_ref(&self) -> bool {
        expr_any(self, |e| match e {
            Expression::PolynomialReference(poly) => poly.next && poly.is_witness(),
            _ => false,
        })
    }

    /// @returns true if the expression contains a reference to a witness column.
    pub fn contains_witness_ref(&self) -> bool {
        expr_any(self, |e| match e {
            Expression::PolynomialReference(poly) => poly.is_witness(),
            _ => false,
        })
    }
}

#[derive(Debug, Clone, Eq)]
pub struct PolynomialReference {
    /// Name of the polynomial - just for informational purposes.
    /// Comparisons are based on polynomial ID.
    pub name: String,
    /// Identifier for a polynomial reference.
    /// Optional because it is filled in in a second stage of analysis.
    /// TODO make this non-optional
    pub poly_id: Option<PolyID>,
    pub index: Option<u64>,
    pub next: bool,
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PolyID {
    pub id: u64,
    pub ptype: PolynomialType,
}

impl From<&Polynomial> for PolyID {
    fn from(poly: &Polynomial) -> Self {
        PolyID {
            id: poly.id,
            ptype: poly.poly_type,
        }
    }
}

impl PolynomialReference {
    #[inline]
    pub fn poly_id(&self) -> u64 {
        self.poly_id.unwrap().id
    }
    #[inline]
    pub fn is_witness(&self) -> bool {
        self.poly_id.unwrap().ptype == PolynomialType::Committed
    }
    #[inline]
    pub fn is_fixed(&self) -> bool {
        self.poly_id.unwrap().ptype == PolynomialType::Constant
    }
}

impl PartialOrd for PolynomialReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PolynomialReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // TODO for efficiency reasons, we should avoid the unwrap check here somehow.
        match self.poly_id.unwrap().cmp(&other.poly_id.unwrap()) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        assert!(self.index.is_none() && other.index.is_none());
        self.next.cmp(&other.next)
    }
}

impl PartialEq for PolynomialReference {
    fn eq(&self, other: &Self) -> bool {
        assert!(self.index.is_none() && other.index.is_none());
        // TODO for efficiency reasons, we should avoid the unwrap check here somehow.
        self.poly_id.unwrap() == other.poly_id.unwrap() && self.next == other.next
    }
}

impl Hash for PolynomialReference {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.poly_id.hash(state);
        self.index.hash(state);
        self.next.hash(state);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PolynomialType {
    Committed,
    Constant,
    Intermediate,
}

impl Display for PolynomialType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PolynomialType::Committed => "witness",
                PolynomialType::Constant => "fixed",
                PolynomialType::Intermediate => "intermediate",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRef {
    pub file: String, // TODO should maybe be a shared pointer
    pub line: usize,
}
