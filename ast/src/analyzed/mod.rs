mod display;
pub mod visitor;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::ops::{self, ControlFlow};

use powdr_number::{DegreeType, FieldElement};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::parsed::types::{ArrayType, Type, TypeScheme};
use crate::parsed::utils::expr_any;
use crate::parsed::visitor::ExpressionVisitable;
pub use crate::parsed::BinaryOperator;
pub use crate::parsed::UnaryOperator;
use crate::parsed::{self, SelectedExpressions};
use crate::SourceRef;

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, PartialEq, Eq)]
pub enum StatementIdentifier {
    /// Either an intermediate column or a definition.
    Definition(String),
    PublicDeclaration(String),
    /// Index into the vector of identities.
    Identity(usize),
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
pub struct Analyzed<T> {
    /// The degree of all namespaces, which must match. If there are no namespaces, then `None`.
    pub degree: Option<DegreeType>,
    pub definitions: HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    pub public_declarations: HashMap<String, PublicDeclaration>,
    pub intermediate_columns: HashMap<String, (Symbol, Vec<AlgebraicExpression<T>>)>,
    pub identities: Vec<Identity<AlgebraicExpression<T>>>,
    /// The order in which definitions and identities
    /// appear in the source.
    pub source_order: Vec<StatementIdentifier>,
}

impl<T> Analyzed<T> {
    /// @returns the degree if any. Panics if there is none.
    pub fn degree(&self) -> DegreeType {
        self.degree.unwrap()
    }
    /// @returns the number of committed polynomials (with multiplicities for arrays)
    pub fn commitment_count(&self) -> usize {
        self.declaration_type_count(PolynomialType::Committed)
    }
    /// @returns the number of intermediate polynomials (with multiplicities for arrays)
    pub fn intermediate_count(&self) -> usize {
        self.intermediate_columns
            .iter()
            .map(|(_, (sym, _))| sym.length.unwrap_or(1) as usize)
            .sum()
    }
    /// @returns the number of constant polynomials (with multiplicities for arrays)
    pub fn constant_count(&self) -> usize {
        self.declaration_type_count(PolynomialType::Constant)
    }

    pub fn constant_polys_in_source_order(
        &self,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)> {
        self.definitions_in_source_order(PolynomialType::Constant)
    }

    pub fn committed_polys_in_source_order(
        &self,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)> {
        self.definitions_in_source_order(PolynomialType::Committed)
    }

    pub fn intermediate_polys_in_source_order(
        &self,
    ) -> Vec<&(Symbol, Vec<AlgebraicExpression<T>>)> {
        self.source_order
            .iter()
            .filter_map(move |statement| {
                if let StatementIdentifier::Definition(name) = statement {
                    if let Some(definition) = self.intermediate_columns.get(name) {
                        return Some(definition);
                    }
                }
                None
            })
            .collect()
    }

    pub fn definitions_in_source_order(
        &self,
        poly_type: PolynomialType,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)> {
        assert!(
            poly_type != PolynomialType::Intermediate,
            "Use intermediate_polys_in_source_order to get intermediate polys."
        );
        self.source_order
            .iter()
            .filter_map(move |statement| {
                if let StatementIdentifier::Definition(name) = statement {
                    if let Some(definition) = self.definitions.get(name) {
                        match definition.0.kind {
                            SymbolKind::Poly(ptype) if ptype == poly_type => {
                                return Some(definition);
                            }
                            _ => {}
                        }
                    }
                }
                None
            })
            .collect()
    }

    pub fn public_declarations_in_source_order(&self) -> Vec<(&String, &PublicDeclaration)> {
        self.source_order
            .iter()
            .filter_map(move |statement| {
                if let StatementIdentifier::PublicDeclaration(name) = statement {
                    if let Some(public_declaration) = self.public_declarations.get(name) {
                        return Some((name, public_declaration));
                    }
                }
                None
            })
            .collect()
    }

    fn declaration_type_count(&self, poly_type: PolynomialType) -> usize {
        self.definitions
            .iter()
            .filter_map(move |(_name, (symbol, _))| match symbol.kind {
                SymbolKind::Poly(ptype) if ptype == poly_type => {
                    Some(symbol.length.unwrap_or(1) as usize)
                }
                _ => None,
            })
            .sum()
    }

    /// Returns the type (scheme) of a symbol with the given name.
    pub fn type_of_symbol(&self, name: &str) -> TypeScheme {
        let (sym, value) = &self.definitions[name];
        type_from_definition(sym, value).unwrap()
    }

    /// Removes the specified polynomials and updates the IDs of the other polynomials
    /// so that they are contiguous again.
    /// There must not be any reference to the removed polynomials left.
    /// Does not support arrays or array elements.
    pub fn remove_polynomials(&mut self, to_remove: &BTreeSet<PolyID>) {
        let mut replacements: BTreeMap<PolyID, PolyID> = [
            // We have to do it separately because we need to re-start the counter
            // for each kind.
            self.committed_polys_in_source_order(),
            self.constant_polys_in_source_order(),
        ]
        .map(|polys| {
            polys
                .iter()
                .fold(
                    (0, BTreeMap::new()),
                    |(shift, mut replacements), (poly, _def)| {
                        let poly_id = poly.into();
                        let length = poly.length.unwrap_or(1);
                        if to_remove.contains(&poly_id) {
                            (shift + length, replacements)
                        } else {
                            for (_name, id) in poly.array_elements() {
                                replacements.insert(
                                    id,
                                    PolyID {
                                        id: id.id - shift,
                                        ..id
                                    },
                                );
                            }
                            // we still need a replacement for zero sized array,
                            // but array_elements() won't return any items in this case
                            // TODO: handle this differently? https://github.com/powdr-labs/powdr/issues/1142
                            if let Some(0) = poly.length {
                                let poly_id = poly.into();
                                replacements.insert(
                                    poly_id,
                                    PolyID {
                                        id: poly.id - shift,
                                        ..poly_id
                                    },
                                );
                            }
                            (shift, replacements)
                        }
                    },
                )
                .1
        })
        .into_iter()
        .flatten()
        .collect();

        // We assume for now that intermediate columns are not removed.
        for (poly, _) in self.intermediate_columns.values() {
            poly.array_elements().for_each(|(_name, id)| {
                assert!(!to_remove.contains(&id));
                replacements.insert(id, id);
            });
        }

        let mut names_to_remove: HashSet<String> = Default::default();
        self.definitions.retain(|name, (poly, _def)| {
            if matches!(poly.kind, SymbolKind::Poly(_))
                && to_remove.contains(&(poly as &Symbol).into())
            {
                names_to_remove.insert(name.clone());
                false
            } else {
                true
            }
        });
        self.source_order.retain(|s| {
            if let StatementIdentifier::Definition(name) = s {
                if names_to_remove.contains(name) {
                    return false;
                }
            }
            true
        });
        self.definitions.values_mut().for_each(|(poly, _def)| {
            if matches!(poly.kind, SymbolKind::Poly(_)) {
                let poly_id = PolyID::from(poly as &Symbol);
                assert!(!to_remove.contains(&poly_id));
                poly.id = replacements[&poly_id].id;
            }
        });
        let visitor = &mut |expr: &mut Expression| {
            if let Expression::Reference(Reference::Poly(poly)) = expr {
                poly.poly_id = poly.poly_id.map(|poly_id| {
                    assert!(!to_remove.contains(&poly_id));
                    replacements[&poly_id]
                });
            }
        };
        self.post_visit_expressions_in_definitions_mut(visitor);
        let algebraic_visitor = &mut |expr: &mut AlgebraicExpression<_>| {
            if let AlgebraicExpression::Reference(poly) = expr {
                assert!(!to_remove.contains(&poly.poly_id));
                poly.poly_id = replacements[&poly.poly_id];
            }
        };
        self.post_visit_expressions_in_identities_mut(algebraic_visitor);
    }

    /// Adds a polynomial identity and returns the ID.
    pub fn append_polynomial_identity(
        &mut self,
        identity: AlgebraicExpression<T>,
        source: SourceRef,
    ) -> u64 {
        let id = self
            .identities
            .iter()
            .map(|identity| identity.id)
            .max()
            .unwrap_or_default()
            + 1;
        self.identities
            .push(Identity::from_polynomial_identity(id, source, identity));
        self.source_order
            .push(StatementIdentifier::Identity(self.identities.len() - 1));
        id
    }

    /// Remove some identities by their index (not their ID).
    /// Does not re-allocate IDs.
    pub fn remove_identities(&mut self, to_remove: &BTreeSet<usize>) {
        let mut shift = 0;
        self.source_order.retain_mut(|s| {
            if let StatementIdentifier::Identity(index) = s {
                if to_remove.contains(index) {
                    shift += 1;
                    return false;
                }
                *index -= shift;
            }
            true
        });
        let mut index = 0;
        self.identities.retain(|_| {
            let retain = !to_remove.contains(&index);
            index += 1;
            retain
        })
    }

    pub fn post_visit_expressions_in_identities_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut AlgebraicExpression<T>),
    {
        self.identities
            .iter_mut()
            .for_each(|i| i.post_visit_expressions_mut(f));
        self.intermediate_columns
            .values_mut()
            .for_each(|(_sym, value)| {
                value
                    .iter_mut()
                    .for_each(|v| v.post_visit_expressions_mut(f))
            });
    }

    pub fn post_visit_expressions_in_definitions_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Expression),
    {
        // TODO add public inputs if we change them to expressions at some point.
        self.definitions
            .values_mut()
            .for_each(|(_poly, definition)| match definition {
                Some(FunctionValueDefinition::Query(e)) => e.post_visit_expressions_mut(f),
                Some(FunctionValueDefinition::Array(elements)) => elements
                    .iter_mut()
                    .flat_map(|e| e.pattern.iter_mut())
                    .for_each(|e| e.post_visit_expressions_mut(f)),
                Some(FunctionValueDefinition::Expression(TypedExpression {
                    e,
                    type_scheme: _,
                })) => e.post_visit_expressions_mut(f),
                None => {}
            });
    }
}

impl<T: FieldElement> Analyzed<T> {
    /// @returns all identities with intermediate polynomials inlined.
    pub fn identities_with_inlined_intermediate_polynomials(
        &self,
    ) -> Vec<Identity<AlgebraicExpression<T>>> {
        let intermediates = &self
            .intermediate_polys_in_source_order()
            .iter()
            .flat_map(|(symbol, def)| {
                symbol
                    .array_elements()
                    .zip(def)
                    .map(|((_, poly_id), def)| (poly_id, def))
            })
            .collect();

        substitute_intermediate(self.identities.clone(), intermediates)
    }

    pub fn get_struct_schema() -> schemars::schema::RootSchema {
        schemars::schema_for!(Self)
    }

    pub fn serialize(&self) -> Result<Vec<u8>, String> {
        serde_cbor::to_vec(self).map_err(|e| format!("Failed to serialize analyzed: {}", e))
    }

    pub fn deserialize(bytes: &[u8]) -> Result<Self, String> {
        serde_cbor::from_slice(bytes).map_err(|e| format!("Failed to deserialize analyzed: {}", e))
    }
}

/// Takes identities as values and inlines intermediate polynomials everywhere, returning a vector of the updated identities
/// TODO: this could return an iterator
fn substitute_intermediate<T: Copy + Display>(
    identities: impl IntoIterator<Item = Identity<AlgebraicExpression<T>>>,
    intermediate_polynomials: &HashMap<PolyID, &AlgebraicExpression<T>>,
) -> Vec<Identity<AlgebraicExpression<T>>> {
    identities
        .into_iter()
        .scan(HashMap::default(), |cache, mut identity| {
            identity.post_visit_expressions_mut(&mut |e| {
                if let AlgebraicExpression::Reference(poly) = e {
                    match poly.poly_id.ptype {
                        PolynomialType::Committed => {}
                        PolynomialType::Constant => {}
                        PolynomialType::Intermediate => {
                            // recursively inline intermediate polynomials, updating the cache
                            *e = inlined_expression_from_intermediate_poly_id(
                                poly.clone(),
                                intermediate_polynomials,
                                cache,
                            );
                        }
                    }
                }
            });
            Some(identity)
        })
        .collect()
}

/// Recursively inlines intermediate polynomials inside an expression and returns the new expression
/// This uses a cache to avoid resolving an intermediate polynomial twice
///
/// poly_to_replace can be a "next" reference, but then its value cannot contain any next references.
fn inlined_expression_from_intermediate_poly_id<T: Copy + Display>(
    poly_to_replace: AlgebraicReference,
    intermediate_polynomials: &HashMap<PolyID, &AlgebraicExpression<T>>,
    cache: &mut HashMap<AlgebraicReference, AlgebraicExpression<T>>,
) -> AlgebraicExpression<T> {
    assert_eq!(poly_to_replace.poly_id.ptype, PolynomialType::Intermediate);
    if let Some(e) = cache.get(&poly_to_replace) {
        return e.clone();
    }
    let mut expr = intermediate_polynomials[&poly_to_replace.poly_id].clone();
    expr.post_visit_expressions_mut(&mut |e| {
        let AlgebraicExpression::Reference(r) = e else { return; };
        // "forward" the next operator from the polynomial to be replaced.
        if poly_to_replace.next && r.next {
            let value = intermediate_polynomials[&poly_to_replace.poly_id];
            panic!(
                "Error inlining intermediate polynomial {poly_to_replace} = ({value})':\nNext operator already applied to {} and then again to {} - cannot apply it twice!",
                r.name,
                poly_to_replace.name
            );
        }
        r.next = r.next || poly_to_replace.next;
        match r.poly_id.ptype {
            PolynomialType::Committed | PolynomialType::Constant => {}
            PolynomialType::Intermediate => {
                *e = inlined_expression_from_intermediate_poly_id(
                    r.clone(),
                    intermediate_polynomials,
                    cache,
                );
            }
        }
    });
    cache.insert(poly_to_replace, expr.clone());
    expr
}

/// Extracts the declared (or implicit) type from a definition.
pub fn type_from_definition(
    symbol: &Symbol,
    value: &Option<FunctionValueDefinition>,
) -> Option<TypeScheme> {
    if let Some(value) = value {
        match value {
            FunctionValueDefinition::Array(_) | FunctionValueDefinition::Query(_) => {
                Some(Type::Col.into())
            }
            FunctionValueDefinition::Expression(TypedExpression { e: _, type_scheme }) => {
                type_scheme.clone()
            }
        }
    } else {
        assert!(
            symbol.kind == SymbolKind::Poly(PolynomialType::Committed)
                || symbol.kind == SymbolKind::Poly(PolynomialType::Constant)
        );
        if symbol.length.is_some() {
            Some(
                Type::Array(ArrayType {
                    base: Box::new(Type::Col),
                    length: None,
                })
                .into(),
            )
        } else {
            Some(Type::Col.into())
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Symbol {
    pub id: u64,
    pub source: SourceRef,
    pub absolute_name: String,
    pub kind: SymbolKind,
    pub length: Option<DegreeType>,
}

impl Symbol {
    pub fn is_array(&self) -> bool {
        self.length.is_some()
    }
    /// Returns an iterator producing either just the symbol (if it is not an array),
    /// or all the elements of the array with their names in the form `array[index]`.
    pub fn array_elements(&self) -> impl Iterator<Item = (String, PolyID)> + '_ {
        let SymbolKind::Poly(ptype) = self.kind else {
            panic!("Expected polynomial.");
        };
        let length = self.length.unwrap_or(1);
        (0..length).map(move |i| {
            (
                self.array_element_name(i),
                PolyID {
                    id: self.id + i,
                    ptype,
                },
            )
        })
    }

    /// Returns "name[index]" if this is an array or just "name" otherwise.
    /// In the second case, requires index to be zero and otherwise
    /// requires index to be less than length.
    pub fn array_element_name(&self, index: u64) -> String {
        match self.length {
            Some(length) => {
                assert!(index < length);
                format!("{}[{index}]", self.absolute_name)
            }
            None => self.absolute_name.to_string(),
        }
    }

    /// Returns "name[length]" if this is an array or just "name" otherwise.
    pub fn array_name(&self) -> String {
        match self.length {
            Some(length) => {
                format!("{}[{length}]", self.absolute_name)
            }
            None => self.absolute_name.to_string(),
        }
    }
}

/// The "kind" of a symbol. In the future, this will be mostly
/// replaced by its type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub enum SymbolKind {
    /// Fixed, witness or intermediate polynomial
    Poly(PolynomialType),
    /// A constant value.
    Constant(),
    /// Other symbol, depends on the type.
    /// Examples include functions not of the type "int -> fe".
    Other(),
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub enum FunctionValueDefinition {
    Array(Vec<RepeatedArray>),
    Query(Expression),
    Expression(TypedExpression),
}

/// An array of elements that might be repeated.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct RepeatedArray {
    /// The pattern to be repeated
    pattern: Vec<Expression>,
    /// The number of values to be filled by repeating the pattern, possibly truncating it at the end
    size: DegreeType,
}

impl RepeatedArray {
    pub fn new(pattern: Vec<Expression>, size: DegreeType) -> Self {
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
    pub fn pattern(&self) -> &[Expression] {
        &self.pattern
    }

    /// Returns the pattern to be repeated
    pub fn pattern_mut(&mut self) -> &mut [Expression] {
        &mut self.pattern
    }

    /// Returns true iff this array is empty.
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Returns whether pattern needs to be repeated (or truncated) in order to match the size.
    pub fn is_repeated(&self) -> bool {
        self.size != self.pattern.len() as DegreeType
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct PublicDeclaration {
    pub id: u64,
    pub source: SourceRef,
    pub name: String,
    pub polynomial: PolynomialReference,
    pub array_index: Option<usize>,
    /// The evaluation point of the polynomial, not the array index.
    pub index: DegreeType,
}

impl PublicDeclaration {
    pub fn referenced_poly_name(&self) -> String {
        match self.array_index {
            Some(index) => format!("{}[{}]", self.polynomial.name, index),
            None => self.polynomial.name.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct IdentityId {
    id: u64,
    kind: IdentityKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Identity<Expr> {
    /// The ID is specific to the identity kind.
    pub id: u64,
    pub kind: IdentityKind,
    pub source: SourceRef,
    /// For a simple polynomial identity, the selector contains
    /// the actual expression (see expression_for_poly_id).
    pub left: SelectedExpressions<Expr>,
    pub right: SelectedExpressions<Expr>,
}

impl<Expr> Identity<Expr> {
    /// Constructs an Identity from a polynomial identity (expression assumed to be identical zero).
    pub fn from_polynomial_identity(id: u64, source: SourceRef, identity: Expr) -> Self {
        Identity {
            id,
            kind: IdentityKind::Polynomial,
            source,
            left: SelectedExpressions {
                selector: Some(identity),
                expressions: vec![],
            },
            right: Default::default(),
        }
    }
    /// Returns the expression in case this is a polynomial identity.
    pub fn expression_for_poly_id(&self) -> &Expr {
        assert_eq!(self.kind, IdentityKind::Polynomial);
        self.left.selector.as_ref().unwrap()
    }

    /// Returns the expression in case this is a polynomial identity.
    pub fn expression_for_poly_id_mut(&mut self) -> &mut Expr {
        assert_eq!(self.kind, IdentityKind::Polynomial);
        self.left.selector.as_mut().unwrap()
    }

    pub fn id(&self) -> IdentityId {
        IdentityId {
            id: self.id,
            kind: self.kind,
        }
    }
}

impl<T> Identity<AlgebraicExpression<T>> {
    pub fn contains_next_ref(&self) -> bool {
        self.left.contains_next_ref() || self.right.contains_next_ref()
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, JsonSchema,
)]
pub enum IdentityKind {
    Polynomial,
    Plookup,
    Permutation,
    Connect,
}

impl<T> SelectedExpressions<AlgebraicExpression<T>> {
    /// @returns true if the expression contains a reference to a next value of a
    /// (witness or fixed) column
    pub fn contains_next_ref(&self) -> bool {
        self.selector
            .iter()
            .chain(self.expressions.iter())
            .any(|e| e.contains_next_ref())
    }
}

pub type Expression = parsed::Expression<Reference>;
pub type TypedExpression = crate::parsed::TypedExpression<Reference, u64>;

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub enum Reference {
    LocalVar(u64, String),
    Poly(PolynomialReference),
}

#[derive(Debug, Clone, Eq, Serialize, Deserialize, JsonSchema)]
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum AlgebraicExpression<T> {
    Reference(AlgebraicReference),
    PublicReference(String),
    Number(T),
    BinaryOperation(
        Box<AlgebraicExpression<T>>,
        AlgebraicBinaryOperator,
        Box<AlgebraicExpression<T>>,
    ),

    UnaryOperation(AlgebraicUnaryOperator, Box<AlgebraicExpression<T>>),
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize, JsonSchema,
)]
pub enum AlgebraicBinaryOperator {
    Add,
    Sub,
    Mul,
    /// Exponentiation, but only by constants.
    Pow,
}

impl From<AlgebraicBinaryOperator> for BinaryOperator {
    fn from(op: AlgebraicBinaryOperator) -> BinaryOperator {
        match op {
            AlgebraicBinaryOperator::Add => BinaryOperator::Add,
            AlgebraicBinaryOperator::Sub => BinaryOperator::Sub,
            AlgebraicBinaryOperator::Mul => BinaryOperator::Mul,
            AlgebraicBinaryOperator::Pow => BinaryOperator::Pow,
        }
    }
}

impl TryFrom<BinaryOperator> for AlgebraicBinaryOperator {
    type Error = String;

    fn try_from(op: BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            BinaryOperator::Add => Ok(AlgebraicBinaryOperator::Add),
            BinaryOperator::Sub => Ok(AlgebraicBinaryOperator::Sub),
            BinaryOperator::Mul => Ok(AlgebraicBinaryOperator::Mul),
            BinaryOperator::Pow => Ok(AlgebraicBinaryOperator::Pow),
            _ => Err(format!(
                "Binary operator {op} not allowed in algebraic expression."
            )),
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize, JsonSchema,
)]
pub enum AlgebraicUnaryOperator {
    Minus,
}

impl From<AlgebraicUnaryOperator> for UnaryOperator {
    fn from(op: AlgebraicUnaryOperator) -> UnaryOperator {
        match op {
            AlgebraicUnaryOperator::Minus => UnaryOperator::Minus,
        }
    }
}

impl TryFrom<UnaryOperator> for AlgebraicUnaryOperator {
    type Error = String;

    fn try_from(op: UnaryOperator) -> Result<Self, Self::Error> {
        match op {
            UnaryOperator::Minus => Ok(AlgebraicUnaryOperator::Minus),
            _ => Err(format!(
                "Unary operator {op} not allowed in algebraic expression."
            )),
        }
    }
}

impl<T> AlgebraicExpression<T> {
    pub fn new_binary(left: Self, op: AlgebraicBinaryOperator, right: Self) -> Self {
        AlgebraicExpression::BinaryOperation(Box::new(left), op, Box::new(right))
    }

    /// @returns true if the expression contains a reference to a next value of a
    /// (witness or fixed) column
    pub fn contains_next_ref(&self) -> bool {
        expr_any(self, |e| match e {
            AlgebraicExpression::Reference(poly) => poly.next,
            _ => false,
        })
    }

    /// @returns true if the expression contains a reference to a next value of a witness column.
    pub fn contains_next_witness_ref(&self) -> bool {
        expr_any(self, |e| match e {
            AlgebraicExpression::Reference(poly) => poly.next && poly.is_witness(),
            _ => false,
        })
    }

    /// @returns true if the expression contains a reference to a witness column.
    pub fn contains_witness_ref(&self) -> bool {
        expr_any(self, |e| match e {
            AlgebraicExpression::Reference(poly) => poly.is_witness(),
            _ => false,
        })
    }
}

impl<T> ops::Add for AlgebraicExpression<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Add, rhs)
    }
}

impl<T> ops::Sub for AlgebraicExpression<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Sub, rhs)
    }
}

impl<T> ops::Mul for AlgebraicExpression<T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new_binary(self, AlgebraicBinaryOperator::Mul, rhs)
    }
}

impl<T> From<T> for AlgebraicExpression<T> {
    fn from(value: T) -> Self {
        AlgebraicExpression::Number(value)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct PolynomialReference {
    /// Name of the polynomial - just for informational purposes.
    /// Comparisons are based on polynomial ID.
    pub name: String,
    /// Identifier for a polynomial reference.
    /// Optional because it is filled in in a second stage of analysis.
    /// TODO make this non-optional
    pub poly_id: Option<PolyID>,
    /// The type arguments if the symbol is generic.
    /// Guarenteed to be Some(_) after type checking is completed.
    pub generic_args: Option<Vec<Type>>,
}

#[derive(
    Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Serialize, Deserialize, JsonSchema,
)]
pub struct PolyID {
    pub id: u64,
    pub ptype: PolynomialType,
}

impl From<&Symbol> for PolyID {
    fn from(symbol: &Symbol) -> Self {
        let SymbolKind::Poly(ptype) = symbol.kind else {
            panic!()
        };
        PolyID {
            id: symbol.id,
            ptype,
        }
    }
}

impl Hash for PolyID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // single call to hash is faster
        ((self.id << 2) + self.ptype as u64).hash(state);
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, JsonSchema,
)]
pub enum PolynomialType {
    Committed = 0,
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

#[cfg(test)]
mod tests {
    use crate::SourceRef;

    use super::{AlgebraicExpression, Analyzed};

    #[test]
    fn insert_remove_identities() {
        let mut pil = Analyzed::default();
        pil.append_polynomial_identity(AlgebraicExpression::Number(0), SourceRef::unknown());
        pil.append_polynomial_identity(AlgebraicExpression::Number(1), SourceRef::unknown());
        pil.append_polynomial_identity(AlgebraicExpression::Number(2), SourceRef::unknown());
        pil.append_polynomial_identity(AlgebraicExpression::Number(3), SourceRef::unknown());
        assert_eq!(pil.identities.len(), 4);
        assert_eq!(pil.source_order.len(), 4);

        let to_remove = [4].into_iter().collect();
        pil.remove_identities(&to_remove);
        assert_eq!(pil.identities.len(), 4);
        assert_eq!(pil.source_order.len(), 4);

        let to_remove = [1, 2].into_iter().collect();
        pil.remove_identities(&to_remove);
        assert_eq!(pil.identities.len(), 2);
        assert_eq!(pil.source_order.len(), 2);

        pil.append_polynomial_identity(AlgebraicExpression::Number(4), SourceRef::unknown());
        pil.append_polynomial_identity(AlgebraicExpression::Number(5), SourceRef::unknown());
        assert_eq!(pil.identities.len(), 4);
        assert_eq!(pil.source_order.len(), 4);

        let to_remove = [1, 2].into_iter().collect();
        pil.remove_identities(&to_remove);
        assert_eq!(pil.identities.len(), 2);
        assert_eq!(pil.source_order.len(), 2);

        let mut pil_result = Analyzed::default();
        pil_result.append_polynomial_identity(AlgebraicExpression::Number(0), SourceRef::unknown());
        pil_result.append_polynomial_identity(AlgebraicExpression::Number(5), SourceRef::unknown());
        pil_result.identities[1].id = 6;
        assert_eq!(pil.identities, pil_result.identities);
        assert_eq!(pil.source_order, pil_result.source_order);
    }
}
