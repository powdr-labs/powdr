use std::{borrow::Cow, iter::once};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, TypedExpression,
    },
    parsed::{
        asm::SymbolPath,
        types::Type,
        visitor::{AllChildren, Children},
        EnumDeclaration, StructDeclaration, TraitImplementation, TypeDeclaration,
    },
};

/// This trait can be used to iterate over all symbols to find a minimal set that
/// is syntactically complete in the sense that it compiles and contains minimal dependencies.
pub trait ReferencedSymbols {
    /// Returns an iterator over all referenced symbols in self including type names.
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_>;
}

#[derive(Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct SymbolReference<'a> {
    pub name: Cow<'a, str>,
    pub type_args: Option<&'a Vec<Type>>,
}

impl<'a> From<&'a String> for SymbolReference<'a> {
    fn from(name: &'a String) -> Self {
        From::from(name.as_str())
    }
}

impl<'a> From<&'a str> for SymbolReference<'a> {
    fn from(name: &'a str) -> Self {
        SymbolReference {
            name: name.into(),
            type_args: None,
        }
    }
}

impl<'a> From<&SymbolPath> for SymbolReference<'a> {
    fn from(name: &SymbolPath) -> Self {
        SymbolReference {
            name: name.to_string().into(),
            type_args: None,
        }
    }
}

impl<'a> From<&'a PolynomialReference> for SymbolReference<'a> {
    fn from(poly: &'a PolynomialReference) -> Self {
        SymbolReference {
            name: poly.name.as_str().into(),
            type_args: poly.type_args.as_ref(),
        }
    }
}

// TODO if we call the following on a symbol, we also need to extract the
// type variables, so we can then substitute them by the concrete types.
// it should be a separate function and it might live in lib.rs.

impl ReferencedSymbols for FunctionValueDefinition {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        match self {
            FunctionValueDefinition::TypeDeclaration(type_decl) => type_decl.symbols(),
            FunctionValueDefinition::TypeConstructor(enum_decl, _) => {
                // This is the type constructor of an enum variant, it references the enum itself.
                Box::new(once(SymbolReference::from(&enum_decl.name)))
            }
            FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: Some(type_scheme),
                e,
            }) => Box::new(type_scheme.ty.symbols().chain(e.symbols())),
            FunctionValueDefinition::TraitFunction(..) => panic!(
                "Should have called .symbols() on a specific trait impl, not on the trait itself."
            ),
            // TODO check that e.g. an enum referenced in a trait declaration is also included.
            // TODO this is probably not the case as we need to call .symbols() on the types not only the exressions
            FunctionValueDefinition::TraitDeclaration(..)
            | FunctionValueDefinition::Array(..)
            | FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: None, ..
            }) => Box::new(self.children().flat_map(|e| e.symbols())),
        }
    }
}

impl ReferencedSymbols for TraitImplementation<Expression> {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        Box::new(
            once(SymbolReference::from(&self.name))
                .chain(self.functions.iter().flat_map(|f| f.body.symbols()))
                .chain(self.type_scheme.ty.symbols()),
        )
    }
}

impl ReferencedSymbols for TypeDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        match self {
            TypeDeclaration::Enum(enum_decl) => enum_decl.symbols(),
            TypeDeclaration::Struct(struct_decl) => struct_decl.symbols(),
        }
    }
}

impl ReferencedSymbols for EnumDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        Box::new(
            self.variants
                .iter()
                .flat_map(|v| &v.fields)
                .flat_map(|t| t.iter())
                .flat_map(|t| t.symbols()),
        )
    }
}

impl ReferencedSymbols for StructDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        Box::new(self.fields.iter().flat_map(|named| named.ty.symbols()))
    }
}

impl ReferencedSymbols for Expression {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        Box::new(
            self.all_children()
                .flat_map(symbols_in_expression)
                .flatten(),
        )
    }
}

fn symbols_in_expression(
    e: &Expression,
) -> Option<Box<dyn Iterator<Item = SymbolReference<'_>> + '_>> {
    match e {
        Expression::PublicReference(_, name) => Some(Box::new(once(SymbolReference::from(name)))),
        Expression::Reference(_, Reference::Poly(pr @ PolynomialReference { type_args, .. })) => {
            let type_iter = type_args
                .iter()
                .flat_map(|t| t.iter())
                .flat_map(|t| t.symbols());

            Some(Box::new(type_iter.chain(once(SymbolReference::from(pr)))))
        }
        _ => None,
    }
}

impl ReferencedSymbols for Type {
    fn symbols(&self) -> Box<dyn Iterator<Item = SymbolReference<'_>> + '_> {
        Box::new(self.contained_named_types().map(SymbolReference::from))
    }
}
