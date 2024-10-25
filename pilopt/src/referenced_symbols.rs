use std::{borrow::Cow, iter::once};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, TypedExpression,
    },
    parsed::{
        types::Type,
        visitor::{AllChildren, Children},
        EnumDeclaration, StructDeclaration, TypeDeclaration,
    },
};

pub trait ReferencedSymbols {
    /// Returns an iterator over all referenced symbols in self including type names.
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_>;
}

impl ReferencedSymbols for FunctionValueDefinition {
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
        match self {
            FunctionValueDefinition::TypeDeclaration(type_decl) => type_decl.symbols(),
            FunctionValueDefinition::TypeConstructor(enum_decl, _) => {
                // This is the type constructor of an enum variant, it references the enum itself.
                Box::new(once(enum_decl.name.as_str().into()))
            }
            FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: Some(type_scheme),
                e,
            }) => Box::new(type_scheme.ty.symbols().chain(e.symbols())),
            _ => Box::new(self.children().flat_map(|e| e.symbols())),
        }
    }
}

impl ReferencedSymbols for TypeDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
        match self {
            TypeDeclaration::Enum(enum_decl) => enum_decl.symbols(),
            TypeDeclaration::Struct(struct_decl) => struct_decl.symbols(),
        }
    }
}

impl ReferencedSymbols for EnumDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
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
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
        Box::new(self.fields.iter().flat_map(|named| named.ty.symbols()))
    }
}

impl ReferencedSymbols for Expression {
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
        Box::new(
            self.all_children()
                .flat_map(|e| match e {
                    Expression::Reference(
                        _,
                        Reference::Poly(PolynomialReference { name, type_args }),
                    ) => Some(
                        type_args
                            .iter()
                            .flat_map(|t| t.iter())
                            .flat_map(|t| t.symbols())
                            .chain(once(name.into())),
                    ),
                    _ => None,
                })
                .flatten(),
        )
    }
}

impl ReferencedSymbols for Type {
    fn symbols(&self) -> Box<dyn Iterator<Item = Cow<'_, str>> + '_> {
        Box::new(self.contained_named_types().map(|n| n.to_string().into()))
    }
}
