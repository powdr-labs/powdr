use std::{borrow::Cow, iter::once};

use powdr_ast::{
    analyzed::{
        Expression, FunctionValueDefinition, PolynomialReference, Reference, TypedExpression,
    },
    parsed::{
        asm::SymbolPath,
        types::Type,
        visitor::{AllChildren, Children},
        EnumDeclaration, StructDeclaration, TypeDeclaration,
    },
};

pub trait ReferencedSymbols {
    /// Returns an iterator over all referenced symbols in the syntactic element `self`,
    /// including symbols referenced inside types and type names.
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_>;
}

pub struct ReferencedSymbol<'a> {
    pub name: Cow<'a, str>,
    pub type_args: Option<&'a Vec<Type>>,
}

impl<'a> From<&'a String> for ReferencedSymbol<'a> {
    fn from(name: &'a String) -> Self {
        Self {
            name: name.into(),
            type_args: None,
        }
    }
}

impl<'a> From<&'a SymbolPath> for ReferencedSymbol<'a> {
    fn from(name: &'a SymbolPath) -> Self {
        Self {
            name: name.to_string().into(),
            type_args: None,
        }
    }
}

impl ReferencedSymbols for FunctionValueDefinition {
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
        match self {
            FunctionValueDefinition::TypeDeclaration(type_decl) => type_decl.symbols(),
            FunctionValueDefinition::TypeConstructor(enum_decl, _) => {
                // This is the type constructor of an enum variant, it references the enum itself.
                Box::new(once(ReferencedSymbol::from(&enum_decl.name)))
            }
            FunctionValueDefinition::TraitDeclaration(..)
            | FunctionValueDefinition::TraitFunction(..) => {
                // TODO but maybe we should at least iterate over the types here?
                panic!("Cannot compute referenced symbols for trait function. Should have called on the specific implementation.")
            }
            FunctionValueDefinition::Expression(TypedExpression {
                type_scheme: Some(type_scheme),
                // TODO we should also provide a way to get the type vars here.
                e,
            }) => Box::new(type_scheme.ty.symbols().chain(e.symbols())),
            FunctionValueDefinition::Array(..) | FunctionValueDefinition::Expression(..) => {
                Box::new(self.children().flat_map(|e| e.symbols()))
            }
        }
    }
}

impl ReferencedSymbols for TypeDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
        match self {
            TypeDeclaration::Enum(enum_decl) => enum_decl.symbols(),
            TypeDeclaration::Struct(struct_decl) => struct_decl.symbols(),
        }
    }
}

// TODO for trait resolving, we don't really need this, do we?

impl ReferencedSymbols for EnumDeclaration {
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
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
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
        Box::new(self.fields.iter().flat_map(|named| named.ty.symbols()))
    }
}

impl ReferencedSymbols for Expression {
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
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
                            // TODO add the type args here.
                            .chain(once(ReferencedSymbol {
                                name: name.as_str().into(),
                                type_args: type_args.as_ref(),
                            })),
                    ),
                    _ => None,
                })
                .flatten(),
        )
    }
}

// TOOD this is also not needed for resolving.

impl ReferencedSymbols for Type {
    fn symbols(&self) -> Box<dyn Iterator<Item = ReferencedSymbol<'_>> + '_> {
        Box::new(self.contained_named_types().map(|n| n.into()))
    }
}
