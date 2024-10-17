use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{FunctionValueDefinition, PolynomialReference, Reference, Symbol},
    parsed::{StructExpression, TypeDeclaration},
};
use powdr_parser_util::{Error, SourceRef};

/// Verifies that all struct instantiations match their corresponding declarations (existence of field names, completeness)
pub fn check_structs_fields<'a>(
    structs_exprs: impl Iterator<Item = (&'a SourceRef, &'a StructExpression<Reference>)>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) -> Vec<Error> {
    let mut errors = Vec::new();
    let mut visited_structs = HashSet::new();

    for (sr, st) in structs_exprs {
        let StructExpression {
            name: Reference::Poly(PolynomialReference { name, .. }),
            fields,
        } = st
        else {
            unreachable!()
        };

        if let Some((
            _,
            Some(FunctionValueDefinition::TypeDeclaration(TypeDeclaration::Struct(struct_decl))),
        )) = definitions.get(name)
        {
            let declared_fields: HashSet<_> = struct_decl.fields.iter().map(|f| &f.name).collect();
            let used_fields: HashSet<_> = fields.iter().map(|f| &f.name).collect();

            errors.extend(
                fields
                    .iter()
                    .filter(|f| !declared_fields.contains(&f.name))
                    .map(|f| {
                        sr.with_error(format!("Struct '{name}' has no field named '{}'", f.name))
                    }),
            );

            errors.extend(declared_fields.difference(&used_fields).map(|&f| {
                sr.with_error(format!("Missing field '{f}' in initializer of '{name}'",))
            }));

            let duplicate_fields: Vec<_> = fields.iter().map(|f| &f.name).duplicates().collect();

            errors.extend(
                duplicate_fields
                    .into_iter()
                    .map(|f| sr.with_error(format!("Field '{f}' specified more than once"))),
            );
        }

        visited_structs.insert(name);
    }

    let structs_decls = definitions.iter().filter(|(_, (_, def))| {
        matches!(
            def,
            Some(FunctionValueDefinition::TypeDeclaration(
                TypeDeclaration::Struct(_)
            ))
        )
    });

    for (_, (symbol, def)) in structs_decls {
        if let Some(FunctionValueDefinition::TypeDeclaration(TypeDeclaration::Struct(
            struct_decl,
        ))) = def
        {
            let duplicate_declaration_fields: Vec<_> = struct_decl
                .fields
                .iter()
                .map(|f| &f.name)
                .duplicates()
                .collect();

            errors.extend(duplicate_declaration_fields.into_iter().map(|f| {
                symbol
                    .source
                    .with_error(format!("Field '{f}' is declared more than once"))
            }));
        } else {
            unreachable!()
        }
    }

    errors
}
