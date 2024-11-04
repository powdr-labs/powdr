use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use powdr_ast::{
    analyzed::{Expression, FunctionValueDefinition, PolynomialReference, Reference, Symbol},
    parsed::{StructExpression, TypeDeclaration},
};
use powdr_parser_util::{Error, SourceRef};

/// Verifies that all struct instantiations match their corresponding declarations
/// (existence of field names, completeness) and ensures that both are correct.
pub fn check_structs_fields<'a>(
    structs_exprs: impl Iterator<Item = &'a Expression>,
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();

    for expr in structs_exprs {
        let Expression::StructExpression(
            source,
            StructExpression {
                name: Reference::Poly(PolynomialReference { name, .. }),
                fields,
            },
        ) = expr
        else {
            unreachable!()
        };

        errors.extend(check_struct_expression(definitions, name, fields, source));
    }

    errors.extend(check_struct_declarations(definitions));

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_struct_expression(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
    name: &String,
    fields: &[powdr_ast::parsed::NamedExpression<Box<powdr_ast::parsed::Expression<Reference>>>],
    source: &SourceRef,
) -> Vec<Error> {
    let mut errors = Vec::new();
    if let (
        _,
        Some(FunctionValueDefinition::TypeDeclaration(TypeDeclaration::Struct(struct_decl))),
    ) = definitions.get(name).unwrap()
    {
        let declared_fields: HashSet<_> = struct_decl.fields.iter().map(|f| &f.name).collect();
        let used_fields: HashSet<_> = fields.iter().map(|f| &f.name).collect();

        errors.extend(
            fields
                .iter()
                .filter(|f| !declared_fields.contains(&f.name))
                .map(|f| {
                    source.with_error(format!("Struct '{name}' has no field named '{}'", f.name))
                }),
        );

        errors.extend(declared_fields.difference(&used_fields).map(|&f| {
            source.with_error(format!("Missing field '{f}' in initializer of '{name}'",))
        }));

        let duplicate_fields: Vec<_> = fields.iter().map(|f| &f.name).duplicates().collect();

        errors.extend(
            duplicate_fields
                .into_iter()
                .map(|f| source.with_error(format!("Field '{f}' specified more than once"))),
        );
    } else {
        panic!("Struct '{name}' has not been declared");
    }

    errors
}

fn check_struct_declarations(
    definitions: &HashMap<String, (Symbol, Option<FunctionValueDefinition>)>,
) -> Vec<Error> {
    let mut errors = Vec::new();
    for (symbol, def) in definitions.values() {
        let Some(FunctionValueDefinition::TypeDeclaration(TypeDeclaration::Struct(struct_decl))) =
            def
        else {
            continue;
        };

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
    }

    errors
}
