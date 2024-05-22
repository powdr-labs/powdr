//! Formatting functions for analyzed PIL files.
//!
//! These are not meant to be 1-1 reproductions, they will have errors.
//! Do not use this to re-generate PIL files!

use std::{
    fmt::{Display, Formatter, Result},
    str::FromStr,
};

use itertools::Itertools;

use crate::{parsed::FunctionKind, writeln_indented, writeln_indented_by};

use self::parsed::{
    asm::{AbsoluteSymbolPath, SymbolPath},
    display::format_type_scheme_around_name,
};

use super::*;

impl<T: Display> Display for Analyzed<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let degree = self.degree.unwrap_or_default();
        let mut current_namespace = AbsoluteSymbolPath::default();
        let mut update_namespace = |name: &str, f: &mut Formatter<'_>| {
            let mut namespace =
                AbsoluteSymbolPath::default().join(SymbolPath::from_str(name).unwrap());
            let name = namespace.pop().unwrap();
            if namespace != current_namespace {
                current_namespace = namespace;
                writeln!(
                    f,
                    "namespace {}({degree});",
                    current_namespace.relative_to(&Default::default())
                )?;
            };
            Ok((name, !current_namespace.is_empty()))
        };

        for statement in &self.source_order {
            match statement {
                StatementIdentifier::Definition(name) => {
                    if self.auto_added_symbols.contains(name) {
                        // Core symbol added automatically, no need to print.
                        continue;
                    }
                    if let Some((symbol, definition)) = self.definitions.get(name) {
                        if matches!(
                            definition,
                            Some(FunctionValueDefinition::TypeConstructor(_, _))
                        ) {
                            // These are printed as part of the enum.
                            continue;
                        }
                        let (name, is_local) = update_namespace(name, f)?;
                        match symbol.kind {
                            SymbolKind::Poly(_) => {
                                writeln_indented(f, format_poly(&name, symbol, definition))?;
                            }
                            SymbolKind::Constant() => {
                                assert!(symbol.stage.is_none());
                                let Some(FunctionValueDefinition::Expression(TypedExpression {
                                    e,
                                    type_scheme,
                                })) = &definition
                                else {
                                    panic!(
                                        "Invalid constant value: {}",
                                        definition.as_ref().unwrap()
                                    );
                                };
                                assert!(
                                    type_scheme.is_none()
                                        || type_scheme == &Some((Type::Fe).into())
                                );
                                writeln_indented_by(
                                    f,
                                    format!("constant {name} = {e};"),
                                    is_local.into(),
                                )?;
                            }
                            SymbolKind::Other() => {
                                assert!(symbol.stage.is_none());
                                match definition {
                                    Some(FunctionValueDefinition::Expression(
                                        TypedExpression { e, type_scheme },
                                    )) => {
                                        writeln_indented(
                                            f,
                                            format!(
                                                "let{} = {e};",
                                                format_type_scheme_around_name(&name, type_scheme)
                                            ),
                                        )?;
                                    }
                                    Some(FunctionValueDefinition::TypeDeclaration(
                                        enum_declaration,
                                    )) => {
                                        writeln_indented(
                                            f,
                                            enum_declaration.to_string_with_name(&name),
                                        )?;
                                    }
                                    _ => {
                                        unreachable!("Invalid definition for symbol: {}", name)
                                    }
                                }
                            }
                        }
                    } else if let Some((symbol, definition)) = self.intermediate_columns.get(name) {
                        assert!(symbol.stage.is_none());
                        let (name, _) = update_namespace(name, f)?;
                        assert_eq!(symbol.kind, SymbolKind::Poly(PolynomialType::Intermediate));
                        if let Some(length) = symbol.length {
                            writeln_indented(
                                f,
                                format!(
                                    "col {name}[{length}] = [{}];",
                                    definition.iter().format(", ")
                                ),
                            )?;
                        } else {
                            assert_eq!(definition.len(), 1);
                            writeln_indented(f, format!("col {name} = {};", definition[0]))?;
                        }
                    } else {
                        panic!()
                    }
                }
                StatementIdentifier::PublicDeclaration(name) => {
                    let decl = &self.public_declarations[name];
                    let (name, is_local) = update_namespace(&decl.name, f)?;
                    writeln_indented_by(
                        f,
                        format_public_declaration(&name, decl),
                        is_local.into(),
                    )?;
                }
                StatementIdentifier::Identity(i) => {
                    writeln_indented(f, &self.identities[*i])?;
                }
            }
        }

        Ok(())
    }
}

fn format_poly(
    name: &str,
    symbol: &Symbol,
    definition: &Option<FunctionValueDefinition>,
) -> String {
    let SymbolKind::Poly(poly_type) = symbol.kind else {
        panic!()
    };
    let kind = match &poly_type {
        PolynomialType::Committed => "witness ",
        PolynomialType::Constant => "fixed ",
        PolynomialType::Intermediate => panic!(),
    };
    let stage = symbol
        .stage
        .map(|s| format!("stage({s}) "))
        .unwrap_or_default();
    let length = symbol
        .length
        .and_then(|length| {
            if let PolynomialType::Committed = poly_type {
                assert!(definition.is_none());
                Some(format!("[{length}]"))
            } else {
                // Do not print an array size, because we will do it as part of the type.
                assert!(matches!(
                    definition,
                    None | Some(FunctionValueDefinition::Expression(TypedExpression {
                        e: _,
                        type_scheme: Some(_)
                    }))
                ));
                None
            }
        })
        .unwrap_or_default();
    let value = definition
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default();
    format!("col {kind}{stage}{name}{length}{value};")
}

fn format_public_declaration(name: &str, decl: &PublicDeclaration) -> String {
    format!(
        "public {name} = {}{}({});",
        decl.polynomial,
        decl.array_index
            .map(|i| format!("[{i}]"))
            .unwrap_or_default(),
        decl.index
    )
}

impl Display for FunctionValueDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionValueDefinition::Array(items) => {
                write!(f, " = {}", items.iter().format(" + "))
            }
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: None,
            }) => format_outer_function(e, f),
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: Some(ty),
            }) if *ty == Type::Col.into() => format_outer_function(e, f),
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: Some(ts),
            }) => {
                assert!(ts.vars.is_empty(), "Should not have called this display function, since we cannot properly format the type vars.");
                write!(f, ": {} = {e}", ts.ty)
            }
            FunctionValueDefinition::TypeDeclaration(_)
            | FunctionValueDefinition::TypeConstructor(_, _) => {
                panic!("Should not use this formatting function.")
            }
        }
    }
}

fn format_outer_function(e: &Expression, f: &mut Formatter<'_>) -> Result {
    match e {
        parsed::Expression::LambdaExpression(lambda) if lambda.params.len() == 1 => {
            let body = if lambda.kind == FunctionKind::Pure
                && !matches!(lambda.body.as_ref(), Expression::BlockExpression(_, _))
            {
                format!("{{ {} }}", lambda.body)
            } else {
                format!("{}", lambda.body)
            };
            write!(
                f,
                "({}) {}{body}",
                lambda.params.iter().format(", "),
                match lambda.kind {
                    FunctionKind::Pure => "".into(),
                    _ => format!("{} ", &lambda.kind),
                },
            )
        }
        _ => write!(f, " = {e}"),
    }
}

impl Display for RepeatedArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.is_empty() {
            return Ok(());
        }
        write!(f, "[{}]", self.pattern.iter().format(", "))?;
        if self.is_repeated() {
            write!(f, "*")?;
        }
        Ok(())
    }
}

impl Display for Identity<Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let (left, right) = self.as_polynomial_identity();
                let right = right
                    .as_ref()
                    .map(|r| r.to_string())
                    .unwrap_or_else(|| "0".into());
                write!(f, "{left} = {right};")
            }
            IdentityKind::Plookup => write!(f, "{} in {};", self.left, self.right),
            IdentityKind::Permutation => write!(f, "{} is {};", self.left, self.right),
            IdentityKind::Connect => write!(f, "{} connect {};", self.left, self.right),
        }
    }
}

impl<T: Display> Display for Identity<AlgebraicExpression<T>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let (left, right) = self.as_polynomial_identity();
                let right = right
                    .as_ref()
                    .map(|r| r.to_string())
                    .unwrap_or_else(|| "0".into());
                write!(f, "{left} = {right};")
            }
            IdentityKind::Plookup => write!(f, "{} in {};", self.left, self.right),
            IdentityKind::Permutation => write!(f, "{} is {};", self.left, self.right),
            IdentityKind::Connect => write!(f, "{} connect {};", self.left, self.right),
        }
    }
}

impl<Expr: Display> Display for SelectedExpressions<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{{ {} }}",
            self.selector
                .as_ref()
                .map(|s| format!("{s} "))
                .unwrap_or_default(),
            self.expressions.iter().format(", ")
        )
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reference::LocalVar(_index, name) => {
                write!(f, "{name}")
            }
            Reference::Poly(r) => write!(f, "{r}"),
        }
    }
}

impl<T: Display> Display for AlgebraicExpression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AlgebraicExpression::Reference(reference) => write!(f, "{reference}"),
            AlgebraicExpression::PublicReference(name) => write!(f, ":{name}"),
            AlgebraicExpression::Challenge(challenge) => {
                write!(
                    f,
                    "std::prover::challenge({}, {})",
                    challenge.stage, challenge.id,
                )
            }
            AlgebraicExpression::Number(value) => write!(f, "{value}"),
            AlgebraicExpression::BinaryOperation(left, op, right) => {
                write!(f, "({left} {op} {right})")
            }
            AlgebraicExpression::UnaryOperation(op, exp) => write!(f, "{op}{exp}"),
        }
    }
}

impl Display for AlgebraicUnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        UnaryOperator::from(*self).fmt(f)
    }
}

impl Display for AlgebraicBinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        BinaryOperator::from(*self).fmt(f)
    }
}

impl Display for AlgebraicReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.name, if self.next { "'" } else { "" },)
    }
}

impl Display for PolynomialReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)?;
        if let Some(type_args) = &self.type_args {
            if !type_args.is_empty() {
                write!(f, "::<{}>", type_args.iter().join(", "))?;
            }
        }
        Ok(())
    }
}
