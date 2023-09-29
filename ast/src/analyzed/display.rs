//! Formatting functions for analyzed PIL files.
//!
//! These are not meant to be 1-1 reproductions, they will have errors.
//! Do not use this to re-generate PIL files!

use std::fmt::{Display, Formatter, Result};

use itertools::Itertools;

use crate::parsed::display::format_expressions;

use super::*;

impl<T: Display> Display for Analyzed<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut current_namespace = "Global".to_string();
        let mut update_namespace = |name: &str, degree: DegreeType, f: &mut Formatter<'_>| {
            let new_name = if let Some(dot) = name.find('.') {
                if name[..dot] != current_namespace {
                    current_namespace = name[..dot].to_string();
                    writeln!(f, "namespace {current_namespace}({degree});")?;
                }
                &name[dot + 1..]
            } else {
                name
            };
            Ok((new_name.to_string(), &current_namespace != "Global"))
        };

        for statement in &self.source_order {
            match statement {
                StatementIdentifier::Definition(name) => {
                    let (symbol, definition) = &self.definitions[name];
                    let (name, is_local) = update_namespace(name, symbol.degree, f)?;
                    match symbol.kind {
                        SymbolKind::Poly(poly_type) => {
                            let kind = match &poly_type {
                                PolynomialType::Committed => "witness ",
                                PolynomialType::Constant => "fixed ",
                                PolynomialType::Intermediate => "",
                            };
                            write!(f, "    col {kind}{name}")?;
                            if let Some(value) = definition {
                                writeln!(f, "{value};")?
                            } else {
                                writeln!(f, ";")?
                            }
                        }
                        SymbolKind::Constant() => {
                            let indentation = if is_local { "    " } else { "" };
                            writeln!(
                                f,
                                "{indentation}constant {name}{};",
                                definition.as_ref().unwrap()
                            )?;
                        }
                        SymbolKind::Other() => {
                            write!(f, "    let {name}")?;
                            if let Some(value) = definition {
                                write!(f, "{value}")?
                            }
                            writeln!(f, ";")?
                        }
                    }
                }
                StatementIdentifier::PublicDeclaration(name) => {
                    let decl = &self.public_declarations[name];
                    // TODO we do not know the degree of the namespace here.
                    let (name, _) = update_namespace(&decl.name, 0, f)?;
                    writeln!(
                        f,
                        "    public {name} = {}({});",
                        decl.polynomial, decl.index
                    )?;
                }
                StatementIdentifier::Identity(i) => writeln!(f, "    {}", &self.identities[*i])?,
            }
        }

        Ok(())
    }
}

impl<T: Display> Display for FunctionValueDefinition<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionValueDefinition::Mapping(e) => write!(f, "(i) {{ {e} }}"),
            FunctionValueDefinition::Array(items) => {
                write!(f, " = {}", items.iter().format(" + "))
            }
            FunctionValueDefinition::Query(e) => write!(f, "(i) query {e}"),
            FunctionValueDefinition::Expression(e) => write!(f, " = {e}"),
        }
    }
}

impl<T: Display> Display for RepeatedArray<T> {
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

impl<T: Display> Display for Identity<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let expression = self.expression_for_poly_id();
                if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expression {
                    write!(f, "{left} = {right};")
                } else {
                    write!(f, "{expression} = 0;")
                }
            }
            IdentityKind::Plookup => write!(f, "{} in {};", self.left, self.right),
            IdentityKind::Permutation => write!(f, "{} is {};", self.left, self.right),
            IdentityKind::Connect => write!(f, "{} connect {};", self.left, self.right),
        }
    }
}

impl<T: Display> Display for SelectedExpressions<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{{ {} }}",
            self.selector
                .as_ref()
                .map(|s| format!("{s} "))
                .unwrap_or_default(),
            format_expressions(&self.expressions)
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

impl Display for PolynomialReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            self.name,
            self.index
                .as_ref()
                .map(|s| format!("[{s}]"))
                .unwrap_or_default(),
            if self.next { "'" } else { "" }
        )
    }
}
