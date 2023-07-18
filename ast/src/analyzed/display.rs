//! Formatting functions for analyzed PIL files.
//!
//! These are not meant to be 1-1 reproductions, they will have errors.
//! Do not use this to re-generate PIL files!

use std::fmt::{Display, Formatter, Result};

use itertools::Itertools;

use super::*;

impl<T: Display> Display for Analyzed<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (name, value) in &self.constants {
            writeln!(f, "constant {name} = {value};")?;
        }

        let mut namespace = "Global".to_string();
        let mut update_namespace = |name: &str, degree: DegreeType, f: &mut Formatter<'_>| {
            if let Some(dot) = name.find('.') {
                if name[..dot] != namespace {
                    namespace = name[..dot].to_string();
                    writeln!(f, "namespace {namespace}({degree});")?;
                }
                Ok(name[dot + 1..].to_string())
            } else {
                Ok(name.to_string())
            }
        };

        for statement in &self.source_order {
            match statement {
                StatementIdentifier::Definition(name) => {
                    let (poly, definition) = &self.definitions[name];
                    let name = update_namespace(name, poly.degree, f)?;
                    let kind = match &poly.poly_type {
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
                StatementIdentifier::PublicDeclaration(name) => {
                    let decl = &self.public_declarations[name];
                    // TODO we do not know the degree of the namespace here.
                    let name = update_namespace(&decl.name, 0, f)?;
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
                write!(f, " = {}", items.iter().map(|i| i.to_string()).join(" + "))
            }
            FunctionValueDefinition::Query(e) => write!(f, "(i) query {e}"),
        }
    }
}

impl<T: Display> Display for RepeatedArray<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.is_empty() {
            return Ok(());
        }
        write!(
            f,
            "[{}]",
            self.pattern.iter().map(|i| i.to_string()).join(", ")
        )?;
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

impl<T: Display> Display for Expression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Constant(name) => write!(f, "{name}"),
            Expression::PolynomialReference(reference) => write!(f, "{reference}"),
            Expression::PublicReference(name) => write!(f, ":{name}"),
            Expression::Number(value) => write!(f, "{value}"),
            Expression::String(value) => write!(f, "\"{value}\""), // TODO quote?
            Expression::Tuple(items) => write!(f, "({})", format_expressions(items)),
            Expression::BinaryOperation(left, op, right) => write!(f, "({left} {op} {right})"),
            Expression::UnaryOperation(op, exp) => write!(f, "{op}{exp}"),
            Expression::FunctionCall(fun, args) => write!(f, "{fun}({})", format_expressions(args)),
            Expression::LocalVariableReference(index) => {
                // TODO this is not really reproducing the input, but
                // if we want to do that, we would need the names of the local variables somehow.
                if *index == 0 {
                    write!(f, "i")
                } else {
                    write!(f, "${index}")
                }
            }
            Expression::MatchExpression(scrutinee, arms) => write!(
                f,
                "match {scrutinee} {{ {} }}",
                arms.iter()
                    .map(|(n, e)| format!(
                        "{} => {e},",
                        n.as_ref()
                            .map(|n| n.to_string())
                            .unwrap_or_else(|| "_".to_string())
                    ))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

fn format_expressions<T: Display>(expressions: &[Expression<T>]) -> String {
    expressions
        .iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join(", ")
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
