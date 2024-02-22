//! Formatting functions for analyzed PIL files.
//!
//! These are not meant to be 1-1 reproductions, they will have errors.
//! Do not use this to re-generate PIL files!

use std::{
    fmt::{Display, Formatter, Result},
    str::FromStr,
};

use itertools::Itertools;

use self::{
    parsed::asm::{AbsoluteSymbolPath, SymbolPath},
    types::{format_type_scheme_around_name, ArrayType, FunctionType, TupleType, Type},
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
                    if let Some((symbol, definition)) = self.definitions.get(name) {
                        let (name, is_local) = update_namespace(name, f)?;
                        match symbol.kind {
                            SymbolKind::Poly(poly_type) => {
                                let kind = match &poly_type {
                                    PolynomialType::Committed => "witness ",
                                    PolynomialType::Constant => "fixed ",
                                    PolynomialType::Intermediate => panic!(),
                                };
                                write!(f, "    col {kind}{name}")?;
                                if let Some(length) = symbol.length {
                                    if let PolynomialType::Committed = poly_type {
                                        write!(f, "[{length}]")?;
                                        assert!(definition.is_none());
                                    } else {
                                        // Do not print an array size, because we will do it as part of the type.
                                        assert!(matches!(
                                            definition,
                                            None | Some(FunctionValueDefinition::Expression(
                                                TypedExpression {
                                                    e: _,
                                                    type_scheme: Some(_)
                                                }
                                            ))
                                        ));
                                    }
                                }
                                if let Some(value) = definition {
                                    writeln!(f, "{value};")?
                                } else {
                                    writeln!(f, ";")?
                                }
                            }
                            SymbolKind::Constant() => {
                                let indentation = if is_local { "    " } else { "" };
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

                                writeln!(f, "{indentation}constant {name} = {e};",)?;
                            }
                            SymbolKind::Other() => match definition {
                                Some(FunctionValueDefinition::Expression(TypedExpression {
                                    e,
                                    type_scheme,
                                })) => {
                                    writeln!(
                                        f,
                                        "    let{} = {e};",
                                        format_type_scheme_around_name(&name, type_scheme)
                                    )?;
                                }
                                _ => {
                                    unreachable!("Invalid definition for symbol: {}", name)
                                }
                            },
                        }
                    } else if let Some((symbol, definition)) = self.intermediate_columns.get(name) {
                        let (name, _) = update_namespace(name, f)?;
                        assert_eq!(symbol.kind, SymbolKind::Poly(PolynomialType::Intermediate));
                        if let Some(length) = symbol.length {
                            writeln!(
                                f,
                                "    col {name}[{length}] = [{}];",
                                definition.iter().format(", ")
                            )?;
                        } else {
                            assert_eq!(definition.len(), 1);
                            writeln!(f, "    col {name} = {};", definition[0])?;
                        }
                    } else {
                        panic!()
                    }
                }
                StatementIdentifier::PublicDeclaration(name) => {
                    let decl = &self.public_declarations[name];
                    let (name, is_local) = update_namespace(&decl.name, f)?;
                    let indentation = if is_local { "    " } else { "" };
                    writeln!(
                        f,
                        "{indentation}public {name} = {}{}({});",
                        decl.polynomial,
                        decl.array_index
                            .map(|i| format!("[{i}]"))
                            .unwrap_or_default(),
                        decl.index
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
            FunctionValueDefinition::Array(items) => {
                write!(f, " = {}", items.iter().format(" + "))
            }
            FunctionValueDefinition::Query(e) => format_outer_function(e, Some("query"), f),
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: None,
            }) => format_outer_function(e, None, f),
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: Some(ty),
            }) if *ty == Type::Col.into() => format_outer_function(e, None, f),
            FunctionValueDefinition::Expression(TypedExpression {
                e,
                type_scheme: Some(ts),
            }) => {
                assert!(ts.vars.is_empty(), "Should not have called this display function, since we cannot properly format the type vars.");
                write!(f, ": {} = {e}", ts.ty)
            }
        }
    }
}

fn format_outer_function<T: Display>(
    e: &Expression<T>,
    qualifier: Option<&str>,
    f: &mut Formatter<'_>,
) -> Result {
    let q = qualifier.map(|s| format!(" {s}")).unwrap_or_default();
    match e {
        parsed::Expression::LambdaExpression(lambda) if lambda.params.len() == 1 => {
            let body = if q.is_empty() {
                format!("{{ {} }}", lambda.body)
            } else {
                format!("{}", lambda.body)
            };
            write!(f, "({}){q} {body}", lambda.params.iter().format(", "),)
        }
        _ => write!(f, " ={q} {e}"),
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

impl<T: Display> Display for Identity<Expression<T>> {
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

impl<T: Display> Display for Identity<AlgebraicExpression<T>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let expression = self.expression_for_poly_id();
                if let AlgebraicExpression::BinaryOperation(
                    left,
                    AlgebraicBinaryOperator::Sub,
                    right,
                ) = expression
                {
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
        write!(f, "{}", self.name,)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Bottom => write!(f, "!"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Fe => write!(f, "fe"),
            Type::String => write!(f, "string"),
            Type::Expr => write!(f, "expr"),
            Type::Constr => write!(f, "constr"),
            Type::Col => write!(f, "col"),
            Type::Array(ar) => write!(f, "{ar}"),
            Type::Tuple(tu) => write!(f, "{tu}"),
            Type::Function(fun) => write!(f, "{fun}"),
            Type::TypeVar(v) => write!(f, "{v}"),
        }
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}[{}]",
            format_type_with_parentheses(&self.base),
            self.length.iter().format("")
        )
    }
}

impl Display for TupleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({})", format_list_of_types(&self.items))
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}-> {}",
            format_list_of_types(&self.params),
            if self.params.is_empty() { "" } else { " " },
            format_type_with_parentheses(&self.value)
        )
    }
}

fn format_type_with_parentheses(ty: &Type) -> String {
    if ty.needs_parentheses() {
        format!("({ty})")
    } else {
        ty.to_string()
    }
}

fn format_list_of_types(types: &[Type]) -> String {
    types
        .iter()
        .map(format_type_with_parentheses)
        .format(", ")
        .to_string()
}
