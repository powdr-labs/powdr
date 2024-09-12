use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{Analyzed, Expression, FunctionValueDefinition, PolynomialReference, Reference},
    parsed::{
        display::{format_type_args, quote},
        types::{ArrayType, FunctionType, Type, TypeScheme},
        ArrayLiteral, BinaryOperation, BinaryOperator, BlockExpression, FunctionCall, IfExpression,
        IndexAccess, LambdaExpression, Number, StatementInsideBlock, UnaryOperation,
    },
};
use powdr_number::FieldElement;

pub struct CodeGenerator<'a, T> {
    analyzed: &'a Analyzed<T>,
    requested: HashSet<String>,
    failed: HashMap<String, String>,
    symbols: HashMap<String, String>,
}

impl<'a, T: FieldElement> CodeGenerator<'a, T> {
    pub fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            requested: Default::default(),
            failed: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn request_symbol(&mut self, name: &str) -> Result<(), String> {
        if let Some(err) = self.failed.get(name) {
            return Err(err.clone());
        }
        if self.requested.contains(name) {
            return Ok(());
        }
        self.requested.insert(name.to_string());
        match self.generate_code(name) {
            Ok(code) => {
                self.symbols.insert(name.to_string(), code);
                Ok(())
            }
            Err(err) => {
                let err = format!("Failed to compile {name}: {err}");
                self.failed.insert(name.to_string(), err.clone());
                Err(err)
            }
        }
    }

    pub fn is_compiled(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn compiled_symbols(self) -> String {
        self.symbols
            .into_iter()
            .sorted()
            .map(|(_, code)| code)
            .format("\n")
            .to_string()
    }

    fn generate_code(&mut self, symbol: &str) -> Result<String, String> {
        if let Some(code) = self.try_generate_builtin(symbol) {
            return Ok(code);
        }

        let Some((_, Some(FunctionValueDefinition::Expression(value)))) =
            self.analyzed.definitions.get(symbol)
        else {
            return Err(format!(
                "No definition for {symbol}, or not a generic symbol"
            ));
        };

        let type_scheme = value.type_scheme.clone().unwrap();

        Ok(match type_scheme {
            TypeScheme {
                vars,
                ty:
                    Type::Function(FunctionType {
                        params: param_types,
                        value: return_type,
                    }),
            } => {
                let Expression::LambdaExpression(_, LambdaExpression { params, body, .. }) =
                    &value.e
                else {
                    return Err(format!(
                        "Expected lambda expression for {symbol}, got {}",
                        value.e
                    ));
                };
                assert!(vars.is_empty());
                format!(
                    "fn {}({}) -> {} {{ {} }}\n",
                    escape_symbol(symbol),
                    params
                        .iter()
                        .zip(param_types)
                        .map(|(p, t)| format!("{}: {}", p, map_type(&t)))
                        .format(", "),
                    map_type(return_type.as_ref()),
                    self.format_expr(body)?
                )
            }
            _ => format!(
                "const {}: {} = {};\n",
                escape_symbol(symbol),
                map_type(&value.type_scheme.as_ref().unwrap().ty),
                self.format_expr(&value.e)?
            ),
        })
    }

    fn try_generate_builtin(&self, symbol: &str) -> Option<String> {
        let code = match symbol {
            "std::check::panic" => Some("(s: &str) -> ! { panic!(\"{s}\"); }".to_string()),
            "std::field::modulus" => {
                let modulus = T::modulus();
                Some(format!("() -> powdr_number::BigInt {{ powdr_number::BigInt::from(\"{modulus}\") }}"))
            }
            "std::convert::fe" => Some("(n: powdr_number::BigInt) -> FieldElement {\n    <FieldElement as PrimeField>::BigInt::try_from(n.to_biguint().unwrap()).unwrap().into()\n}"
                .to_string()),
            _ => None,
        }?;
        Some(format!("fn {}{code}", escape_symbol(symbol)))
    }

    fn format_expr(&mut self, e: &Expression) -> Result<String, String> {
        Ok(match e {
            Expression::Reference(_, Reference::LocalVar(_id, name)) => name.clone(),
            Expression::Reference(_, Reference::Poly(PolynomialReference { name, type_args })) => {
                self.request_symbol(name)?;
                let ta = type_args.as_ref().unwrap();
                format!(
                    "{}{}",
                    escape_symbol(name),
                    (!ta.is_empty())
                        .then(|| format!("::{}", format_type_args(ta)))
                        .unwrap_or_default()
                )
            }
            Expression::Number(
                _,
                Number {
                    value,
                    type_: Some(type_),
                },
            ) => {
                let value = u64::try_from(value).unwrap_or_else(|_| unimplemented!());
                match type_ {
                    Type::Int => format!("powdr_number::BigInt::from({value}_u64)"),
                    Type::Fe => format!("FieldElement::from({value}_u64)"),
                    Type::Expr => format!("Expr::from({value}_u64)"),
                    _ => unreachable!(),
                }
            }
            Expression::FunctionCall(
                _,
                FunctionCall {
                    function,
                    arguments,
                },
            ) => {
                format!(
                    "({})({})",
                    self.format_expr(function)?,
                    arguments
                        .iter()
                        .map(|a| self.format_expr(a))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        // TODO these should all be refs -> turn all types to arc
                        .map(|x| format!("{x}.clone()"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::BinaryOperation(_, BinaryOperation { left, op, right }) => {
                let left = self.format_expr(left)?;
                let right = self.format_expr(right)?;
                match op {
                    BinaryOperator::ShiftLeft => {
                        format!("(({left}).clone() << u32::try_from(({right}).clone()).unwrap())")
                    }
                    _ => format!("(({left}).clone() {op} ({right}).clone())"),
                }
            }
            Expression::UnaryOperation(_, UnaryOperation { op, expr }) => {
                format!("({op} ({}).clone())", self.format_expr(expr)?)
            }
            Expression::IndexAccess(_, IndexAccess { array, index }) => {
                format!(
                    "{}[usize::try_from({}).unwrap()].clone()",
                    self.format_expr(array)?,
                    self.format_expr(index)?
                )
            }
            Expression::LambdaExpression(_, LambdaExpression { params, body, .. }) => {
                format!(
                    "|{}| {{ {} }}",
                    params.iter().format(", "),
                    self.format_expr(body)?
                )
            }
            Expression::IfExpression(
                _,
                IfExpression {
                    condition,
                    body,
                    else_body,
                },
            ) => {
                format!(
                    "if {} {{ {} }} else {{ {} }}",
                    self.format_expr(condition)?,
                    self.format_expr(body)?,
                    self.format_expr(else_body)?
                )
            }
            Expression::ArrayLiteral(_, ArrayLiteral { items }) => {
                format!(
                    "vec![{}]",
                    items
                        .iter()
                        .map(|i| self.format_expr(i))
                        .collect::<Result<Vec<_>, _>>()?
                        .join(", ")
                )
            }
            Expression::String(_, s) => quote(s),
            Expression::Tuple(_, items) => format!(
                "({})",
                items
                    .iter()
                    .map(|i| self.format_expr(i))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(", ")
            ),
            Expression::BlockExpression(_, BlockExpression { statements, expr }) => {
                format!(
                    "{{\n{}\n{}\n}}",
                    statements
                        .iter()
                        .map(|s| self.format_statement(s))
                        .collect::<Result<Vec<_>, _>>()?
                        .join("\n"),
                    expr.as_ref()
                        .map(|e| self.format_expr(e.as_ref()))
                        .transpose()?
                        .unwrap_or_default()
                )
            }
            _ => return Err(format!("Implement {e}")),
        })
    }

    fn format_statement(&mut self, s: &StatementInsideBlock<Expression>) -> Result<String, String> {
        Err(format!("Implement {s}"))
    }
}

pub fn escape_symbol(s: &str) -> String {
    // TODO better escaping
    s.replace('.', "_").replace("::", "_")
}

fn map_type(ty: &Type) -> String {
    match ty {
        Type::Bottom | Type::Bool => format!("{ty}"),
        Type::Int => "powdr_number::BigInt".to_string(),
        Type::Fe => "FieldElement".to_string(),
        Type::String => "String".to_string(),
        Type::Expr => "Expr".to_string(),
        Type::Array(ArrayType { base, length: _ }) => format!("Vec<{}>", map_type(base)),
        Type::Tuple(_) => todo!(),
        Type::Function(ft) => todo!("Type {ft}"),
        Type::TypeVar(tv) => tv.to_string(),
        Type::NamedType(_path, _type_args) => todo!(),
        Type::Col | Type::Inter => unreachable!(),
    }
}
