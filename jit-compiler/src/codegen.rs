use std::collections::HashMap;

use itertools::Itertools;
use powdr_ast::{
    analyzed::{Analyzed, Expression, FunctionValueDefinition, PolynomialReference, Reference},
    parsed::{
        display::quote,
        types::{ArrayType, FunctionType, Type, TypeScheme},
        ArrayLiteral, BinaryOperation, BinaryOperator, BlockExpression, FunctionCall, IfExpression,
        IndexAccess, LambdaExpression, Number, StatementInsideBlock, UnaryOperation,
    },
};
use powdr_number::{BigUint, FieldElement, LargeInt};

pub struct CodeGenerator<'a, T> {
    analyzed: &'a Analyzed<T>,
    /// Symbols mapping to either their code or an error message explaining
    /// why they could not be compiled.
    /// While the code is still being generated, this contains `None`.
    symbols: HashMap<String, Result<Option<String>, String>>,
}

pub fn escape_symbol(s: &str) -> String {
    // TODO better escaping
    s.replace('.', "_").replace("::", "_")
}

impl<'a, T: FieldElement> CodeGenerator<'a, T> {
    pub fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            symbols: Default::default(),
        }
    }

    /// Tries to generate code for the symbol and all its dependencies.
    /// On success, returns an expression string for referencing the symbol,
    /// i.e. it evaluates to the value of the symbol.
    /// On failure, returns an error string.
    /// After a failure, `self` can still be used to request other symbols.
    /// The code can later be retrieved via `generated_code`.
    pub fn request_symbol(&mut self, name: &str) -> Result<String, String> {
        match self.symbols.get(name) {
            Some(Err(e)) => return Err(e.clone()),
            Some(_) => {}
            None => {
                let name = name.to_string();
                self.symbols.insert(name.clone(), Ok(None));
                match self.generate_code(&name) {
                    Ok(code) => {
                        self.symbols.insert(name.clone(), Ok(Some(code)));
                    }
                    Err(err) => {
                        self.symbols.insert(name.clone(), Err(err.clone()));
                        return Err(err);
                    }
                }
            }
        }
        Ok(self.symbol_reference(name))
    }

    /// Returns the concatenation of all successfully compiled symbols.
    pub fn generated_code(self) -> String {
        self.symbols
            .into_iter()
            .filter_map(|(s, r)| match r {
                Ok(Some(code)) => Some((s, code)),
                _ => None,
            })
            .sorted()
            .map(|(_, code)| code)
            .format("\n")
            .to_string()
    }

    fn generate_code(&mut self, symbol: &str) -> Result<String, String> {
        if let Some(code) = try_generate_builtin::<T>(symbol) {
            return Ok(code);
        }

        let Some((_, Some(FunctionValueDefinition::Expression(value)))) =
            self.analyzed.definitions.get(symbol)
        else {
            return Err(format!(
                "No definition for {symbol}, or not a generic symbol"
            ));
        };

        let type_scheme = value.type_scheme.as_ref().unwrap();

        Ok(match (&value.e, type_scheme) {
            (Expression::LambdaExpression(_, expr), TypeScheme { vars, ty }) => {
                assert!(vars.is_empty());
                self.try_format_function(symbol, expr, ty)?
            }
            _ => {
                let type_scheme = value.type_scheme.as_ref().unwrap();
                assert!(type_scheme.vars.is_empty());
                let ty = if type_scheme.ty == Type::Col {
                    Type::Function(FunctionType {
                        params: vec![Type::Int],
                        value: Box::new(Type::Fe),
                    })
                } else {
                    type_scheme.ty.clone()
                };
                // We need a lazy static here because we want symbols to only be
                // evaluated once and the code is not `const` in the general case.
                format!(
                    "lazy_static::lazy_static! {{\n\
                    static ref {}: {} = {};\n\
                    }}\n",
                    escape_symbol(symbol),
                    map_type(&ty),
                    self.format_expr(&value.e)?
                )
            }
        })
    }

    fn try_format_function(
        &mut self,
        name: &str,
        LambdaExpression { params, body, .. }: &LambdaExpression<Expression>,
        ty: &Type,
    ) -> Result<String, String> {
        let (param_types, return_type) = &match ty {
            Type::Function(FunctionType { params, value }) => (params.clone(), (**value).clone()),
            Type::Col => {
                // TODO we assume it is an int -> fe function, even though other
                // types are possible.
                // At some point, the type inference algorithm should store the derived type.
                // Alternatively, we could insert a trait conversion function and store the type
                // in the trait vars.
                (vec![Type::Int], Type::Fe)
            }
            _ => return Err(format!("Expected function type, got {ty}")),
        };

        Ok(format!(
            "fn {}({}) -> {} {{ {} }}\n",
            escape_symbol(name),
            params
                .iter()
                .zip(param_types)
                .map(|(p, t)| format!("{p}: {}", map_type(t)))
                .format(", "),
            map_type(return_type),
            self.format_expr(body)?
        ))
    }

    fn format_expr(&mut self, e: &Expression) -> Result<String, String> {
        Ok(match e {
            Expression::Reference(_, Reference::LocalVar(_id, name)) => name.clone(),
            Expression::Reference(_, Reference::Poly(PolynomialReference { name, type_args })) => {
                let reference = self.request_symbol(name)?;
                let ta = type_args.as_ref().unwrap();
                format!(
                    "{reference}{}",
                    (!ta.is_empty())
                        .then(|| format!("::<{}>", ta.iter().map(map_type).join(", ")))
                        .unwrap_or_default()
                )
            }
            Expression::Number(
                _,
                Number {
                    value,
                    type_: Some(type_),
                },
            ) => match type_ {
                Type::Int => format_unsigned_integer(value),
                Type::Fe => {
                    let val = u64::try_from(value)
                        .map_err(|_| "Large numbers for fe not yet implemented.".to_string())?;
                    format!("FieldElement::from({val}_u64)",)
                }
                Type::Expr => {
                    let val = u64::try_from(value)
                        .map_err(|_| "Large numbers for expr not yet implemented.".to_string())?;
                    format!("Expr::from({val}_u64)")
                }
                _ => unreachable!(),
            },
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
                        format!("(({left}).clone() << usize::try_from(({right}).clone()).unwrap())")
                    }
                    BinaryOperator::ShiftRight => {
                        format!("(({left}).clone() >> usize::try_from(({right}).clone()).unwrap())")
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
        Err(format!(
            "Compiling statements inside blocks is not yet implemented: {s}"
        ))
    }

    /// Returns a string expression evaluating to the value of the symbol.
    /// This is either the escaped name of the symbol or a deref operator
    /// applied to it.
    fn symbol_reference(&self, symbol: &str) -> String {
        let needs_deref = if is_builtin(symbol) {
            false
        } else {
            let (_, def) = self.analyzed.definitions.get(symbol).as_ref().unwrap();
            if let Some(FunctionValueDefinition::Expression(typed_expr)) = def {
                !matches!(typed_expr.e, Expression::LambdaExpression(..))
            } else {
                false
            }
        };
        if needs_deref {
            format!("(*{})", escape_symbol(symbol))
        } else {
            escape_symbol(symbol)
        }
    }
}

fn format_unsigned_integer(n: &BigUint) -> String {
    if let Ok(n) = u64::try_from(n) {
        format!("ibig::IBig::from({n}_u64)")
    } else {
        format!(
            "ibig::IBig::from(ibig::UBig::from_le_bytes(&[{}]))",
            n.to_le_bytes()
                .iter()
                .map(|b| format!("{b}_u8"))
                .format(", ")
        )
    }
}

fn map_type(ty: &Type) -> String {
    match ty {
        Type::Bottom | Type::Bool => format!("{ty}"),
        Type::Int => "ibig::IBig".to_string(),
        Type::Fe => "FieldElement".to_string(),
        Type::String => "String".to_string(),
        Type::Expr => "Expr".to_string(),
        Type::Array(ArrayType { base, length: _ }) => format!("Vec<{}>", map_type(base)),
        Type::Tuple(_) => todo!(),
        Type::Function(ft) => format!(
            "fn({}) -> {}",
            ft.params.iter().map(map_type).join(", "),
            map_type(&ft.value)
        ),
        Type::TypeVar(tv) => tv.to_string(),
        Type::NamedType(path, type_args) => {
            if type_args.is_some() {
                unimplemented!()
            }
            escape_symbol(&path.to_string())
        }
        Type::Col | Type::Inter => unreachable!(),
    }
}

fn is_builtin(symbol: &str) -> bool {
    matches!(
        symbol,
        "std::array::len"
            | "std::check::panic"
            | "std::field::modulus"
            | "std::convert::fe"
            | "std::convert::int"
    )
}

fn try_generate_builtin<T: FieldElement>(symbol: &str) -> Option<String> {
    let code = match symbol {
        "std::array::len" => {
            "<T>(a: Vec<T>) -> ibig::IBig { ibig::IBig::from(a.len()) }".to_string()
        }
        "std::check::panic" => "(s: &str) -> ! { panic!(\"{s}\"); }".to_string(),
        "std::field::modulus" => {
            format!(
                "() -> ibig::IBig {{ {} }}",
                format_unsigned_integer(&T::modulus().to_arbitrary_integer())
            )
        }
        "std::convert::fe" => {
            "(n: ibig::IBig) -> FieldElement { u64::try_from(n).unwrap().into() }".to_string()
        }
        "std::convert::int" => {
            "(n: FieldElement) -> ibig::IBig {{ u64::try_from(n).into() }}".to_string()
        }
        _ => return None,
    };
    Some(format!("fn {}{code}", escape_symbol(symbol)))
}

#[cfg(test)]
mod test {
    use powdr_number::GoldilocksField;
    use powdr_pil_analyzer::analyze_string;

    use pretty_assertions::assert_eq;

    use super::CodeGenerator;

    fn compile(input: &str, syms: &[&str]) -> String {
        let analyzed = analyze_string::<GoldilocksField>(input).unwrap();
        let mut compiler = CodeGenerator::new(&analyzed);
        for s in syms {
            compiler.request_symbol(s).unwrap();
        }
        compiler.generated_code()
    }

    #[test]
    fn empty_code() {
        let result = compile("", &[]);
        assert_eq!(result, "");
    }

    #[test]
    fn simple_fun() {
        let result = compile("let c: int -> int = |i| i;", &["c"]);
        assert_eq!(result, "fn c(i: ibig::IBig) -> ibig::IBig { i }\n");
    }

    #[test]
    fn fun_calls() {
        let result = compile(
            "let c: int -> int = |i| i + 20; let d = |k| c(k * 20);",
            &["c", "d"],
        );
        assert_eq!(
            result,
            "fn c(i: ibig::IBig) -> ibig::IBig { ((i).clone() + (ibig::IBig::from(20_u64)).clone()) }\n\
            \n\
            fn d(k: ibig::IBig) -> ibig::IBig { (c)(((k).clone() * (ibig::IBig::from(20_u64)).clone()).clone()) }\n\
            "
        );
    }
}
