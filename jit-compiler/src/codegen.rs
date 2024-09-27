use std::collections::HashMap;

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

    /// Request a symbol to be compiled. The code can later be retrieved
    /// via `compiled_symbols`.
    /// In the error case, `self` can still be used to compile other symbols.
    pub fn request_symbol(&mut self, name: &str) -> Result<(), String> {
        match self.symbols.get(name) {
            Some(Ok(_)) => Ok(()),
            Some(Err(e)) => Err(e.clone()),
            None => {
                let name = name.to_string();
                self.symbols.insert(name.clone(), Ok(None));
                let to_insert;
                let to_return;
                match self.generate_code(&name) {
                    Ok(code) => {
                        to_insert = Ok(Some(code));
                        to_return = Ok(());
                    }
                    Err(err) => {
                        to_insert = Err(err.clone());
                        to_return = Err(err);
                    }
                }
                self.symbols.insert(name, to_insert);
                to_return
            }
        }
    }

    /// Returns the concatenation of all successfully compiled symbols.
    pub fn compiled_symbols(self) -> String {
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
                assert!(vars.is_empty());
                self.try_format_function(symbol, &param_types, return_type.as_ref(), &value.e)?
            }
            TypeScheme {
                vars,
                ty: Type::Col,
            } => {
                assert!(vars.is_empty());
                // TODO we assume it is an int -> int function.
                // The type inference algorithm should store the derived type.
                // Alternatively, we insert a trait conversion function and store the type
                // in the trait vars.
                self.try_format_function(symbol, &[Type::Int], &Type::Int, &value.e)?
            }
            _ => format!(
                "const {}: {} = {};\n",
                escape_symbol(symbol),
                map_type(&value.type_scheme.as_ref().unwrap().ty),
                self.format_expr(&value.e)?
            ),
        })
    }

    fn try_format_function(
        &mut self,
        name: &str,
        param_types: &[Type],
        return_type: &Type,
        expr: &Expression,
    ) -> Result<String, String> {
        let Expression::LambdaExpression(_, LambdaExpression { params, body, .. }) = expr else {
            return Err(format!("Expected lambda expression for {name}, got {expr}",));
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

    fn try_generate_builtin(&self, symbol: &str) -> Option<String> {
        let code = match symbol {
            "std::check::panic" => Some("(s: &str) -> ! { panic!(\"{s}\"); }".to_string()),
            "std::field::modulus" => {
                Some(format!("() -> ibig::IBig {{ {} }}", format_number(&T::modulus().to_arbitrary_integer())))
            }
            "std::convert::fe" => Some("(n: ibig::IBig) -> FieldElement {\n    <FieldElement as PrimeField>::BigInt::try_from(n.to_biguint().unwrap()).unwrap().into()\n}"
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
                if *type_ == Type::Int {
                    format_number(value)
                } else {
                    let value = u64::try_from(value).unwrap_or_else(|_| unimplemented!());
                    match type_ {
                        Type::Fe => format!("FieldElement::from({value}_u64)"),
                        Type::Expr => format!("Expr::from({value}_u64)"),
                        _ => unreachable!(),
                    }
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
}

fn format_number(n: &BigUint) -> String {
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
        Type::Function(ft) => todo!("Type {ft}"),
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
        compiler.compiled_symbols()
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
