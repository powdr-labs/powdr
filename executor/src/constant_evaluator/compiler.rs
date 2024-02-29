use std::{collections::HashMap, io::Write};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
        Reference, SymbolKind,
    },
    parsed::{
        types::{ArrayType, FunctionType, Type, TypeScheme},
        ArrayLiteral, BinaryOperation, BinaryOperator, FunctionCall, IfExpression, IndexAccess,
        LambdaExpression, Number, UnaryOperation,
    },
};
use powdr_number::FieldElement;

use super::VariablySizedColumn;

const PREAMBLE: &str = r#"
#![allow(unused_parens)]
use ark_ff::{BigInt, BigInteger, Fp64, MontBackend, MontConfig, PrimeField};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use std::io::{BufWriter, Write};
use std::fs::File;

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
pub struct GoldilocksBaseFieldConfig;
pub type FieldElement = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;
"#;

pub fn generate_fixed_cols<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> HashMap<String, (PolyID, VariablySizedColumn<T>)> {
    let definitions = process_definitions(analyzed);
    let degree = analyzed.degree();
    // TODO also eval other cols
    let main_func = format!(
        "
fn main() {{
    let data = (0..{degree})
    .into_par_iter()
    .map(|i| {{
        main_inv(num_bigint::BigInt::from(i))
    }})
    .collect::<Vec<_>>();
    let mut writer = BufWriter::new(File::create(\"./constants.bin\").unwrap());
    for i in 0..{degree} {{
        writer
            .write_all(&BigInt::from(data[i]).to_bytes_le())
            .unwrap();
    }}
}}
"
    );
    let result = format!("{PREAMBLE}\n{definitions}\n{main_func}\n");
    // write result to a temp file
    let mut file = std::fs::File::create("/tmp/te/src/main.rs").unwrap();
    file.write_all(result.as_bytes()).unwrap();
    Default::default()
}

pub fn process_definitions<T: FieldElement>(analyzed: &Analyzed<T>) -> String {
    let mut result = String::new();
    for (name, (sym, value)) in &analyzed.definitions {
        if name == "std::check::panic" {
            result.push_str("fn std_check_panic(s: &str) -> ! { panic!(\"{s}\"); }");
        } else if name == "std::field::modulus" {
            result.push_str("fn std_field_modulus() -> num_bigint::BigInt { num_bigint::BigInt::from(18446744069414584321_u64) }");
        } else if name == "std::convert::fe" {
            result.push_str("fn std_convert_fe(n: num_bigint::BigInt) -> FieldElement {\n    <FieldElement as PrimeField>::BigInt::try_from(n.to_biguint().unwrap()).unwrap().into()\n}");
        } else if let Some(FunctionValueDefinition::Expression(value)) = value {
            println!("Processing {name} = {}", value.e);
            let type_scheme = if sym.kind == SymbolKind::Poly(PolynomialType::Constant) {
                TypeScheme {
                    vars: Default::default(),
                    ty: Type::Function(FunctionType {
                        params: vec![Type::Int],
                        value: Box::new(Type::Fe),
                    }),
                }
            } else {
                value.type_scheme.clone().unwrap()
            };
            match &type_scheme {
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
                        todo!("value of fun: {}", value.e)
                    };
                    result.push_str(&format!(
                        "fn {}<{}>({}) -> {} {{ {} }}\n",
                        escape(name),
                        vars,
                        params
                            .iter()
                            .zip(param_types)
                            .map(|(p, t)| format!("{}: {}", p, map_type(t)))
                            .format(", "),
                        map_type(return_type),
                        format_expr(body)
                    ));
                }
                _ => {
                    result.push_str(&format!(
                        "const {}: {} = {};\n",
                        escape(name),
                        map_type(&value.type_scheme.as_ref().unwrap().ty),
                        format_expr(&value.e)
                    ));
                }
            }
        }
    }

    result
}

fn format_expr(e: &Expression) -> String {
    match e {
        Expression::Reference(_, Reference::LocalVar(_id, name)) => name.clone(),
        Expression::Reference(
            _,
            Reference::Poly(PolynomialReference {
                name,
                poly_id: _,
                type_args: _,
            }),
        ) => escape(name), // TOOD use type args if needed.
        Expression::Number(
            _,
            Number {
                value,
                type_: Some(type_),
            },
        ) => match type_ {
            Type::Int => format!("num_bigint::BigInt::from({value}_u64)"),
            Type::Fe => format!("FieldElement::from({value}_u64)"),
            Type::Expr => format!("Expr::from({value}_u64)"),
            Type::TypeVar(t) => format!("{t}::from({value}_u64)"),
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
                format_expr(function),
                arguments
                    .iter()
                    .map(format_expr)
                    .map(|x| format!("{x}.clone()"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        Expression::BinaryOperation(_, BinaryOperation { left, op, right }) => {
            let left = format_expr(left);
            let right = format_expr(right);
            match op {
                BinaryOperator::ShiftLeft => {
                    format!("(({left}).clone() << u32::try_from(({right}).clone()).unwrap())")
                }
                _ => format!("(({left}).clone() {op} ({right}).clone())"),
            }
        }
        Expression::UnaryOperation(_, UnaryOperation { op, expr }) => {
            format!("({op} ({}).clone())", format_expr(expr))
        }
        Expression::IndexAccess(_, IndexAccess { array, index }) => {
            format!(
                "{}[usize::try_from({}).unwrap()].clone()",
                format_expr(array),
                format_expr(index)
            )
        }
        Expression::LambdaExpression(_, LambdaExpression { params, body, .. }) => {
            // let params = if *params == vec!["r".to_string()] {
            //     // Hack because rust needs the type
            //     vec!["r: Vec<num_bigint::BigInt>".to_string()]
            // } else {
            //     params.clone()
            // };
            format!(
                "|{}| {{ {} }}",
                params.iter().format(", "),
                format_expr(body)
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
                format_expr(condition),
                format_expr(body),
                format_expr(else_body)
            )
        }
        Expression::ArrayLiteral(_, ArrayLiteral { items }) => {
            format!(
                "vec![{}]",
                items.iter().map(format_expr).collect::<Vec<_>>().join(", ")
            )
        }
        Expression::String(_, s) => format!("{s:?}"), // TODO does this quote properly?
        Expression::Tuple(_, items) => format!(
            "({})",
            items.iter().map(format_expr).collect::<Vec<_>>().join(", ")
        ),
        _ => panic!("Implement {e}"),
    }
}

fn escape(s: &str) -> String {
    s.replace('.', "_").replace("::", "_")
}

fn map_type(ty: &Type) -> String {
    match ty {
        Type::Bottom | Type::Bool => format!("{ty}"),
        Type::Int => "num_bigint::BigInt".to_string(),
        Type::Fe => "FieldElement".to_string(),
        Type::String => "String".to_string(),
        Type::Col => unreachable!(),
        Type::Expr => "Expr".to_string(),
        Type::Array(ArrayType { base, length: _ }) => format!("Vec<{}>", map_type(base)),
        Type::Tuple(_) => todo!(),
        Type::Function(ft) => todo!("Type {ft}"),
        Type::TypeVar(tv) => tv.to_string(),
        Type::NamedType(_path, _type_args) => todo!(),
    }
}
