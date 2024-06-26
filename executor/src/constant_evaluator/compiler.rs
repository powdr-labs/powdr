use std::io::Write;

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        types::{ArrayType, FunctionType, Type, TypeScheme},
        Analyzed, Expression, FunctionValueDefinition, PolynomialReference, PolynomialType,
        Reference, SymbolKind,
    },
    parsed::{
        ArrayLiteral, BinaryOperator, FunctionCall, IfExpression, IndexAccess, LambdaExpression,
        TypeName,
    },
};
use powdr_number::FieldElement;

const PREAMBLE: &'static str = r#"
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

pub fn generate<T: FieldElement>(analyzed: &Analyzed<T>) {
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
}

pub fn process_definitions<T: FieldElement>(analyzed: &Analyzed<T>) -> String {
    let mut result = String::new();
    for (name, (sym, value)) in &analyzed.definitions {
        if name == "std::check::panic" {
            result.push_str(&format!(
                "fn std_check_panic(s: &str) -> ! {{ panic!(\"{{s}}\"); }}"
            ));
        } else if name == "std::field::modulus" {
            result.push_str(&format!(
                "fn std_field_modulus() -> num_bigint::BigInt {{ num_bigint::BigInt::from(18446744069414584321_u64) }}"
            ));
        } else if name == "std::convert::fe" {
            result.push_str(&format!(
                "fn std_convert_fe(n: num_bigint::BigInt) -> FieldElement {{\n    <FieldElement as PrimeField>::BigInt::try_from(n.to_biguint().unwrap()).unwrap().into()\n}}"
            ));
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
                    let Expression::LambdaExpression(LambdaExpression { params, body }) = &value.e
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
                        map_type(&return_type),
                        format_expr(&body)
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

fn format_expr<T: FieldElement>(e: &Expression<T>) -> String {
    match e {
        Expression::Reference(Reference::LocalVar(id, name)) => name.clone(),
        Expression::Reference(Reference::Poly(PolynomialReference {
            name,
            poly_id,
            generic_args,
        })) => escape(name), // TOOD use generic args if needed.
        Expression::Number(n, Some(ty)) => match ty {
            TypeName::Int => format!("num_bigint::BigInt::from({n}_u64)"),
            TypeName::Fe => format!("FieldElement::from({n}_u64)"),
            TypeName::Expr => format!("Expr::from({n}_u64)"),
            TypeName::TypeVar(t) => format!("{t}::from({n}_u64)"),
            _ => unreachable!(),
        },
        Expression::FunctionCall(FunctionCall {
            function,
            arguments,
        }) => {
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
        Expression::BinaryOperation(left, op, right) => {
            let left = format_expr(&left);
            let right = format_expr(&right);
            match op {
                BinaryOperator::ShiftLeft => {
                    format!("(({left}).clone() << u32::try_from(({right}).clone()).unwrap())")
                }
                _ => format!("(({left}).clone() {op} ({right}).clone())"),
            }
        }
        Expression::UnaryOperation(op, inner) => {
            format!("({op} ({}).clone())", format_expr(&inner))
        }
        Expression::IndexAccess(IndexAccess { array, index }) => {
            format!(
                "{}[usize::try_from({}).unwrap()].clone()",
                format_expr(&array),
                format_expr(&index)
            )
        }
        Expression::LambdaExpression(LambdaExpression { params, body }) => {
            let params = if *params == vec!["r".to_string()] {
                // Hack because rust needs the type
                vec!["r: Vec<num_bigint::BigInt>".to_string()]
            } else {
                params.clone()
            };
            format!("|{}| {{ {} }}", params.join(", "), format_expr(&body))
        }
        Expression::IfExpression(IfExpression {
            condition,
            body,
            else_body,
        }) => {
            format!(
                "if {} {{ {} }} else {{ {} }}",
                format_expr(&condition),
                format_expr(&body),
                format_expr(&else_body)
            )
        }
        Expression::ArrayLiteral(ArrayLiteral { items }) => {
            format!(
                "vec![{}]",
                items.iter().map(format_expr).collect::<Vec<_>>().join(", ")
            )
        }
        Expression::String(s) => format!("{:?}", s), // TODO does this quote properly?
        Expression::Tuple(items) => format!(
            "({})",
            items.iter().map(format_expr).collect::<Vec<_>>().join(", ")
        ),
        _ => panic!("Implement {e}"),
    }
}

fn escape(s: &str) -> String {
    s.replace(".", "_").replace("::", "_")
}

fn map_type(ty: &Type) -> String {
    match ty {
        Type::Bottom | Type::Bool => format!("{ty}"),
        Type::Int => format!("num_bigint::BigInt"),
        Type::Fe => format!("FieldElement"),
        Type::String => format!("String"),
        Type::Col => unreachable!(),
        Type::Expr => format!("Expr"),
        Type::Constr => format!("Constr"),
        Type::Array(ArrayType { base, length: _ }) => format!("Vec<{}>", map_type(base)),
        Type::Tuple(_) => todo!(),
        Type::Function(ft) => todo!("Type {ft}"),
        Type::TypeVar(tv) => format!("{tv}"),
    }
}
