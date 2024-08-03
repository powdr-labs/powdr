use libc::{c_void, dlclose, dlopen, dlsym, RTLD_NOW};
use std::{
    collections::{HashMap, HashSet},
    ffi::CString,
    fs::File,
    io::Write,
    process::Command,
    sync::Arc,
};

use itertools::Itertools;
use powdr_ast::{
    analyzed::{
        Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
        Reference, SymbolKind,
    },
    parsed::{
        display::{format_type_args, quote},
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

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
pub struct GoldilocksBaseFieldConfig;
pub type FieldElement = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;
"#;

// TODO this is the old impl of goldilocks

const CARGO_TOML: &str = r#"
[package]
name = "powdr_constants"
version = "0.1.0"
edition = "2021"

[dependencies]
ark-ff = "0.4.2"
"#;

// TODO crate type dylib?

pub fn generate_fixed_cols<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> HashMap<String, (PolyID, VariablySizedColumn<T>)> {
    let mut compiler = Compiler::new(analyzed);
    for (sym, _) in &analyzed.constant_polys_in_source_order() {
        compiler.request_symbol(&sym.absolute_name);
    }
    let code = format!("{PREAMBLE}\n{}\n", compiler.compiled_symbols());

    let dir = mktemp::Temp::new_dir().unwrap();
    std::fs::create_dir(dir.as_path().join("src")).unwrap();
    std::fs::write(dir.as_path().join("src").join("lib.rs"), code).unwrap();
    Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(dir.as_path())
        .output()
        .unwrap();

    unsafe {
        let lib_path = CString::new(
            dir.as_path()
                .join("target")
                .join("release")
                .join("libpowdr_constants.so")
                .to_str()
                .unwrap(),
        )
        .unwrap();
        let lib = dlopen(lib_path.as_ptr(), RTLD_NOW);
        if lib.is_null() {
            panic!("Failed to load library: {:?}", lib_path);
        }
        for (sym, poly_id) in analyzed.constant_polys_in_source_order() {
            let sym = escape(&sym.absolute_name);
            let sym = CString::new(sym).unwrap();
            let sym = dlsym(lib, sym.as_ptr());
            if sym.is_null() {
                println!("Failed to load symbol: {:?}", sym);
                continue;
            }
            println!("Loaded symbol: {:?}", sym);
            // let sym = sym as *const VariablySizedColumn<T>;
            // cols.insert(sym.absolute_name.clone(), (poly_id, (*sym).clone()));
        }
    }
    todo!()
}

struct Compiler<'a, T> {
    analyzed: &'a Analyzed<T>,
    queue: Vec<String>,
    requested: HashSet<String>,
    failed: HashMap<String, String>,
    symbols: HashMap<String, String>,
}

impl<'a, T> Compiler<'a, T> {
    pub fn new(analyzed: &'a Analyzed<T>) -> Self {
        Self {
            analyzed,
            queue: Default::default(),
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

    pub fn compiled_symbols(self) -> String {
        self.symbols
            .into_iter()
            .map(|(name, code)| code)
            .format("\n\n")
            .to_string()
    }

    fn generate_code(&mut self, symbol: &str) -> Result<String, String> {
        if symbol == "std::check::panic" {
            // TODO should this really panic?
            return Ok("fn std_check_panic(s: &str) -> ! { panic!(\"{s}\"); }".to_string());
        } else if symbol == "std::field::modulus" {
            // TODO depends on T
            return Ok("fn std_field_modulus() -> num_bigint::BigInt { num_bigint::BigInt::from(18446744069414584321_u64) }"
                .to_string());
        } else if symbol == "std::convert::fe" {
            return Ok("fn std_convert_fe(n: num_bigint::BigInt) -> FieldElement {\n    <FieldElement as PrimeField>::BigInt::try_from(n.to_biguint().unwrap()).unwrap().into()\n}"
                .to_string());
        }

        let Some((sym, Some(FunctionValueDefinition::Expression(value)))) =
            self.analyzed.definitions.get(symbol)
        else {
            return Err(format!(
                "No definition for {symbol}, or not a generic symbol"
            ));
        };
        println!("Processing {symbol} = {}", value.e);
        Ok(match &value.type_scheme.as_ref().unwrap() {
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
                format!(
                    "fn {}<{}>({}) -> {} {{ {} }}\n",
                    escape(symbol),
                    vars,
                    params
                        .iter()
                        .zip(param_types)
                        .map(|(p, t)| format!("{}: {}", p, map_type(t)))
                        .format(", "),
                    map_type(return_type),
                    self.format_expr(body)?
                )
            }
            _ => format!(
                "const {}: {} = {};\n",
                escape(symbol),
                map_type(&value.type_scheme.as_ref().unwrap().ty),
                self.format_expr(&value.e)?
            ),
        })
    }

    fn format_expr(&mut self, e: &Expression) -> Result<String, String> {
        Ok(match e {
            Expression::Reference(_, Reference::LocalVar(_id, name)) => name.clone(),
            Expression::Reference(
                _,
                Reference::Poly(PolynomialReference {
                    name,
                    poly_id: _,
                    type_args,
                }),
            ) => {
                self.request_symbol(name)?;
                format!(
                    "{}{}",
                    escape(name),
                    // TODO do all type args work here?
                    type_args
                        .as_ref()
                        .map(|ta| format!("::{}", format_type_args(&ta)))
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
                    self.format_expr(function)?,
                    arguments
                        .iter()
                        .map(|a| self.format_expr(a))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        // TODO these should all be refs
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
                // let params = if *params == vec!["r".to_string()] {
                //     // Hack because rust needs the type
                //     vec!["r: Vec<num_bigint::BigInt>".to_string()]
                // } else {
                //     params.clone()
                // };
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
            _ => return Err(format!("Implement {e}")),
        })
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
