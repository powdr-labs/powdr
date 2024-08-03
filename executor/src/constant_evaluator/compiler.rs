use libc::{c_void, dlclose, dlopen, dlsym, RTLD_NOW};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    collections::{HashMap, HashSet},
    ffi::CString,
    fs::{self, create_dir, File},
    io::Write,
    path,
    process::Command,
    sync::Arc,
    time::Instant,
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
        ArrayLiteral, BinaryOperation, BinaryOperator, BlockExpression, FunctionCall, IfExpression,
        IndexAccess, LambdaExpression, Number, StatementInsideBlock, UnaryOperation,
    },
};
use powdr_number::FieldElement;

use crate::constant_evaluator::{MAX_DEGREE_LOG, MIN_DEGREE_LOG};

use super::VariablySizedColumn;

const PREAMBLE: &str = r#"
#![allow(unused_parens)]
use ark_ff::{BigInt, BigInteger, Fp64, MontBackend, MontConfig, PrimeField};

#[derive(MontConfig)]
#[modulus = "18446744069414584321"]
#[generator = "7"]
struct GoldilocksBaseFieldConfig;
type FieldElement = Fp64<MontBackend<GoldilocksBaseFieldConfig, 1>>;
"#;

// TODO this is the old impl of goldilocks

const CARGO_TOML: &str = r#"
[package]
name = "powdr_constants"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["dylib"]

[dependencies]
ark-ff = "0.4.2"
num-bigint = { version = "0.4.3", features = ["serde"] }
num-traits = "0.2.15"
"#;

// TODO crate type dylib?

pub fn generate_fixed_cols<T: FieldElement>(
    analyzed: &Analyzed<T>,
) -> HashMap<String, (PolyID, VariablySizedColumn<T>)> {
    let mut compiler = Compiler::new(analyzed);
    let mut glue = String::new();
    for (sym, _) in &analyzed.constant_polys_in_source_order() {
        // ignore err
        if let Err(e) = compiler.request_symbol(&sym.absolute_name) {
            println!("Failed to compile {}: {e}", &sym.absolute_name);
        }
    }
    for (sym, _) in &analyzed.constant_polys_in_source_order() {
        // TODO escape?
        if compiler.is_compiled(&sym.absolute_name) {
            // TODO it is a rust function, can we use a more complex type as well?
            // TODO only works for goldilocks
            glue.push_str(&format!(
                r#"
                #[no_mangle]
                pub extern fn extern_{}(i: u64) -> u64 {{
                    {}(num_bigint::BigInt::from(i)).into_bigint().0[0]
                }}
                "#,
                escape(&sym.absolute_name),
                escape(&sym.absolute_name),
            ));
        }
    }

    let code = format!("{PREAMBLE}\n{}\n{glue}\n", compiler.compiled_symbols());
    println!("Compiled code:\n{code}");

    //let dir = mktemp::Temp::new_dir().unwrap();
    let _ = fs::remove_dir_all("/tmp/powdr_constants");
    fs::create_dir("/tmp/powdr_constants").unwrap();
    let dir = path::Path::new("/tmp/powdr_constants");
    fs::write(dir.join("Cargo.toml"), CARGO_TOML).unwrap();
    fs::create_dir(dir.join("src")).unwrap();
    fs::write(dir.join("src").join("lib.rs"), code).unwrap();
    let out = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(dir)
        .output()
        .unwrap();
    out.stderr.iter().for_each(|b| print!("{}", *b as char));
    if !out.status.success() {
        panic!("Failed to compile.");
    }

    let mut columns = HashMap::new();
    unsafe {
        let lib_path = CString::new(
            dir.join("target")
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
        let start = Instant::now();
        for (poly, value) in analyzed.constant_polys_in_source_order() {
            let sym = format!("extern_{}", escape(&poly.absolute_name));
            let sym = CString::new(sym).unwrap();
            let sym = dlsym(lib, sym.as_ptr());
            if sym.is_null() {
                println!("Failed to load symbol: {:?}", sym);
                continue;
            }
            println!("Loaded symbol: {:?}", sym);
            let fun = std::mem::transmute::<*mut c_void, fn(u64) -> u64>(sym);
            let degrees = if let Some(degree) = poly.degree {
                vec![degree]
            } else {
                (MIN_DEGREE_LOG..=MAX_DEGREE_LOG)
                    .map(|degree_log| 1 << degree_log)
                    .collect::<Vec<_>>()
            };

            let col_values = degrees
                .into_iter()
                .map(|degree| {
                    (0..degree)
                        .into_par_iter()
                        .map(|i| T::from(fun(i as u64)))
                        .collect::<Vec<T>>()
                })
                .collect::<Vec<_>>()
                .into();
            columns.insert(poly.absolute_name.clone(), (poly.into(), col_values));
        }
        log::info!(
            "Fixed column generation (without compilation and loading time) took {}s",
            start.elapsed().as_secs_f32()
        );
    }
    columns
}

struct Compiler<'a, T> {
    analyzed: &'a Analyzed<T>,
    requested: HashSet<String>,
    failed: HashMap<String, String>,
    symbols: HashMap<String, String>,
}

impl<'a, T> Compiler<'a, T> {
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
                println!("Generated code for {name}");
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
                format!(
                    "fn {}<{}>({}) -> {} {{ {} }}\n",
                    escape(symbol),
                    vars,
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
