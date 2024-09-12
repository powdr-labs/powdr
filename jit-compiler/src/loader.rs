// use libc::{c_void, dlclose, dlopen, dlsym, RTLD_NOW};
// use rayon::iter::{IntoParallelIterator, ParallelIterator};
// use std::{
//     collections::{HashMap, HashSet},
//     ffi::CString,
//     fs::{self, create_dir, File},
//     io::Write,
//     path,
//     process::Command,
//     sync::Arc,
//     time::Instant,
// };

// use itertools::Itertools;
// use powdr_ast::{
//     analyzed::{
//         Analyzed, Expression, FunctionValueDefinition, PolyID, PolynomialReference, PolynomialType,
//         Reference, SymbolKind,
//     },
//     parsed::{
//         display::{format_type_args, quote},
//         types::{ArrayType, FunctionType, Type, TypeScheme},
//         ArrayLiteral, BinaryOperation, BinaryOperator, BlockExpression, FunctionCall, IfExpression,
//         IndexAccess, LambdaExpression, Number, StatementInsideBlock, UnaryOperation,
//     },
// };
// use powdr_number::FieldElement;

// // pub fn generate_fixed_cols<T: FieldElement>(
// //     analyzed: &Analyzed<T>,
// // ) -> HashMap<String, (PolyID, VariablySizedColumn<T>)> {
// //     let mut compiler = Compiler::new(analyzed);
// //     let mut glue = String::new();
// //     for (sym, _) in &analyzed.constant_polys_in_source_order() {
// //         // ignore err
// //         if let Err(e) = compiler.request_symbol(&sym.absolute_name) {
// //             println!("Failed to compile {}: {e}", &sym.absolute_name);
// //         }
// //     }
// //     for (sym, _) in &analyzed.constant_polys_in_source_order() {
// //         // TODO escape?
// //         if compiler.is_compiled(&sym.absolute_name) {
// //             // TODO it is a rust function, can we use a more complex type as well?
// //             // TODO only works for goldilocks
// //             glue.push_str(&format!(
// //                 r#"
//                 #[no_mangle]
//                 pub extern fn extern_{}(i: u64) -> u64 {{
//                     {}(num_bigint::BigInt::from(i)).into_bigint().0[0]
//                 }}
//                 "#,
//                 escape(&sym.absolute_name),
//                 escape(&sym.absolute_name),
//             ));
//         }
//     }

//     let code = format!("{PREAMBLE}\n{}\n{glue}\n", compiler.compiled_symbols());
//     println!("Compiled code:\n{code}");

//     //let dir = mktemp::Temp::new_dir().unwrap();
//     let _ = fs::remove_dir_all("/tmp/powdr_constants");
//     fs::create_dir("/tmp/powdr_constants").unwrap();
//     let dir = path::Path::new("/tmp/powdr_constants");
//     fs::write(dir.join("Cargo.toml"), CARGO_TOML).unwrap();
//     fs::create_dir(dir.join("src")).unwrap();
//     fs::write(dir.join("src").join("lib.rs"), code).unwrap();
//     let out = Command::new("cargo")
//         .arg("build")
//         .arg("--release")
//         .current_dir(dir)
//         .output()
//         .unwrap();
//     out.stderr.iter().for_each(|b| print!("{}", *b as char));
//     if !out.status.success() {
//         panic!("Failed to compile.");
//     }

//     let mut columns = HashMap::new();
//     unsafe {
//         let lib_path = CString::new(
//             dir.join("target")
//                 .join("release")
//                 .join("libpowdr_constants.so")
//                 .to_str()
//                 .unwrap(),
//         )
//         .unwrap();
//         let lib = dlopen(lib_path.as_ptr(), RTLD_NOW);
//         if lib.is_null() {
//             panic!("Failed to load library: {:?}", lib_path);
//         }
//         let start = Instant::now();
//         for (poly, value) in analyzed.constant_polys_in_source_order() {
//             let sym = format!("extern_{}", escape(&poly.absolute_name));
//             let sym = CString::new(sym).unwrap();
//             let sym = dlsym(lib, sym.as_ptr());
//             if sym.is_null() {
//                 println!("Failed to load symbol: {:?}", sym);
//                 continue;
//             }
//             println!("Loaded symbol: {:?}", sym);
//             let fun = std::mem::transmute::<*mut c_void, fn(u64) -> u64>(sym);
//             let degrees = if let Some(degree) = poly.degraee {
//                 vec![degree]
//             } else {
//                 (MIN_DEGREE_LOG..=MAX_DEGREE_LOG)
//                     .map(|degree_log| 1 << degree_log)
//                     .collect::<Vec<_>>()
//             };

//             let col_values = degrees
//                 .into_iter()
//                 .map(|degree| {
//                     (0..degree)
//                         .into_par_iter()
//                         .map(|i| T::from(fun(i as u64)))
//                         .collect::<Vec<T>>()
//                 })
//                 .collect::<Vec<_>>()
//                 .into();
//             columns.insert(poly.absolute_name.clone(), (poly.into(), col_values));
//         }
//         log::info!(
//             "Fixed column generation (without compilation and loading time) took {}s",
//             start.elapsed().as_secs_f32()
//         );
//     }
//     columns
// }
