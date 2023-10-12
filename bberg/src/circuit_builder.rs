use std::{fmt::Display, process::id};

use ast::{analyzed::Identity, parsed::BinaryOperator};
use itertools::Itertools;
use num_bigint::BigUint;

use ast::analyzed::{Analyzed, Expression, IdentityKind, Reference, SelectedExpressions};
use num_traits::{identities, One};
use number::{BigInt, FieldElement};

// use super::circuit_data::CircuitData;

use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
) -> String {
    let all_rows = get_all_row_names(fixed, witness);

    let row_type = create_row_type(&all_rows);

    let identities = create_identities(&analyzed.identities, &all_rows);

    // These are all of the exotic ish data structures we will need
    // let mut lookups = vec![];
    // let mut perms = vec![];
    // let mut polys = vec![];

    // // From all of the fixed and witness columns we have, we want to create the row object for bb

    // // Note: we do not have lookups yet
    // assert!(lookups.len() == 0, "lookups not implemented");

    "ye-haw".to_owned()
}

// As all of our rows are annotated, we should be able to create
// the row type by hand here
// The row type is a combination of the fixed and witness columns

// The include statements required for a new relation file
fn relation_includes() -> &'static str {
    r#"
#pragma once
#include "../relation_parameters.hpp"
#include "../relation_types.hpp"
"#
}

// Yucky that everything is allocated into vecs here
fn create_row_type_items<T: Display>(names: &Vec<T>) -> Vec<String> {
    names
        .iter()
        .map(|name| format!("    FF {};", name))
        .collect::<Vec<_>>()
}

fn get_all_row_names<F: FieldElement>(
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
) -> Vec<String> {
    let fixed_names: Vec<String> = fixed.iter().map(|(name, _)| (*name).to_owned()).collect();
    let witness_names: Vec<String> = witness.iter().map(|(name, _)| (*name).to_owned()).collect();

    let shift_names: Vec<String> = witness
        .iter()
        .map(|(name, _)| format!("{}_shift", *name))
        .collect();

    // h/t kev
    [fixed_names, witness_names, shift_names]
        .into_iter()
        .flatten()
        .collect()
}

// Each vm will need to have a row which is a combination of all of the witness columns
fn create_row_type(all_rows: &Vec<String>) -> String {
    let all_annotated = create_row_type_items(all_rows);

    let row_type = format!(
        "template <typename FF> struct Row {{ \n{}\n }}",
        all_annotated.join("\n"),
    );

    println!("{}", row_type);
    row_type
}

// Get the boiler plate to access rows
fn get_cols_in_identity(row_index: usize, all_rows: &Vec<String>) -> String {
    let template = format!("\nusing View = typename std::tuple_element<{}, typename AccumulatorTypes::AccumulatorViews>::type;\n", row_index);
    let col_accesses: Vec<String> = all_rows
        .iter()
        .map(|col_name| format!("auto {} = View(new_term.{});\n", col_name, col_name))
        .collect();

    return template + &col_accesses.join("\n");
}

fn create_identity<F: FieldElement>(
    row_index: usize,
    expression: SelectedExpressions<F>,
    all_rows: &Vec<String>,
) -> String {
    // We want to read the types of operators and then create the appropiate code

    "output".to_owned()
}

/// Todo, eventually these will need to be siloed based on the file name they are in
fn create_identities<F: FieldElement>(
    identities: &Vec<Identity<F>>,
    all_rows: &Vec<String>,
) -> String {
    // We only want the expressions for now
    // When we have a poly type, we only need the left side of it
    let expressions = identities
        .iter()
        .filter_map(|identity| {
            if identity.kind == IdentityKind::Polynomial {
                Some(identity.left.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    for expression in expressions {
        dbg!(&expression);
    }

    "The output".to_owned()
}

// ///  &_analyzed.identities = [
// Identity {
//     id: 0,
//     kind: Polynomial,
//     source: SourceRef {
//         file: "fibonacci.pil",
//         line: 12,
//     },
//     left: SelectedExpressions {
//         selector: Some(
//             BinaryOperation(
//                 Reference(
//                     Poly(
//                         PolynomialReference {
//                             name: "Fibonacci.ISLAST",
//                             poly_id: Some(
//                                 PolyID {
//                                     id: 0,
//                                     ptype: Constant,
//                                 },
//                             ),
//                             index: None,
//                             next: false,
//                         },
//                     ),
//                 ),
//                 Mul,
//                 BinaryOperation(
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.y",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 1,
//                                         ptype: Committed,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: true,
//                             },
//                         ),
//                     ),
//                     Sub,
//                     Number(
//                         Bn254Field {
//                             value: BigInt(
//                                 [
//                                     1,
//                                     0,
//                                     0,
//                                     0,
//                                 ],
//                             ),
//                         },
//                     ),
//                 ),
//             ),
//         ),
//         expressions: [],
//     },
//     right: SelectedExpressions {
//         selector: None,
//         expressions: [],
//     },
// },
// Identity {
//     id: 1,
//     kind: Polynomial,
//     source: SourceRef {
//         file: "fibonacci.pil",
//         line: 13,
//     },
//     left: SelectedExpressions {
//         selector: Some(
//             BinaryOperation(
//                 Reference(
//                     Poly(
//                         PolynomialReference {
//                             name: "Fibonacci.ISLAST",
//                             poly_id: Some(
//                                 PolyID {
//                                     id: 0,
//                                     ptype: Constant,
//                                 },
//                             ),
//                             index: None,
//                             next: false,
//                         },
//                     ),
//                 ),
//                 Mul,
//                 BinaryOperation(
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.x",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 0,
//                                         ptype: Committed,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: true,
//                             },
//                         ),
//                     ),
//                     Sub,
//                     Number(
//                         Bn254Field {
//                             value: BigInt(
//                                 [
//                                     1,
//                                     0,
//                                     0,
//                                     0,
//                                 ],
//                             ),
//                         },
//                     ),
//                 ),
//             ),
//         ),
//         expressions: [],
//     },
//     right: SelectedExpressions {
//         selector: None,
//         expressions: [],
//     },
// },
// Identity {
//     id: 2,
//     kind: Polynomial,
//     source: SourceRef {
//         file: "fibonacci.pil",
//         line: 15,
//     },
//     left: SelectedExpressions {
//         selector: Some(
//             BinaryOperation(
//                 BinaryOperation(
//                     Number(
//                         Bn254Field {
//                             value: BigInt(
//                                 [
//                                     1,
//                                     0,
//                                     0,
//                                     0,
//                                 ],
//                             ),
//                         },
//                     ),
//                     Sub,
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.ISLAST",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 0,
//                                         ptype: Constant,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: false,
//                             },
//                         ),
//                     ),
//                 ),
//                 Mul,
//                 BinaryOperation(
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.x",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 0,
//                                         ptype: Committed,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: true,
//                             },
//                         ),
//                     ),
//                     Sub,
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.y",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 1,
//                                         ptype: Committed,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: false,
//                             },
//                         ),
//                     ),
//                 ),
//             ),
//         ),
//         expressions: [],
//     },
//     right: SelectedExpressions {
//         selector: None,
//         expressions: [],
//     },
// },
// Identity {
//     id: 3,
//     kind: Polynomial,
//     source: SourceRef {
//         file: "fibonacci.pil",
//         line: 16,
//     },
//     left: SelectedExpressions {
//         selector: Some(
//             BinaryOperation(
//                 BinaryOperation(
//                     Number(
//                         Bn254Field {
//                             value: BigInt(
//                                 [
//                                     1,
//                                     0,
//                                     0,
//                                     0,
//                                 ],
//                             ),
//                         },
//                     ),
//                     Sub,
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.ISLAST",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 0,
//                                         ptype: Constant,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: false,
//                             },
//                         ),
//                     ),
//                 ),
//                 Mul,
//                 BinaryOperation(
//                     Reference(
//                         Poly(
//                             PolynomialReference {
//                                 name: "Fibonacci.y",
//                                 poly_id: Some(
//                                     PolyID {
//                                         id: 1,
//                                         ptype: Committed,
//                                     },
//                                 ),
//                                 index: None,
//                                 next: true,
//                             },
//                         ),
//                     ),
//                     Sub,
//                     BinaryOperation(
//                         Reference(
//                             Poly(
//                                 PolynomialReference {
//                                     name: "Fibonacci.x",
//                                     poly_id: Some(
//                                         PolyID {
//                                             id: 0,
//                                             ptype: Committed,
//                                         },
//                                     ),
//                                     index: None,
//                                     next: false,
//                                 },
//                             ),
//                         ),
//                         Add,
//                         Reference(
//                             Poly(
//                                 PolynomialReference {
//                                     name: "Fibonacci.y",
//                                     poly_id: Some(
//                                         PolyID {
//                                             id: 1,
//                                             ptype: Committed,
//                                         },
//                                     ),
//                                     index: None,
//                                     next: false,
//                                 },
//                             ),
//                         ),
//                     ),
//                 ),
//             ),
//         ),
//         expressions: [],
//     },
//     right: SelectedExpressions {
//         selector: None,
//         expressions: [],
//     },
// },
// ]
