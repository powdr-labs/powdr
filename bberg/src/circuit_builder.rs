use std::fs::File;
use std::{fmt::Display, io::Write, process::id};

use ast::{analyzed::Identity, asm_analysis::DegreeStatement, parsed::BinaryOperator};
use itertools::Itertools;
use num_bigint::BigUint;

use ast::analyzed::{Analyzed, Expression, IdentityKind, Reference, SelectedExpressions};
use num_traits::{identities, One};
use number::{BigInt, DegreeType, FieldElement};

// use super::circuit_data::CircuitData;

use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

use crate::{flavor_builder, trace_builder};

pub struct BBFiles {
    file_name: String,
    relation_hpp: String,
    arith_hpp: String,
    trace_hpp: String,
    flavor_hpp: String,
}

impl BBFiles {
    pub fn write(&self) {
        self.write_file(
            &format!("{}.hpp", self.file_name),
            self.relation_hpp.to_owned(),
        );
        self.write_file(
            &format!("{}_arith.hpp", self.file_name),
            self.arith_hpp.to_owned(),
        );
        self.write_file(
            &format!("{}_trace.hpp", self.file_name),
            self.trace_hpp.to_owned(),
        );
        self.write_file(
            &format!("{}_flavor.hpp", self.file_name),
            self.flavor_hpp.to_owned(),
        );
    }

    fn write_file(&self, filename: &str, contents: String) {
        println!("Writing file: {}", filename);
        let mut file = File::create(filename).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }
}

pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
) -> BBFiles {
    let all_cols = get_all_col_names(fixed, witness);
    let fixed_names = fixed
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();
    let witness_names = witness
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();

    let num_cols = all_cols.len();

    let row_type = create_row_type(&all_cols);

    let (subrelations, identities) = create_identities(&analyzed.identities, &all_cols);

    let file_name: &str = "ExampleRelation";

    // ----------------------- Create the relation file -----------------------
    let relation_hpp = create_relation_hpp(file_name, &subrelations, &identities, &row_type); // TODO: do we need this

    // ----------------------- Create the arithmetization file -----------------------
    let arith_hpp = create_arith_boilerplate_file(file_name, num_cols);

    // ----------------------- Create the read from powdr columns file -----------------------
    let trace_hpp = trace_builder::create_trace_buidler(file_name, fixed, witness);

    let flavor_hpp =
        flavor_builder::create_flavor_hpp(file_name, all_cols, &fixed_names, &witness_names);

    // These are all of the exotic ish data structures we will need
    // let mut lookups = vec![];
    // let mut perms = vec![];
    // let mut polys = vec![];

    // // From all of the fixed and witness columns we have, we want to create the row object for bb

    // // Note: we do not have lookups yet
    // assert!(lookups.len() == 0, "lookups not implemented");

    BBFiles {
        file_name: file_name.to_owned(),
        relation_hpp,
        arith_hpp,
        trace_hpp,
        flavor_hpp,
    }
}

// We have no selectors so we can easily create a boilerplate file
fn create_arith_boilerplate_file(name: &str, num_cols: usize) -> String {
    format!(
        "
namespace arithmetization {{
    class {name}Arithmetization : public Arithmetization<{num_cols}, 0> {{
        public:
            using FF = barretenberg::fr;
            struct Selectors {{}};
    }};
}} // namespace arithmetization
"
    )
}

fn create_relation_hpp(
    name: &str,
    sub_relations: &Vec<String>,
    identities: &Vec<BBIdentity>,
    row_type: &String,
) -> String {
    let includes = relation_includes();
    let class_boilerplate = relation_class_boilerplate(name, sub_relations, identities);
    let export = get_export(name);

    format!(
        "{includes}
namespace proof_system::{name}_vm {{

{row_type};

{class_boilerplate}

{export}

        }}"
    )
}

fn relation_class_boilerplate(
    name: &str,
    sub_relations: &Vec<String>,
    identities: &Vec<BBIdentity>,
) -> String {
    // TODO: MOVE ELSEWHERE: We add one to all degrees because we have an extra scaling factor
    let degrees = identities.iter().map(|(d, _)| d + 1).collect();
    let degree_boilerplate = get_degree_boilerplate(degrees);
    let relation_code = get_relation_code(sub_relations);
    format!(
        "template <typename FF> class {name}Impl {{
    public:
        
        {degree_boilerplate}
        
        {relation_code}
}};",
    )
}

fn get_export(name: &str) -> String {
    format!(
        "template <typename FF> using {name} = {name}Impl<FF>;",
        name = name
    )
}

fn get_relation_code(ids: &Vec<String>) -> String {
    let mut relation_code = r#"
    template <typename AccumulatorTypes>
    void static accumulate(
        typename AccumulatorTypes::Accumulators& evals,
        const auto& new_term,
        [[maybe_unused]] const RelationParameters<FF>&,
        [[maybe_unused]] const FF& scaling_factor
    ){

    "#
    .to_owned();
    for id in ids {
        relation_code.push_str(&format!("{}\n", id));
    }
    relation_code.push_str("}\n");
    relation_code
}

fn get_degree_boilerplate(degrees: Vec<DegreeType>) -> String {
    let max = degrees.iter().max().unwrap();

    let mut degree_boilerplate = format!("static constexpr size_t RELATION_LENGTH = {};\n", max);
    for i in 0..degrees.len() {
        degree_boilerplate.push_str(&format!(
            "   static constexpr size_t DEGREE_{i} = {};\n",
            degrees[i]
        ));
    }

    let degrees_str = degrees
        .iter()
        .enumerate()
        .map(|(i, _)| format!("DEGREE_{}", i))
        .join(", ");

    let acc_boilerplate = format!(
        "template <template <size_t...> typename SubrelationAccumulatorsTemplate>
    using GetAccumulatorTypes = SubrelationAccumulatorsTemplate<{degrees_str}>;"
    );

    degree_boilerplate.push_str(&acc_boilerplate);

    degree_boilerplate
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
fn create_row_type_items(names: &Vec<String>) -> Vec<String> {
    names
        .iter()
        .map(|name| format!("    FF {};", name.replace(".", "_")))
        .collect::<Vec<_>>()
}

fn get_all_col_names<F: FieldElement>(
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
    let template = format!(
        "
        using View = typename std::tuple_element<{}, typename AccumulatorTypes::AccumulatorViews>::type;
    ",
        row_index
    );
    let col_accesses: Vec<String> = all_rows
        .iter()
        .map(|col_name| {
            let name = col_name.replace(".", "_");
            format!("   auto {} = View(new_term.{});", name, name)
        })
        .collect();

    return template + &col_accesses.join("\n");
}

fn create_identity<F: FieldElement>(expression: &SelectedExpressions<F>) -> Option<BBIdentity> {
    // We want to read the types of operators and then create the appropiate code

    if let Some(expr) = &expression.selector {
        let x = craft_expression(&expr);
        println!("{:?}", x);
        Some(x)
    } else {
        None
    }
}

fn create_subrelation(index: usize, preamble: String, identity: &BBIdentity) -> String {
    // \\\
    let id = &identity.1;
    format!(
        "//Contribution {index}
    {{\n{preamble}
    
    auto tmp = {id};
    tmp *= scaling_factor;
    std::get<{index}>(evals) += tmp;
}}",
    )
}

fn craft_expression<T: FieldElement>(expr: &Expression<T>) -> BBIdentity {
    match expr {
        Expression::Number(n) => (1, format!("FF({})", n.to_arbitrary_integer())),
        Expression::Reference(Reference::Poly(polyref)) => {
            assert_eq!(polyref.index, None);
            let mut poly_name = format!("{}", &polyref.name.replace(".", "_"));
            if polyref.next {
                poly_name = format!("{}_shift", poly_name);
            }
            (1, poly_name)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (ld, lhe) = craft_expression(lhe);
            let (rd, rhe) = craft_expression(rhe);
            let degree = std::cmp::max(ld, rd);
            match op {
                BinaryOperator::Add => (degree, format!("({} + {})", lhe, rhe)),
                BinaryOperator::Sub => (degree, format!("({} - {})", lhe, rhe)),
                BinaryOperator::Mul => (degree + 1, format!("({} * {})", lhe, rhe)),
                _ => unimplemented!("{:?}", expr),
            }
        }
        Expression::Constant(name) => {
            panic!("Constant {name} was not inlined. optimize_constants needs to be run at least.")
        }

        _ => unimplemented!("{:?}", expr),
    }
}

// TODO: MOve -> to gen code we need to know the degree of each poly
type BBIdentity = (DegreeType, String);

/// Todo, eventually these will need to be siloed based on the file name they are in
fn create_identities<F: FieldElement>(
    identities: &Vec<Identity<F>>,
    all_rows: &Vec<String>,
) -> (Vec<String>, Vec<BBIdentity>) {
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

    let mut identities = Vec::new();
    let mut subrelations = Vec::new();
    for (i, expression) in expressions.iter().enumerate() {
        let relation_boilerplate = get_cols_in_identity(i, all_rows);
        // TODO: deal with unwrap
        let identity = create_identity(expression).unwrap();
        let subrelation = create_subrelation(i, relation_boilerplate, &identity);

        identities.push(identity);

        subrelations.push(subrelation);
    }

    // Returning both for now
    (subrelations, identities)
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
