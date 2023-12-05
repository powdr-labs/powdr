use ast::analyzed::Identity;
use ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression as Expression, AlgebraicUnaryOperator,
    IdentityKind,
};
use ast::parsed::SelectedExpressions;
use std::collections::HashSet;

use number::{DegreeType, FieldElement};

use crate::file_writer::BBFiles;
use crate::utils::map_with_newline;

pub trait RelationBuilder {
    fn create_relations(
        &self,
        root_name: &str,
        name: &str,
        sub_relations: &[String],
        identities: &[BBIdentity],
        row_type: &str,
    );

    fn create_declare_views(&self, name: &str, all_cols_and_shifts: &[String]);
}

// TODO: MOve -> to gen code we need to know the degree of each poly
type BBIdentity = (DegreeType, String);

impl RelationBuilder for BBFiles {
    fn create_relations(
        &self,
        root_name: &str,
        name: &str,
        sub_relations: &[String],
        identities: &[BBIdentity],
        row_type: &str,
    ) {
        let includes = relation_includes();
        let class_boilerplate = relation_class_boilerplate(name, sub_relations, identities);
        let export = get_export(name);

        let relations = format!(
            "{includes}
namespace proof_system::{root_name}_vm {{

{row_type};

{class_boilerplate}

{export}

        }}"
        );

        self.write_file(
            &format!("{}/{}", &self.rel, root_name),
            &format!("{}.hpp", name),
            &relations,
        );
    }

    fn create_declare_views(&self, name: &str, all_cols_and_shifts: &[String]) {
        let view_transformation =
            |name: &String| format!("[[maybe_unused]] auto {name} = View(new_term.{name});  \\");
        let make_view_per_row = map_with_newline(all_cols_and_shifts, view_transformation);

        let declare_views = format!(
            "
    #define DECLARE_VIEWS(index) \\
        using Accumulator = typename std::tuple_element<index, ContainerOverSubrelations>::type; \\
        using View = typename Accumulator::View; \\
        {make_view_per_row}


    "
        );

        self.write_file(
            &format!("{}/{name}", &self.rel),
            "declare_views.hpp",
            &declare_views,
        );
    }
}

fn relation_class_boilerplate(
    name: &str,
    sub_relations: &[String],
    identities: &[BBIdentity],
) -> String {
    // We add one to all degrees because we have an extra scaling factor
    let degrees = identities.iter().map(|(d, _)| d + 1).collect();
    let degree_boilerplate = get_degree_boilerplate(degrees);
    let relation_code = get_relation_code(sub_relations);
    format!(
        "template <typename FF_> class {name}Impl {{
    public:
        using FF = FF_;
        
        {degree_boilerplate}
        
        {relation_code}
}};",
    )
}

fn get_export(name: &str) -> String {
    format!(
        "template <typename FF> using {name} = Relation<{name}Impl<FF>>;",
        name = name
    )
}

fn get_relation_code(ids: &[String]) -> String {
    let mut relation_code = r#"
    template <typename ContainerOverSubrelations, typename AllEntities>
    void static accumulate(
        ContainerOverSubrelations& evals,
        const AllEntities& new_term,
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
    let num_degrees = degrees.len();

    let mut degree_boilerplate = format!(
        "static constexpr std::array<size_t, {num_degrees}> SUBRELATION_PARTIAL_LENGTHS{{\n"
    );
    for degree in &degrees {
        degree_boilerplate.push_str(&format!("   {},\n", degree));
    }
    degree_boilerplate.push_str("};");

    degree_boilerplate
}

// The include statements required for a new relation file
fn relation_includes() -> &'static str {
    r#"
#pragma once
#include "../../relation_parameters.hpp"
#include "../../relation_types.hpp"
#include "./declare_views.hpp"
"#
}

// Each vm will need to have a row which is a combination of all of the witness columns
pub(crate) fn create_row_type(name: &str, all_rows: &[String]) -> String {
    let row_transformation = |row: &_| format!("    FF {row} {{}};");
    let all_annotated = map_with_newline(all_rows, row_transformation);

    format!(
        "template <typename FF> struct {name}Row {{ \n{}\n }}",
        all_annotated,
    )
}

fn create_identity<T: FieldElement>(
    expression: &SelectedExpressions<Expression<T>>,
    collected_cols: &mut HashSet<String>,
    collected_public_identities: &mut HashSet<String>,
) -> Option<BBIdentity> {
    // We want to read the types of operators and then create the appropiate code

    if let Some(expr) = &expression.selector {
        let x = craft_expression(expr, collected_cols, collected_public_identities);
        println!("{:?}", x);
        Some(x)
    } else {
        None
    }
}

// TODO: replace the preamble with a macro so the code looks nicer
fn create_subrelation(index: usize, preamble: String, identity: &mut BBIdentity) -> String {
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

fn craft_expression<T: FieldElement>(
    expr: &Expression<T>,
    // TODO: maybe make state?
    collected_cols: &mut HashSet<String>,
    collected_public_identities: &mut HashSet<String>,
) -> BBIdentity {
    match expr {
        Expression::Number(n) => (1, format!("FF({})", n.to_arbitrary_integer())),
        Expression::Reference(polyref) => {
            let mut poly_name = polyref.name.replace('.', "_").to_string();
            if polyref.next {
                // NOTE: Naive algorithm to collect all shifted polys
                poly_name = format!("{}_shift", poly_name);
            }
            collected_cols.insert(poly_name.clone());
            (1, poly_name)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (ld, lhs) = craft_expression(lhe, collected_cols, collected_public_identities);
            let (rd, rhs) = craft_expression(rhe, collected_cols, collected_public_identities);

            let degree = std::cmp::max(ld, rd);
            match op {
                AlgebraicBinaryOperator::Add => (degree, format!("({} + {})", lhs, rhs)),
                AlgebraicBinaryOperator::Sub => match lhe.as_ref() {
                    // BBerg hack, we do not want a field on the lhs of an expression
                    Expression::Number(_) => (degree, format!("(-{} + {})", rhs, lhs)),
                    _ => (degree, format!("({} - {})", lhs, rhs)),
                },

                AlgebraicBinaryOperator::Mul => (degree + 1, format!("({} * {})", lhs, rhs)),
                _ => unimplemented!("{:?}", expr),
            }
        }
        Expression::UnaryOperation(operator, expression) => match operator {
            AlgebraicUnaryOperator::Minus => {
                let (d, e) =
                    craft_expression(expression, collected_cols, collected_public_identities);
                (d, format!("-{}", e))
            }
            _ => unimplemented!("{:?}", expr),
        },
        // TODO: for now we do nothing with calls to public identities
        // These probably can be implemented as some form of copy, however im not sure how we are going to process these down the line
        Expression::PublicReference(name) => {
            // We collect them for now to warn the user what is going on
            collected_public_identities.insert(name.clone());
            (1, "FF(0)".to_string())
        }
    }
}

pub(crate) fn create_identities<F: FieldElement>(
    identities: &[Identity<Expression<F>>],
) -> (Vec<String>, Vec<BBIdentity>, Vec<String>, Vec<String>) {
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
    let mut collected_cols: HashSet<String> = HashSet::new();
    let mut collected_public_identities: HashSet<String> = HashSet::new();

    for (i, expression) in expressions.iter().enumerate() {
        let relation_boilerplate = format!(
            "DECLARE_VIEWS({i});
        ",
        );
        // TODO: deal with unwrap

        // TODO: collected pattern is shit
        let mut identity = create_identity(
            expression,
            &mut collected_cols,
            &mut collected_public_identities,
        )
        .unwrap();
        let subrelation = create_subrelation(i, relation_boilerplate, &mut identity);

        identities.push(identity);

        subrelations.push(subrelation);
    }

    // Print a warning to the user about usage of public identities
    if !collected_public_identities.is_empty() {
        println!("Public Identities are not supported yet in codegen, however some were collected");
        println!("Public Identities: {:?}", collected_public_identities);
    }

    let collected_cols: Vec<String> = collected_cols.drain().collect();
    let collected_shifts: Vec<String> = collected_cols
        .clone()
        .iter()
        .filter_map(|col| {
            if col.ends_with("shift") {
                Some(col.clone())
            } else {
                None
            }
        })
        .collect();

    // Returning both for now
    (subrelations, identities, collected_cols, collected_shifts)
}
