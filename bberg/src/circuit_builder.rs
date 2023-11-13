use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

use ast::parsed::SelectedExpressions;
// use acvm::acir::native_types::Expression;
use ast::analyzed::Identity;
use itertools::Itertools;

use ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression as Expression, AlgebraicUnaryOperator, Analyzed,
    IdentityKind,
};

use number::{DegreeType, FieldElement};

// use super::circuit_data::CircuitData;

use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

use crate::prover_builder::{prover_builder_cpp, prover_builder_hpp};
use crate::verifier_builder::{verifier_builder_cpp, verifier_builder_hpp};
use crate::FILE_NAME;
use crate::{
    composer_builder::{composer_builder_cpp, composer_builder_hpp},
    flavor_builder,
    trace_builder::TraceBuilder,
};

pub struct BBFiles {
    pub relation_hpp: Option<String>,
    pub arith_hpp: Option<String>,
    pub flavor_hpp: Option<String>,
    // trace
    pub trace_hpp: Option<String>,
    // composer
    pub composer_cpp: Option<String>,
    pub composer_hpp: Option<String>,

    // prover
    pub prover_cpp: Option<String>,
    pub prover_hpp: Option<String>,

    // verifier
    pub verifier_cpp: Option<String>,
    pub verifier_hpp: Option<String>,

    // Relative paths
    pub file_name: String,
    pub base: String,
    pub rel: String,
    pub arith: String,
    pub trace: String,
    pub flavor: String,
    pub composer: String,
    pub prover: String, // path for both prover and verifier files
}

impl BBFiles {
    pub fn default(file_name: String) -> Self {
        Self::new(file_name, None, None, None, None, None, None, None)
    }

    pub fn new(
        file_name: String,
        base: Option<String>,
        rel: Option<String>,
        arith: Option<String>,
        trace: Option<String>,
        flavor: Option<String>,
        composer: Option<String>,
        prover: Option<String>,
    ) -> Self {
        let base = base.unwrap_or("src/barretenberg".to_owned());
        let rel = rel.unwrap_or("proof_system/relations/generated".to_owned());
        let arith = arith.unwrap_or("proof_system/arithmetization/generated".to_owned());
        let trace = trace.unwrap_or("proof_system/circuit_builder/generated".to_owned());
        let flavor = flavor.unwrap_or("honk/flavor/generated".to_owned());
        let composer = composer.unwrap_or("honk/composer/generated".to_owned());
        let prover = prover.unwrap_or("honk/proof_system/generated".to_owned());

        Self {
            file_name,
            relation_hpp: None,
            arith_hpp: None,
            flavor_hpp: None,
            trace_hpp: None,
            composer_cpp: None,
            composer_hpp: None,
            prover_cpp: None,
            prover_hpp: None,
            verifier_cpp: None,
            verifier_hpp: None,

            base,
            rel,
            arith,
            trace,
            flavor,
            composer,
            prover,
        }
    }

    pub fn add_files(
        &mut self,
        relation_hpp: String,
        arith_hpp: String,
        trace_hpp: String,
        flavor_hpp: String,
        composer_cpp: String,
        composer_hpp: String,
        verifier_cpp: String,
        verifier_hpp: String,
        prover_cpp: String,
        prover_hpp: String,
    ) {
        self.relation_hpp = Some(relation_hpp);
        self.arith_hpp = Some(arith_hpp);
        self.flavor_hpp = Some(flavor_hpp);
        self.composer_cpp = Some(composer_cpp);
        self.composer_hpp = Some(composer_hpp);

        self.trace_hpp = Some(trace_hpp);

        self.verifier_cpp = Some(verifier_cpp);
        self.verifier_hpp = Some(verifier_hpp);

        self.prover_cpp = Some(prover_cpp);
        self.prover_hpp = Some(prover_hpp);
    }

    pub fn write(&self) {
        // Helper macro codegen using the classes' write_file method
        macro_rules! write_file {
            ($location:expr, $extension:expr, $content:expr) => {
                self.write_file(
                    &$location,
                    &format!("{}{}", self.file_name, $extension),
                    &$content.clone().unwrap(),
                );
            };
        }
        write_file!(self.rel, ".hpp", self.relation_hpp);
        write_file!(self.arith, "_arith.hpp", self.arith_hpp);

        // Trace
        write_file!(self.trace, "_trace.hpp", self.trace_hpp);

        write_file!(self.flavor, "_flavor.hpp", self.flavor_hpp);

        // Composer
        write_file!(self.composer, "_composer.hpp", self.composer_hpp);
        write_file!(self.composer, "_composer.cpp", self.composer_cpp);

        // Prover
        write_file!(self.prover, "_prover.hpp", self.prover_hpp);
        write_file!(self.prover, "_prover.cpp", self.prover_cpp);

        // Verifier
        write_file!(self.prover, "_verifier.hpp", self.verifier_hpp);
        write_file!(self.prover, "_verifier.cpp", self.verifier_cpp);
    }

    fn write_file(&self, folder: &str, filename: &str, contents: &String) {
        // attempt to create dir
        let base_path = format!("{}/{}", self.base, folder);
        let _ = std::fs::create_dir_all(&base_path);

        let joined = format!("{}/{}", base_path, filename);
        println!("Writing file: {}", joined);
        let mut file = File::create(joined).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }
}

pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
) -> BBFiles {
    let file_name: &str = FILE_NAME;

    let mut bb_files = BBFiles::default(file_name.to_owned());

    // Collect all column names and determine if they need a shift or not

    // TODO: currently we provide shifts for both the fixed and witness columns, in the long term we need to work out what needs a shift and what doesn't
    let fixed_names = fixed
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();
    let witness_names = witness
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();

    println!("Fixed: {:?}", fixed_names);
    println!("Witness: {:?}", witness_names);
    let first_col = fixed
        .iter()
        .find(|col_name| col_name.0.contains("FIRST"))
        .expect("PIL file must contain a fixed column named FIRST")
        .0
        .replace(".", "_");

    let last_col = fixed
        .iter()
        .find(|col_name| col_name.0.contains("LAST"))
        .expect("PIL file must contain a fixed column named LAST")
        .0
        .replace(".", "_");

    // Inlining step to remove the intermediate poly definitions
    let analyzed_identities = inline_intermediate_polynomials(analyzed);

    let (subrelations, identities, mut collected_shifts) =
        create_identities(&first_col, &last_col, &analyzed_identities);
    let shifted_polys: Vec<String> = collected_shifts.drain().collect_vec();
    dbg!(shifted_polys.clone());

    let (all_cols, unshifted, to_be_shifted, _shifted, all_cols_with_shifts) =
        get_all_col_names(fixed, witness, &shifted_polys);
    let num_cols = all_cols_with_shifts.len();

    let row_type = create_row_type(&all_cols_with_shifts);

    // ----------------------- Create the relation file -----------------------
    let relation_hpp = create_relation_hpp(
        file_name,
        &subrelations,
        &identities,
        &row_type,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the arithmetization file -----------------------
    let arith_hpp = create_arith_boilerplate_file(file_name, num_cols);

    // ----------------------- Create the read from powdr columns file -----------------------
    let trace_hpp = bb_files.create_trace_builder_hpp(file_name, &all_cols, &to_be_shifted);

    // ----------------------- Create the flavor file -----------------------
    let flavor_hpp = flavor_builder::create_flavor_hpp(
        file_name,
        &subrelations,
        &all_cols,
        &to_be_shifted,
        // &shifted,
    );

    // ----------------------- Create the composer files -----------------------
    let composer_cpp = composer_builder_cpp(file_name);
    let composer_hpp = composer_builder_hpp(file_name);

    // ----------------------- Create the prover files -----------------------
    let verifier_cpp = verifier_builder_cpp(file_name, &all_cols);
    let verifier_hpp = verifier_builder_hpp(file_name);

    // ----------------------- Create the verifier files -----------------------
    let prover_cpp = prover_builder_cpp(file_name, &unshifted, &to_be_shifted);
    let prover_hpp = prover_builder_hpp(file_name);

    bb_files.add_files(
        relation_hpp,
        arith_hpp,
        trace_hpp,
        flavor_hpp,
        composer_cpp,
        composer_hpp,
        verifier_cpp,
        verifier_hpp,
        prover_cpp,
        prover_hpp,
    );
    bb_files
}

// We have no selectors so we can easily create a boilerplate file
fn create_arith_boilerplate_file(name: &str, num_cols: usize) -> String {
    format!(
        "
#pragma once
#include \"barretenberg/proof_system/arithmetization/arithmetization.hpp\"
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
    sub_relations: &[String],
    identities: &[BBIdentity],
    row_type: &String,
    all_rows_and_shifts: &[String],
) -> String {
    let includes = relation_includes();
    let class_boilerplate = relation_class_boilerplate(name, sub_relations, identities);
    let export = get_export(name);

    let view_macro_preamble = get_cols_in_identity_macro(all_rows_and_shifts);

    format!(
        "{includes}
namespace proof_system::{name}_vm {{

{row_type};

{view_macro_preamble}

{class_boilerplate}

{export}

        }}"
    )
}

fn relation_class_boilerplate(
    name: &str,
    sub_relations: &[String],
    identities: &[BBIdentity],
) -> String {
    // TODO: MOVE ELSEWHERE: We add one to all degrees because we have an extra scaling factor
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
    // TODO: for the meantime we will use the max degree for all, i am facing a compile time issue with cpp
    // that is preventing me from using the real degree
    let max = degrees.iter().max().unwrap();
    let num_degrees = degrees.len();

    let mut degree_boilerplate =
        format!("static constexpr std::array<size_t, {num_degrees}> SUBRELATION_LENGTHS{{\n");
    // for i in 0..degrees.len() {
    //     degree_boilerplate.push_str(&format!("   {},\n", degrees[i]));
    // }
    for _ in 0..degrees.len() {
        degree_boilerplate.push_str(&format!("   {},\n", max));
    }
    degree_boilerplate.push_str("};");

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
fn create_row_type_items(names: &[String]) -> Vec<String> {
    names
        .iter()
        .map(|name| format!("    FF {} {{}};", name.replace('.', "_")))
        .collect::<Vec<_>>()
}

fn get_all_col_names<F: FieldElement>(
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
    to_be_shifted: &[String],
) -> (
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
) {
    let fixed_names: Vec<String> = fixed
        .iter()
        .map(|(name, _)| {
            let n = name.replace('.', "_");
            n.to_owned()
        })
        .collect();
    let witness_names: Vec<String> = witness
        .iter()
        .map(|(name, _)| {
            let n = name.replace('.', "_");
            n.to_owned()
        })
        .collect();

    let shifted: Vec<String> = to_be_shifted
        .iter()
        .map(|name| format!("{}_shift", *name))
        .collect();

    let all_cols: Vec<String> = [fixed_names.clone(), witness_names.clone()]
        .into_iter()
        .flatten()
        .collect();

    dbg!(all_cols.clone());

    let unshifted: Vec<String> = [fixed_names.clone(), witness_names.clone()]
        .into_iter()
        .flatten()
        .filter(|name| !shifted.contains(name))
        .collect();

    let with_shifts: Vec<String> = [fixed_names, witness_names, shifted.clone()]
        .into_iter()
        .flatten()
        .collect();

    (
        all_cols,
        unshifted,
        to_be_shifted.to_vec(),
        shifted,
        with_shifts,
    )
}

// Each vm will need to have a row which is a combination of all of the witness columns
fn create_row_type(all_rows: &[String]) -> String {
    let all_annotated = create_row_type_items(all_rows);

    let row_type = format!(
        "template <typename FF> struct Row {{ \n{}\n }}",
        all_annotated.join("\n"),
    );

    println!("{}", row_type);
    row_type
}

fn get_cols_in_identity_macro(all_rows_and_shifts: &[String]) -> String {
    let make_view_per_row = all_rows_and_shifts
        .iter()
        .map(|row_name| {
            let name = row_name.replace('.', "_");
            format!("[[maybe_unused]] auto {name} = View(new_term.{name});  \\")
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "
    #define DECLARE_VIEWS(index) \
        using View = typename std::tuple_element<index, ContainerOverSubrelations>::type; \
        {make_view_per_row}

    


    "
    )
}

fn create_identity<T: FieldElement>(
    last_col: &str,
    expression: &SelectedExpressions<Expression<T>>,
    collected_shifts: &mut HashSet<String>,
) -> Option<BBIdentity> {
    // We want to read the types of operators and then create the appropiate code

    if let Some(expr) = &expression.selector {
        let x = craft_expression(last_col, expr, collected_shifts);
        println!("{:?}", x);
        Some(x)
    } else {
        None
    }
}

// TODO: replace the preamble with a macro so the code looks nicer
fn create_subrelation(
    first_col: &str,
    index: usize,
    preamble: String,
    identity: &mut BBIdentity,
) -> String {
    // \\\
    let id = &identity.1;

    // TODO: TEMP HACK: Part of the main_FIRST hack below - to switch off constraints on the first row
    identity.0 += 1;
    format!(
        "//Contribution {index}
    {{\n{preamble}
    
    auto tmp = {id};
    tmp *= scaling_factor;
    tmp *= (-{first_col} + FF(1)); // Temp to switch off 
    std::get<{index}>(evals) += tmp;
}}",
    )
}

fn craft_expression<T: FieldElement>(
    last_col: &str,
    expr: &Expression<T>,
    collected_shifts: &mut HashSet<String>,
) -> BBIdentity {
    match expr {
        Expression::Number(n) => (1, format!("FF({})", n.to_arbitrary_integer())),
        Expression::Reference(polyref) => {
            assert_eq!(polyref.index, None);
            let mut poly_name = polyref.name.replace('.', "_").to_string();
            let mut degree = 1;
            if polyref.next {
                // NOTE: Naive algorithm to collect all shifted polys
                collected_shifts.insert(poly_name.clone());

                poly_name = format!("{}_shift", poly_name);

                // TODO(HORRIBLE): TEMP, add in a relation that turns off shifts on the last row
                poly_name = format!("{poly_name} * (-{} + FF(1))", last_col);
                degree += 1;
            }
            (degree, poly_name)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (ld, lhs) = craft_expression(last_col, lhe, collected_shifts);
            let (rd, rhs) = craft_expression(last_col, rhe, collected_shifts);

            // dbg!(&lhe);
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
        // Expression::Constant(name) => {
        //     panic!("Constant {name} was not inlined. optimize_constants needs to be run at least.")
        // }
        // pub enum UnaryOperator {
        //     Plus,
        //     Minus,
        //     LogicalNot,
        // }
        Expression::UnaryOperation(operator, expression) => match operator {
            AlgebraicUnaryOperator::Minus => {
                let (d, e) = craft_expression(last_col, expression, collected_shifts);
                (d, format!("-{}", e))
            }
            _ => unimplemented!("{:?}", expr),
        },

        _ => unimplemented!("{:?}", expr),
    }
}

// TODO: MOve -> to gen code we need to know the degree of each poly
type BBIdentity = (DegreeType, String);

/// Todo, eventually these will need to be siloed based on the file name they are in
fn create_identities<F: FieldElement>(
    first_col: &str,
    last_col: &str,
    identities: &Vec<Identity<Expression<F>>>,
) -> (Vec<String>, Vec<BBIdentity>, HashSet<String>) {
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
    let mut collected_shifts: HashSet<String> = HashSet::new();

    for (i, expression) in expressions.iter().enumerate() {
        let relation_boilerplate = format!(
            "DECLARE_VIEWS({i});
        ",
        );
        // TODO: deal with unwrap

        let mut identity = create_identity(last_col, expression, &mut collected_shifts).unwrap();
        let subrelation = create_subrelation(first_col, i, relation_boilerplate, &mut identity);

        identities.push(identity);

        subrelations.push(subrelation);
    }

    // Returning both for now
    (subrelations, identities, collected_shifts)
}

//
//    Row check_row = { .main_FIRST = 1, .main__block_enforcer_last_step = 1, .main_XIsZero = 1 };
// rows.push_back(check_row);
//
//
