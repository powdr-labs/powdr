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

use crate::prover_builder::{prover_builder_cpp, prover_builder_hpp};
use crate::verifier_builder::{verifier_builder_cpp, verifier_builder_hpp};
use crate::{
    composer_builder::{composer_builder_cpp, composer_builder_hpp},
    flavor_builder,
    trace_builder::TraceBuilder,
};

pub struct BBFiles {
    pub relation_hpp: Option<String>,
    pub arith_hpp: Option<String>,
    pub trace_hpp: Option<String>,
    pub flavor_hpp: Option<String>,
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
            trace_hpp: None,
            flavor_hpp: None,
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
        self.trace_hpp = Some(trace_hpp);
        self.flavor_hpp = Some(flavor_hpp);
        self.composer_cpp = Some(composer_cpp);
        self.composer_hpp = Some(composer_hpp);

        self.verifier_cpp = Some(verifier_cpp);
        self.verifier_hpp = Some(verifier_hpp);

        self.prover_cpp = Some(prover_cpp);
        self.prover_hpp = Some(prover_hpp);
    }

    pub fn write(&self) {
        self.write_file(
            &self.rel,
            &format!("{}.hpp", self.file_name),
            &self.relation_hpp.clone().unwrap(),
        );
        self.write_file(
            &self.arith,
            &format!("{}_arith.hpp", self.file_name),
            &self.arith_hpp.clone().unwrap(),
        );
        self.write_file(
            &self.trace,
            &format!("{}_trace.hpp", self.file_name),
            &self.trace_hpp.clone().unwrap(),
        );
        self.write_file(
            &self.flavor,
            &format!("{}_flavor.hpp", self.file_name),
            &self.flavor_hpp.clone().unwrap(),
        );
        // Composer
        self.write_file(
            &self.composer,
            &format!("{}_composer.cpp", self.file_name),
            &self.composer_cpp.clone().unwrap(),
        );
        self.write_file(
            &self.composer,
            &format!("{}_composer.hpp", self.file_name),
            &self.composer_hpp.clone().unwrap(),
        );

        // Prover
        self.write_file(
            &self.prover,
            &format!("{}_prover.cpp", self.file_name),
            &self.prover_cpp.clone().unwrap(),
        );
        self.write_file(
            &self.prover,
            &format!("{}_prover.hpp", self.file_name),
            &self.prover_hpp.clone().unwrap(),
        );

        // Verifier
        self.write_file(
            &self.prover,
            &format!("{}_verifier.cpp", self.file_name),
            &self.verifier_cpp.clone().unwrap(),
        );
        self.write_file(
            &self.prover,
            &format!("{}_verifier.hpp", self.file_name),
            &self.verifier_hpp.clone().unwrap(),
        );
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
    let file_name: &str = "ExampleRelation";
    let mut bb_files = BBFiles::default(file_name.to_owned());

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

    // ----------------------- Create the relation file -----------------------
    let relation_hpp = create_relation_hpp(file_name, &subrelations, &identities, &row_type); // TODO: do we need this

    // ----------------------- Create the arithmetization file -----------------------
    let arith_hpp = create_arith_boilerplate_file(file_name, num_cols);

    // ----------------------- Create the read from powdr columns file -----------------------
    let trace_hpp = bb_files.create_trace_builder(file_name, fixed, witness);

    // ----------------------- Create the flavor file -----------------------
    let flavor_hpp =
        flavor_builder::create_flavor_hpp(file_name, &all_cols, &fixed_names, &witness_names);

    // ----------------------- Create the composer files -----------------------
    let composer_cpp = composer_builder_cpp(file_name);
    let composer_hpp = composer_builder_hpp(file_name);

    // ----------------------- Create the prover files -----------------------
    let verifier_cpp = verifier_builder_cpp(file_name, &all_cols);
    let verifier_hpp = verifier_builder_hpp(file_name);

    // ----------------------- Create the verifier files -----------------------
    let prover_cpp = prover_builder_cpp(file_name, &fixed_names, &witness_names);
    let prover_hpp = prover_builder_hpp(file_name);

    // These are all of the exotic ish data structures we will need
    // let mut lookups = vec![];
    // let mut perms = vec![];
    // let mut polys = vec![];

    // // From all of the fixed and witness columns we have, we want to create the row object for bb

    // // Note: we do not have lookups yet
    // assert!(lookups.len() == 0, "lookups not implemented");

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
            format!(
                "   [[maybe_unused]] auto {} = View(new_term.{});",
                name, name
            )
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
