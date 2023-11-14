use ast::analyzed::Analyzed;

use itertools::Itertools;
use number::FieldElement;
use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

use crate::file_writer::BBFiles;
use crate::prover_builder::{prover_builder_cpp, prover_builder_hpp};
use crate::relation_builder::{create_identities, create_relation_hpp, create_row_type};
use crate::trace_builder::TraceBuilder;
use crate::verifier_builder::{verifier_builder_cpp, verifier_builder_hpp};
use crate::{
    composer_builder::{composer_builder_cpp, composer_builder_hpp},
    flavor_builder,
};

pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(&str, Vec<F>)],
    witness: &[(&str, Vec<F>)],
    bname: Option<String>,
) -> BBFiles {
    let file_name: &str = &bname.unwrap_or("Example".to_owned());

    let mut bb_files = BBFiles::default(file_name.to_owned());

    // Collect all column names and determine if they need a shift or not

    // TODO: currently we provide shifts for both the fixed and witness columns, in the long term we need to work out what needs a shift and what doesn't
    let _fixed_names = fixed
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();
    let _witness_names = witness
        .iter()
        .map(|(name, _)| (*name).to_owned())
        .collect::<Vec<_>>();

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
