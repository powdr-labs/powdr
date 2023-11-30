use std::collections::HashMap;

use ast::analyzed::AlgebraicExpression as Expression;
use ast::analyzed::Analyzed;
use ast::analyzed::Identity;

use itertools::Itertools;
use number::FieldElement;
use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

use crate::circuit_builder::CircuitBuilder;
use crate::composer_builder::ComposerBuilder;
use crate::file_writer::BBFiles;
use crate::flavor_builder::FlavorBuilder;
use crate::prover_builder::ProverBuilder;
use crate::relation_builder::{create_identities, create_row_type, RelationBuilder};
use crate::verifier_builder::VerifierBuilder;

// TODO: move to util
fn sanitize_name(string: &String) -> String {
    string.replace(".", "_").replace("[", "_").replace("]", "_")
}

pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(String, Vec<F>)],
    witness: &[(String, Vec<F>)],
    name: Option<String>,
) {
    let file_name: &str = &name.unwrap_or("Example".to_owned());

    let mut bb_files = BBFiles::default(file_name.to_owned());

    // Inlining step to remove the intermediate poly definitions
    let analyzed_identities = inline_intermediate_polynomials(analyzed);

    let per_file_identites = group_relations_per_file(&analyzed_identities);
    // We require all of the relation file names in order to import them into the flavor
    let relations = per_file_identites.keys().cloned().collect_vec();

    // TODO: duplicated but only used to get the shifts
    let (_, __, collected_cols, collected_shifts) = create_identities(&analyzed_identities);

    // TODO: hack - this can be removed with some restructuring
    let shifted_polys: Vec<String> = collected_shifts
        .clone()
        .iter()
        .map(|s| s.replace("_shift", ""))
        .collect();

    // Collect all column names and determine if they need a shift or not
    let (
        fixed_names,
        witness_names,
        all_cols,
        unshifted,
        to_be_shifted,
        shifted,
        all_cols_with_shifts,
    ) = get_all_col_names(fixed, witness, &shifted_polys);

    // Contains all of the rows in each relation, will be useful for creating composite builder types
    // TODO: this will change up
    let mut all_rows: HashMap<String, String> = HashMap::new();
    // ----------------------- Create the relation files -----------------------
    for (relation_name, analyzed_idents) in per_file_identites.iter() {
        // TODO: make this more granular instead of doing everything at once
        let (subrelations, identities, collected_polys, collected_shifts) =
            create_identities(analyzed_idents);

        // let all_cols_with_shifts = combine_cols(collected_polys, collected_shifts);
        // TODO: This can probably be moved into the create_identities function
        let row_type = create_row_type(&capitalize(relation_name), &collected_polys);

        all_rows.insert(relation_name.clone(), row_type.clone());

        bb_files.create_relations(
            file_name,
            relation_name,
            &subrelations,
            &identities,
            &row_type,
        );
    }

    bb_files.create_declare_views(&file_name, &all_cols_with_shifts);

    // ----------------------- Create the circuit builder file -----------------------
    bb_files.create_circuit_builder_hpp(
        file_name,
        &relations,
        &all_cols,
        &to_be_shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the flavor file -----------------------
    bb_files.create_flavor_hpp(
        file_name,
        &relations,
        &fixed_names,
        &witness_names,
        &all_cols,
        &to_be_shifted,
        &shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the composer files -----------------------
    bb_files.create_composer_cpp(file_name, &all_cols);
    bb_files.create_composer_hpp(file_name);

    // ----------------------- Create the Verifier files -----------------------
    bb_files.create_verifier_cpp(file_name, &witness_names);
    bb_files.create_verifier_hpp(file_name);

    // ----------------------- Create the Prover files -----------------------
    bb_files.create_prover_cpp(file_name, &unshifted, &to_be_shifted);
    bb_files.create_prover_hpp(file_name);
}

fn combine_cols(collected_polys: Vec<String>, collected_shifts: Vec<String>) -> Vec<String> {
    let all_cols_with_shifts = collected_polys
        .iter()
        .map(|name| sanitize_name(name).to_owned())
        .chain(
            collected_shifts
                .iter()
                .map(|name| format!("{}", sanitize_name(name).to_owned()))
                .collect::<Vec<_>>(),
        )
        .collect_vec();
    all_cols_with_shifts
}

fn group_relations_per_file<F: FieldElement>(
    identities: &Vec<Identity<Expression<F>>>,
) -> HashMap<String, Vec<Identity<Expression<F>>>> {
    identities
        .iter()
        .map(|identity| identity.clone())
        .into_group_map_by(|identity| identity.source.file.clone().replace(".pil", ""))
}

fn get_all_col_names<F: FieldElement>(
    fixed: &[(String, Vec<F>)],
    witness: &[(String, Vec<F>)],
    to_be_shifted: &[String],
) -> (
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
    Vec<String>,
) {
    let fixed_names = fixed
        .iter()
        .map(|(name, _)| sanitize_name(name).to_owned())
        .collect::<Vec<_>>();
    let witness_names = witness
        .iter()
        .map(|(name, _)| sanitize_name(name).to_owned())
        .collect::<Vec<_>>();

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

    let with_shifts: Vec<String> = [fixed_names.clone(), witness_names.clone(), shifted.clone()]
        .into_iter()
        .flatten()
        .collect();

    (
        fixed_names,
        witness_names,
        all_cols,
        unshifted,
        to_be_shifted.to_vec(),
        shifted,
        with_shifts,
    )
}

fn capitalize(s: &String) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
