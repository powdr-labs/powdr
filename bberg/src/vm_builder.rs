use std::collections::HashMap;

use ast::analyzed::AlgebraicExpression;
use ast::analyzed::Analyzed;
use ast::analyzed::Identity;

use itertools::Itertools;
use number::FieldElement;

use crate::circuit_builder::CircuitBuilder;
use crate::composer_builder::ComposerBuilder;
use crate::file_writer::BBFiles;
use crate::flavor_builder::FlavorBuilder;
use crate::permutation_builder::Permutation;
use crate::permutation_builder::PermutationBuilder;
use crate::permutation_builder::get_inverses_from_permutations;
use crate::prover_builder::ProverBuilder;
use crate::relation_builder::{create_identities, create_row_type, RelationBuilder};
use crate::utils::capitalize;
use crate::utils::collect_col;
use crate::utils::flatten;
use crate::utils::sanitize_name;
use crate::utils::transform_map;
use crate::verifier_builder::VerifierBuilder;

/// All of the combinations of columns that are used in a bberg flavor file
struct ColumnGroups {
    /// fixed or constant columns in pil -> will be found in vk
    fixed: Vec<String>,
    /// witness or commit columns in pil -> will be found in proof
    witness: Vec<String>,
    /// fixed + witness columns
    all_cols: Vec<String>,
    /// Columns that will not be shifted
    unshifted: Vec<String>,
    /// Columns that will be shifted
    to_be_shifted: Vec<String>,
    /// The shifts of the columns that will be shifted
    shifted: Vec<String>,
    /// fixed + witness + shifted
    all_cols_with_shifts: Vec<String>,
}

/// Analyzed to cpp
/// 
/// Converts an analyzed pil AST into a set of cpp files that can be used to generate a proof
pub(crate) fn analyzed_to_cpp<F: FieldElement>(
    analyzed: &Analyzed<F>,
    fixed: &[(String, Vec<F>)],
    witness: &[(String, Vec<F>)],
    name: Option<String>,
) {
    let file_name: &str = &name.unwrap_or("Example".to_owned());
    let mut bb_files = BBFiles::default(file_name.to_owned());

    // Inlining step to remove the intermediate poly definitions
    let analyzed_identities = analyzed.identities_with_inlined_intermediate_polynomials();

    // ----------------------- Handle Standard Relation Identities -----------------------
    // We collect all references to shifts as we traverse all identities and create relation files
    let RelationOutput {
        relations,
        shifted_polys
     } = create_relation_files(&bb_files, file_name, &analyzed_identities);

    // ----------------------- Handle Lookup / Permutation Relation Identities -----------------------
    let permutations = bb_files.create_permutation_files(file_name, analyzed);
    let inverses = get_inverses_from_permutations(&permutations);

    // TODO: hack - this can be removed with some restructuring
    let shifted_polys: Vec<String> = shifted_polys
        .clone()
        .iter()
        .map(|s| s.replace("_shift", ""))
        .collect();

    // Collect all column names and determine if they need a shift or not
    let ColumnGroups {
        fixed,
        witness,
        all_cols,
        unshifted,
        to_be_shifted,
        shifted,
        all_cols_with_shifts,
    } = get_all_col_names(fixed, witness, &shifted_polys, &permutations);

    bb_files.create_declare_views(file_name, &all_cols_with_shifts);

    // ----------------------- Create the circuit builder file -----------------------
    bb_files.create_circuit_builder_hpp(
        file_name,
        &relations,
        &inverses,
        &all_cols,
        &to_be_shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the flavor file -----------------------
    bb_files.create_flavor_hpp(
        file_name,
        &relations,
        &permutations,
        &fixed,
        &witness,
        &all_cols,
        &to_be_shifted,
        &shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the composer files -----------------------
    bb_files.create_composer_cpp(file_name, &all_cols);
    bb_files.create_composer_hpp(file_name);

    // ----------------------- Create the Verifier files -----------------------
    bb_files.create_verifier_cpp(file_name, &witness);
    bb_files.create_verifier_hpp(file_name);

    // ----------------------- Create the Prover files -----------------------
    bb_files.create_prover_cpp(file_name, &unshifted, &to_be_shifted);
    bb_files.create_prover_hpp(file_name);
}


struct RelationOutput {
    relations: Vec<String>,
    shifted_polys: Vec<String>,
}

/// TODO: MOVE THIS OUT OF THIS FILE????
/// Does this need to return all of the shifted polys that it collects>
/// TODO: restructure this so that we do not have to pass in bb files abd the name at the top level
fn create_relation_files<F: FieldElement>(bb_files: &BBFiles, file_name: &str, analyzed_identities: &Vec<Identity<AlgebraicExpression<F>>>) -> RelationOutput {
    // Group relations per file
    let grouped_relations: HashMap<String, Vec<Identity<AlgebraicExpression<F>>>> = group_relations_per_file(&analyzed_identities);
    let relations = grouped_relations.keys().cloned().collect_vec();

    // Contains all of the rows in each relation, will be useful for creating composite builder types
    // TODO: this will change up
    let mut all_rows: HashMap<String, String> = HashMap::new();
    let mut shifted_polys: Vec<String> = Vec::new();

    // ----------------------- Create the relation files -----------------------
    for (relation_name, analyzed_idents) in grouped_relations.iter() {
        // TODO: make this more granular instead of doing everything at once
        let (subrelations, identities, collected_polys, collected_shifts) =
            create_identities(file_name,analyzed_idents);

        shifted_polys.extend(collected_shifts);

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

    RelationOutput {
        relations,
        shifted_polys
    }

}



/// Group relations per file
///
/// The compiler returns all relations in one large vector, however we want to distinguish
/// which files .pil files the relations belong to for later code gen
///
/// Say we have two files foo.pil and bar.pil
/// foo.pil contains the following relations:
///    - foo1
///    - foo2
/// bar.pil contains the following relations:
///    - bar1
///    - bar2
///
/// This function will return a hashmap with the following structure:
/// {
///  "foo": [foo1, foo2],
///  "bar": [bar1, bar2]
/// }
///
/// This allows us to generate a relation.hpp file containing ONLY the relations for that .pil file
fn group_relations_per_file<F: FieldElement>(
    identities: &[Identity<AlgebraicExpression<F>>],
) -> HashMap<String, Vec<Identity<AlgebraicExpression<F>>>> {
    identities
        .iter()
        .cloned()
        .into_group_map_by(|identity| identity.source.file.clone().replace(".pil", ""))
}

/// Get all col names
///
/// In the flavor file, there are a number of different groups of columns that we need to keep track of
/// This function will return all of the columns in the following groups:
/// - fixed
/// - witness
/// - all_cols
/// - unshifted
/// - to_be_shifted
/// - all_cols_with_shifts
fn get_all_col_names<F: FieldElement>(
    fixed: &[(String, Vec<F>)],
    witness: &[(String, Vec<F>)],
    to_be_shifted: &[String],
    permutations: &[Permutation],
) -> ColumnGroups {
    // Transformations
    let sanitize = |(name, _): &(String, Vec<F>)| sanitize_name(name).to_owned();
    let append_shift = |name: &String| format!("{}_shift", *name);

    let perm_inverses = get_inverses_from_permutations(permutations);

    // Gather sanitized column names
    let fixed_names = collect_col(fixed, sanitize);
    let witness_names = collect_col(witness, sanitize);
    let witness_names = flatten(&[witness_names, perm_inverses]);

    // Group columns by properties
    let shifted = transform_map(to_be_shifted, append_shift);
    let all_cols: Vec<String> = flatten(&[fixed_names.clone(), witness_names.clone()]);
    let unshifted: Vec<String> = flatten(&[fixed_names.clone(), witness_names.clone()])
        .into_iter()
        .filter(|name| !shifted.contains(name))
        .collect();

    let all_cols_with_shifts: Vec<String> =
        flatten(&[fixed_names.clone(), witness_names.clone(), shifted.clone()]);

    // TODO: remove dup
    ColumnGroups {
        fixed: fixed_names,
        witness: witness_names,
        all_cols,
        unshifted,
        to_be_shifted: to_be_shifted.to_vec(),
        shifted,
        all_cols_with_shifts,
    }
}
