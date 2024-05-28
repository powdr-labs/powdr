use ast::analyzed::Analyzed;

use number::FieldElement;

use crate::circuit_builder::CircuitBuilder;
use crate::composer_builder::ComposerBuilder;
use crate::file_writer::BBFiles;
use crate::flavor_builder::FlavorBuilder;
use crate::lookup_builder::get_counts_from_lookups;
use crate::lookup_builder::get_inverses_from_lookups;
use crate::lookup_builder::Lookup;
use crate::lookup_builder::LookupBuilder;
use crate::permutation_builder::get_inverses_from_permutations;
use crate::permutation_builder::Permutation;
use crate::permutation_builder::PermutationBuilder;
use crate::prover_builder::ProverBuilder;
use crate::relation_builder::RelationBuilder;
use crate::relation_builder::RelationOutput;
use crate::utils::collect_col;
use crate::utils::flatten;
use crate::utils::sanitize_name;
use crate::utils::sort_cols;
use crate::utils::transform_map;
use crate::verifier_builder::VerifierBuilder;

/// All of the combinations of columns that are used in a bberg flavor file
struct ColumnGroups {
    /// fixed or constant columns in pil -> will be found in vk
    fixed: Vec<String>,
    /// witness or commit columns in pil -> will be found in proof
    witness: Vec<String>,
    // public input columns, evaluations will be calculated within the verifier
    public: Vec<String>,
    /// witness or commit columns in pil, with out the inverse columns
    witnesses_without_inverses: Vec<String>,
    /// fixed + witness columns without lookup inverses
    all_cols_without_inverses: Vec<String>,
    /// fixed + witness columns with lookup inverses
    all_cols: Vec<String>,
    /// Columns that will not be shifted
    unshifted: Vec<String>,
    /// Columns that will be shifted
    to_be_shifted: Vec<String>,
    /// The shifts of the columns that will be shifted
    shifted: Vec<String>,
    /// fixed + witness + shifted
    all_cols_with_shifts: Vec<String>,
    /// Inverses from lookups and permuations
    inverses: Vec<String>,
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
    // Sort fixed and witness to ensure consistent ordering
    let fixed = &sort_cols(fixed);
    let witness = &sort_cols(witness);

    let file_name: &str = &name.unwrap_or("Example".to_owned());
    let mut bb_files = BBFiles::default(file_name.to_owned());

    // Inlining step to remove the intermediate poly definitions
    let mut analyzed_identities = analyzed.identities_with_inlined_intermediate_polynomials();
    analyzed_identities.sort_by(|a, b| a.id.cmp(&b.id));

    // ----------------------- Handle Standard Relation Identities -----------------------
    // We collect all references to shifts as we traverse all identities and create relation files
    let RelationOutput {
        relations,
        shifted_polys,
    } = bb_files.create_relations(file_name, &analyzed_identities);

    // ----------------------- Handle Lookup / Permutation Relation Identities -----------------------
    let permutations = bb_files.create_permutation_files(file_name, analyzed);
    let lookups = bb_files.create_lookup_files(file_name, analyzed);

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
        public,
        witnesses_without_inverses,
        all_cols,
        all_cols_without_inverses,
        unshifted: _unshifted,
        to_be_shifted,
        shifted,
        all_cols_with_shifts,
        inverses,
    } = get_all_col_names(fixed, witness, &shifted_polys, &permutations, &lookups);

    bb_files.create_declare_views(file_name, &all_cols_with_shifts);

    // ----------------------- Create the circuit builder file -----------------------
    bb_files.create_circuit_builder_hpp(
        file_name,
        &relations,
        &inverses,
        &all_cols_without_inverses,
        &all_cols,
        &to_be_shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the flavor file -----------------------
    bb_files.create_flavor_hpp(
        file_name,
        &relations,
        &inverses,
        &fixed,
        &witness,
        &all_cols,
        &to_be_shifted,
        &shifted,
        &all_cols_with_shifts,
    );

    // ----------------------- Create the composer files -----------------------
    bb_files.create_composer_cpp(file_name);
    bb_files.create_composer_hpp(file_name);

    // ----------------------- Create the Verifier files -----------------------
    bb_files.create_verifier_cpp(file_name, &witnesses_without_inverses, &inverses, &public);
    bb_files.create_verifier_hpp(file_name, &public);

    // ----------------------- Create the Prover files -----------------------
    bb_files.create_prover_cpp(file_name, &witnesses_without_inverses, &inverses);
    bb_files.create_prover_hpp(file_name);
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
    lookups: &[Lookup],
) -> ColumnGroups {
    // Transformations
    let sanitize = |(name, _): &(String, Vec<F>)| sanitize_name(name).to_owned();
    let append_shift = |name: &String| format!("{}_shift", *name);

    let perm_inverses = get_inverses_from_permutations(permutations);
    let lookup_inverses = get_inverses_from_lookups(lookups);
    let lookup_counts = get_counts_from_lookups(lookups);

    // Gather sanitized column names
    let fixed_names = collect_col(fixed, sanitize);
    let witness_names = collect_col(witness, sanitize);
    let (witness_names, public_input_column_names) = extract_public_input_columns(witness_names);

    let inverses = flatten(&[perm_inverses, lookup_inverses]);
    let witnesses_without_inverses = flatten(&[witness_names.clone(), lookup_counts.clone()]);
    let witnesses_with_inverses = flatten(&[witness_names, inverses.clone(), lookup_counts]);

    // Group columns by properties
    let shifted = transform_map(to_be_shifted, append_shift);
    let all_cols_without_inverses: Vec<String> =
        flatten(&[fixed_names.clone(), witnesses_without_inverses.clone()]);
    let all_cols: Vec<String> = flatten(&[fixed_names.clone(), witnesses_with_inverses.clone()]);
    let unshifted: Vec<String> = flatten(&[fixed_names.clone(), witnesses_with_inverses.clone()])
        .into_iter()
        .filter(|name| !shifted.contains(name))
        .collect();

    let all_cols_with_shifts: Vec<String> = flatten(&[
        fixed_names.clone(),
        witnesses_with_inverses.clone(),
        shifted.clone(),
    ]);

    ColumnGroups {
        fixed: fixed_names,
        witness: witnesses_with_inverses,
        public: public_input_column_names,
        all_cols_without_inverses,
        witnesses_without_inverses,
        all_cols,
        unshifted,
        to_be_shifted: to_be_shifted.to_vec(),
        shifted,
        all_cols_with_shifts,
        inverses,
    }
}

/// Extract public input columns
/// The compiler automatically suffixes the public input columns with "__is_public"
/// This function removes the suffix and collects the columns into their own container
pub fn extract_public_input_columns(witness_columns: Vec<String>) -> (Vec<String>, Vec<String>) {
    let witness_names: Vec<String> = witness_columns.clone();
    let public_input_column_names: Vec<String> = witness_columns
        .into_iter()
        .filter(|name| name.ends_with("__is_public"))
        .collect();

    (witness_names, public_input_column_names)
}
