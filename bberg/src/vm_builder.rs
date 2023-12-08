use std::collections::HashMap;

use ast::analyzed::AlgebraicExpression;
use ast::analyzed::Analyzed;
use ast::analyzed::Identity;

use ast::analyzed::IdentityKind;
use ast::parsed::SelectedExpressions;
use itertools::Itertools;
use number::FieldElement;

use crate::circuit_builder::CircuitBuilder;
use crate::composer_builder::ComposerBuilder;
use crate::file_writer::BBFiles;
use crate::flavor_builder::FlavorBuilder;
use crate::prover_builder::ProverBuilder;
use crate::relation_builder;
use crate::relation_builder::{create_identities, create_row_type, RelationBuilder};
use crate::utils::capitalize;
use crate::utils::collect_col;
use crate::utils::flatten;
use crate::utils::sanitize_name;
use crate::utils::transform_map;
use crate::verifier_builder::VerifierBuilder;

struct ColumnGroups {
    fixed: Vec<String>,
    witness: Vec<String>,
    all_cols: Vec<String>,
    unshifted: Vec<String>,
    to_be_shifted: Vec<String>,
    shifted: Vec<String>,
    all_cols_with_shifts: Vec<String>,
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
    let analyzed_identities = analyzed.identities_with_inlined_intermediate_polynomials();

    // ----------------------- Handle Standard Relation Identities -----------------------
    // We collect all references to shifts as we traverse all identities and create relation files
    let RelationOutput {
        relations,
        shifted_polys
     } = create_relation_files(&bb_files, file_name, &analyzed_identities);

    // ----------------------- Handle Lookup / Permutation Relation Identities -----------------------
    let permutations = handle_permutations(&bb_files, file_name, analyzed);
    let inverses = get_inverses_from_permutations(&permutations);
    // TODO: may need to merge the perm_inverses for the flavor

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

    dbg!(&fixed);
    dbg!(&witness);

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
            create_identities(analyzed_idents);

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


// TODO(md): move permutation related code into its own home
#[derive(Debug)]
pub struct Permutation {
    /// Attribute in this setting is used to determine what the name of the inverse column should be
    /// TODO(md): Future implementations should use this to allow using the same inverse column multiple times, thus 
    /// allowing granularity in the logup trade-off
    pub attribute: Option<String>,
    pub left: PermSide,
    pub right: PermSide
}

/// The attributes of a permutation contain the name of the inverse, we collect all of these to create the inverse column
pub fn get_inverses_from_permutations(permutations: &[Permutation]) -> Vec<String> {
    permutations.iter().map(|perm| perm.attribute.clone().unwrap()).collect()
}

// TODO: rename
#[derive(Debug)]
pub struct PermSide {
    selector: Option<String>,
    cols: Vec<String>
}

fn handle_permutations<F: FieldElement>(bb_files: &BBFiles, project_name: &str, analyzed: &Analyzed<F>) -> Vec<Permutation> {
    let perms: Vec<&Identity<AlgebraicExpression<F>>> = analyzed.identities.iter().filter(|identity| matches!(identity.kind, IdentityKind::Permutation)).collect();
    let new_perms = perms.iter().map(|perm| 
        Permutation {
            attribute: perm.attribute.clone(),
            left: get_perm_side(&perm.left),
            right: get_perm_side(&perm.right)
        }).collect_vec();
    dbg!(&new_perms);

    // For every permutation set that we create, we will need to create an inverse column ( helper function )
    // TODO: how do we determine the name of this inverse column?

    create_permutations(bb_files, &project_name, &new_perms);
    new_perms
}

fn create_permutations(bb_files: &BBFiles, project_name: &str, permutations: &Vec<Permutation>) {
    for permutation in permutations {
        let perm_settings = create_permutation_settings_file(permutation);
        
        // TODO: temp this is not going to be the final configuration, maybe we use the trait construction again to have access to bb
        let folder = format!("{}/{}", bb_files.rel, project_name);
        let file_name = format!("{}{}", permutation.attribute.clone().unwrap_or("NONAME".to_owned()), ".hpp".to_owned());
        bb_files.write_file(&folder, &file_name, &perm_settings);
    }
}

/// All relation types eventually get wrapped in the relation type 
/// This function creates the export for the relation type so that it can be added to the flavor
fn create_relation_exporter(permutation_name: &str) -> String {
    let name = format!("{}_permutation_settings", permutation_name);

    let class_export = format!("template class GenericPermutationRelationImpl<{name}, barretenberg::fr>;");
    let template_export = format!("template <typename FF_> using Generic{permutation_name} = GenericPermutationRelationImpl<{name}, FF_>;",);
    let relation_export = format!("template <typename FF> using {permutation_name} = Relation<Generic{permutation_name}<FF>>;");

    format!("
    {class_export}

    {template_export}

    {relation_export} 
    ")
}

fn permutation_settings_includes() -> &'static str {
    r#"
    #pragma once

    #include "barretenberg/relations/generic_permutation/generic_permutation_relation.hpp"

    #include <cstddef>
    #include <tuple> 
    "#
}

fn create_permutation_settings_file(permutation: &Permutation) -> String {

    println!("Permutation: {:?}", permutation);
    let columns_per_set = permutation.left.cols.len();
    // TODO(md): Throw an error if no attribute is provided for the permutation
    // TODO(md): In the future we will need to condense off the back of this - combining those with the same inverse column
    let permutation_name = permutation.attribute.clone().expect("Inverse column name must be provided"); // TODO(md): catch this earlier than here
    
    // NOTE: syntax is not flexible enough to enable the single row case right now :(:(:(:(:))))
    // This also will need to work for both sides of this !
    let selector = permutation.left.selector.clone().unwrap(); // TODO: deal with unwrap
    let lhs_cols = permutation.left.cols.clone();
    let rhs_cols = permutation.right.cols.clone();

    // 0.                       The polynomial containing the inverse products -> taken from the attributes
    // 1.                       The polynomial enabling the relation (the selector)
    // 2.                       lhs selector
    // 3.                       rhs selector
    // 4.. + columns per set.   lhs cols
    // 4 + columns per set.. .  rhs cols
    let mut perm_entities: Vec<String> = [
        permutation_name.clone(),
        selector.clone(),
        selector.clone(),
        selector.clone() // TODO: update this away from the simple example
    ].to_vec();

    perm_entities.extend(lhs_cols);
    perm_entities.extend(rhs_cols);


    // TODO: below here should really be in a new function as we have just got the settings extracted from the parsed type
    let permutation_settings_includes = permutation_settings_includes();
    let inverse_computed_at = create_inverse_computed_at(selector);
    let const_entities = create_get_const_entities(&perm_entities);
    let nonconst_entities = create_get_nonconst_entities(&perm_entities);
    let relation_exporter = create_relation_exporter(&permutation_name);

    format!(
        // TODO: replace with the inverse label name!
        "
        {permutation_settings_includes}

        namespace proof_system::honk::sumcheck {{

        class {permutation_name}_permutation_settings {{
            public:
                  // This constant defines how many columns are bundled together to form each set.
                  constexpr static size_t COLUMNS_PER_SET = {columns_per_set};
              
                  /**
                   * @brief If this method returns true on a row of values, then the inverse polynomial at this index. Otherwise the
                   * value needs to be set to zero.
                   *
                   * @details If this is true then permutation takes place in this row
                   */
                  {inverse_computed_at}
              
                  /**
                   * @brief Get all the entities for the permutation when we don't need to update them
                   *
                   * @details The entities are returned as a tuple of references in the following order:
                   * - The entity/polynomial used to store the product of the inverse values
                   * - The entity/polynomial that switches on the subrelation of the permutation relation that ensures correctness of
                   * the inverse polynomial
                   * - The entity/polynomial that enables adding a tuple-generated value from the first set to the logderivative sum
                   * subrelation
                   * - The entity/polynomial that enables adding a tuple-generated value from the second set to the logderivative sum
                   * subrelation
                   * - A sequence of COLUMNS_PER_SET entities/polynomials that represent the first set (N.B. ORDER IS IMPORTANT!)
                   * - A sequence of COLUMNS_PER_SET entities/polynomials that represent the second set (N.B. ORDER IS IMPORTANT!)
                   *
                   * @return All the entities needed for the permutation
                   */
                  {const_entities}
              
                  /**
                   * @brief Get all the entities for the permutation when need to update them
                   *
                   * @details The entities are returned as a tuple of references in the following order:
                   * - The entity/polynomial used to store the product of the inverse values
                   * - The entity/polynomial that switches on the subrelation of the permutation relation that ensures correctness of
                   * the inverse polynomial
                   * - The entity/polynomial that enables adding a tuple-generated value from the first set to the logderivative sum
                   * subrelation
                   * - The entity/polynomial that enables adding a tuple-generated value from the second set to the logderivative sum
                   * subrelation
                   * - A sequence of COLUMNS_PER_SET entities/polynomials that represent the first set (N.B. ORDER IS IMPORTANT!)
                   * - A sequence of COLUMNS_PER_SET entities/polynomials that represent the second set (N.B. ORDER IS IMPORTANT!)
                   *
                   * @return All the entities needed for the permutation
                   */
                  {nonconst_entities}
        }};

        {relation_exporter}
    }}
        "
    )
    
}



// TODO: make this dynamic such that there can be more than one
fn create_inverse_computed_at(inverse_selector: String) -> String {
    let inverse_computed_selector = format!("in.{inverse_selector}");
    format!("
    template <typename AllEntities> static inline auto inverse_polynomial_is_computed_at_row(const AllEntities& in) {{
        return ({inverse_computed_selector} == 1);
    }}")
}

fn create_get_const_entities (settings: &[String]) -> String {
    let forward = create_forward_as_tuple(settings);
    format!("
    template <typename AllEntities> static inline auto get_const_entities(const AllEntities& in) {{
        {forward}
    }}
    ")
}

fn create_get_nonconst_entities (settings: &[String]) -> String {
    let forward = create_forward_as_tuple(settings);
    format!("
    template <typename AllEntities> static inline auto get_nonconst_entities(AllEntities& in) {{
        {forward}
    }}
    ")
}


fn create_forward_as_tuple(settings: &[String]) -> String {
    let adjusted = settings.iter().map(|col| format!("in.{col}")).join(",\n");
    format!("
        return std::forward_as_tuple(
            {}
        );
    ", adjusted)
}

fn get_perm_side<F: FieldElement>(def: &SelectedExpressions<AlgebraicExpression<F>>) -> PermSide {
    let get_name = |expr: &AlgebraicExpression<F>| match expr {
        AlgebraicExpression::Reference(a_ref) => sanitize_name(&a_ref.name),
        _ => panic!("Expected reference")
    };

    PermSide {
        selector: def.selector.as_ref().map(|expr| get_name(&expr)),
        cols: def.expressions.iter().map(|expr| get_name(&expr)).collect_vec()
    }
}



// pub struct SelectedExpressions<Expr> {
//     pub selector: Option<Expr>,
//     pub expressions: Vec<Expr>,
// }



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
