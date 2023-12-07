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
    let shifted_polys = create_relation_files(&bb_files, file_name, &analyzed_identities);

    // ----------------------- Handle Lookup / Permutation Relation Identities -----------------------
    // We will likely need to create a new set of permutation identities for each lookup / permutation pair we come across
    handle_permutations(analyzed);
    // handle_lookups();



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
    } = get_all_col_names(fixed, witness, &shifted_polys);

    // bb_files.create_declare_views(file_name, &all_cols_with_shifts);

    // // ----------------------- Create the circuit builder file -----------------------
    // bb_files.create_circuit_builder_hpp(
    //     file_name,
    //     &relations,
    //     &all_cols,
    //     &to_be_shifted,
    //     &all_cols_with_shifts,
    // );

    // // ----------------------- Create the flavor file -----------------------
    // bb_files.create_flavor_hpp(
    //     file_name,
    //     &relations,
    //     &fixed,
    //     &witness,
    //     &all_cols,
    //     &to_be_shifted,
    //     &shifted,
    //     &all_cols_with_shifts,
    // );

    // // ----------------------- Create the composer files -----------------------
    // bb_files.create_composer_cpp(file_name, &all_cols);
    // bb_files.create_composer_hpp(file_name);

    // // ----------------------- Create the Verifier files -----------------------
    // bb_files.create_verifier_cpp(file_name, &witness);
    // bb_files.create_verifier_hpp(file_name);

    // // ----------------------- Create the Prover files -----------------------
    // bb_files.create_prover_cpp(file_name, &unshifted, &to_be_shifted);
    // bb_files.create_prover_hpp(file_name);
}


/// TODO: MOVE THIS OUT OF THIS FILE????
/// Does this need to return all of the shifted polys that it collects>
/// TODO: restructure this so that we do not have to pass in bb files abd the name at the top level
fn create_relation_files<F: FieldElement>(bb_files: &BBFiles, file_name: &str, analyzed_identities: &Vec<Identity<AlgebraicExpression<F>>>) -> Vec<String> {
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

    shifted_polys

}


// Maybe this is not the right thing to do?
#[derive(Debug)]
struct Permutation {
    left: PermSide,
    right: PermSide
}

// TODO: rename
#[derive(Debug)]
struct PermSide {
    selector: Option<String>,
    cols: Vec<String>
}

fn handle_permutations<F: FieldElement>(analyzed: &Analyzed<F>) -> Vec<Permutation> {
    let perms: Vec<&Identity<AlgebraicExpression<F>>> = analyzed.identities.iter().filter(|identity| matches!(identity.kind, IdentityKind::Permutation)).collect();
    let new_perms = perms.iter().map(|perm| 
        Permutation {
            left: get_perm_side(&perm.left),
            right: get_perm_side(&perm.right)
        }).collect_vec();

    dbg!(&new_perms);

    // For every permutation set that we create, we will need to create an inverse column ( helper function )
    // TODO: how do we determine the name of this inverse column?


    new_perms
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
) -> ColumnGroups {
    // Transformations
    let sanitize = |(name, _): &(String, Vec<F>)| sanitize_name(name).to_owned();
    let append_shift = |name: &String| format!("{}_shift", *name);

    // Gather sanitized column names
    let fixed_names = collect_col(fixed, sanitize);
    let witness_names = collect_col(witness, sanitize);

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
