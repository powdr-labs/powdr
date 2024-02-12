use crate::{
    file_writer::BBFiles,
    utils::{create_get_const_entities, create_get_nonconst_entities},
};
use ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, IdentityKind},
    parsed::SelectedExpressions,
};
use itertools::Itertools;
use number::FieldElement;

use crate::utils::sanitize_name;

#[derive(Debug)]
/// Permutation
///
/// Contains the information required to produce a permutation relation
pub struct Permutation {
    /// -> Attribute - the name given to the inverse helper column
    pub attribute: Option<String>,
    /// -> PermSide - the left side of the permutation
    pub left: PermutationSide,
    /// -> PermSide - the right side of the permutation
    pub right: PermutationSide,
}

#[derive(Debug)]
/// PermSide
///
/// One side of a two sided permutation relationship
pub struct PermutationSide {
    /// -> Option<String> - the selector for the permutation ( on / off toggle )
    selector: Option<String>,
    /// The columns involved in this side of the permutation
    cols: Vec<String>,
}

pub trait PermutationBuilder {
    /// Takes in an AST and works out what permutation relations are needed
    /// Note: returns the name of the inverse columns, such that they can be added to he prover in subsequent steps
    fn create_permutation_files<F: FieldElement>(
        &self,
        name: &str,
        analyzed: &Analyzed<F>,
    ) -> Vec<Permutation>;
}

impl PermutationBuilder for BBFiles {
    fn create_permutation_files<F: FieldElement>(
        &self,
        project_name: &str,
        analyzed: &Analyzed<F>,
    ) -> Vec<Permutation> {
        let perms: Vec<&Identity<AlgebraicExpression<F>>> = analyzed
            .identities
            .iter()
            .filter(|identity| matches!(identity.kind, IdentityKind::Permutation))
            .collect();
        let new_perms = perms
            .iter()
            .map(|perm| Permutation {
                attribute: perm.attribute.clone(),
                left: get_perm_side(&perm.left),
                right: get_perm_side(&perm.right),
            })
            .collect_vec();

        create_permutations(self, project_name, &new_perms);
        new_perms
    }
}

/// The attributes of a permutation contain the name of the inverse, we collect all of these to create the inverse column
pub fn get_inverses_from_permutations(permutations: &[Permutation]) -> Vec<String> {
    permutations
        .iter()
        .map(|perm| perm.attribute.clone().unwrap())
        .collect()
}

/// Write the permutation settings files to disk
fn create_permutations(bb_files: &BBFiles, project_name: &str, permutations: &Vec<Permutation>) {
    for permutation in permutations {
        let perm_settings = create_permutation_settings_file(permutation);

        let folder = format!("{}/{}", bb_files.rel, project_name);
        let file_name = format!(
            "{}{}",
            permutation.attribute.clone().unwrap_or("NONAME".to_owned()),
            ".hpp".to_owned()
        );
        bb_files.write_file(&folder, &file_name, &perm_settings);
    }
}

/// All relation types eventually get wrapped in the relation type
/// This function creates the export for the relation type so that it can be added to the flavor
fn create_relation_exporter(permutation_name: &str) -> String {
    let settings_name = format!("{}_permutation_settings", permutation_name);
    let permutation_export = format!("template <typename FF_> using {permutation_name}_relation = GenericPermutationRelation<{settings_name}, FF_>;");
    let relation_export = format!("template <typename FF_> using {permutation_name} = GenericPermutation<{settings_name}, FF_>;");

    format!(
        "
    {permutation_export} 
    {relation_export} 
    "
    )
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
    let permutation_name = permutation
        .attribute
        .clone()
        .expect("Inverse column name must be provided"); // TODO(md): catch this earlier than here

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
        selector.clone(), // TODO: update this away from the simple example
    ]
    .to_vec();

    perm_entities.extend(lhs_cols);
    perm_entities.extend(rhs_cols);

    let permutation_settings_includes = permutation_settings_includes();
    let inverse_computed_at = create_inverse_computed_at(selector);
    let const_entities = create_get_const_entities(&perm_entities);
    let nonconst_entities = create_get_nonconst_entities(&perm_entities);
    let relation_exporter = create_relation_exporter(&permutation_name);

    format!(
        // TODO: replace with the inverse label name!
        "
        {permutation_settings_includes}

        namespace bb {{

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

fn get_perm_side<F: FieldElement>(
    def: &SelectedExpressions<AlgebraicExpression<F>>,
) -> PermutationSide {
    let get_name = |expr: &AlgebraicExpression<F>| match expr {
        AlgebraicExpression::Reference(a_ref) => sanitize_name(&a_ref.name),
        _ => panic!("Expected reference"),
    };

    PermutationSide {
        selector: def.selector.as_ref().map(|expr| get_name(expr)),
        cols: def
            .expressions
            .iter()
            .map(|expr| get_name(expr))
            .collect_vec(),
    }
}
