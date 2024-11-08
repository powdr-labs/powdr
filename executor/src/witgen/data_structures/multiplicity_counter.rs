use std::collections::BTreeMap;

use powdr_ast::analyzed::PolyID;
use powdr_number::{DegreeType, FieldElement};

use crate::witgen::machines::Connection;

/// Utility for counting the multiplicities for lookups.
pub struct MultiplicityCounter {
    /// Maps an identity ID to the corresponding multiplicity column.
    /// Note that multiple identity IDs can map to the same multiplicity column.
    identity_id_to_multiplicity_column: BTreeMap<u64, PolyID>,

    /// Optional poly_id -> size map.
    sizes: Option<BTreeMap<PolyID, DegreeType>>,

    /// A map poly_id -> (row -> count).
    /// Has a (possibly empty) entry for each value in `identity_id_to_multiplicity_column`.
    counts: BTreeMap<PolyID, BTreeMap<usize, usize>>,
}

impl MultiplicityCounter {
    pub fn new<T: FieldElement>(connections: &BTreeMap<u64, Connection<T>>) -> Self {
        Self::new_with_sizes(connections, BTreeMap::new())
    }

    pub fn new_with_sizes<T: FieldElement>(
        connections: &BTreeMap<u64, Connection<T>>,
        sizes: BTreeMap<PolyID, DegreeType>,
    ) -> Self {
        let identity_id_to_multiplicity_column = connections
            .iter()
            .filter_map(|(identity_id, connection)| {
                connection.multiplicity_column.map(|m| (*identity_id, m))
            })
            .collect::<BTreeMap<_, _>>();
        let counts = identity_id_to_multiplicity_column
            .values()
            .map(|poly_id| (*poly_id, BTreeMap::new()))
            .collect();
        Self {
            identity_id_to_multiplicity_column,
            sizes: Some(sizes),
            counts,
        }
    }

    /// For a given identity ID, increments the count of the corresponding multiplicity column at the given index.
    /// Does not fail if the identity does not have a multiplicity column.
    pub fn increment_at_row(&mut self, identity_id: u64, row: usize) {
        if let Some(poly_id) = self.identity_id_to_multiplicity_column.get(&identity_id) {
            // Every value of `identity_id_to_multiplicity_column` should have an entry in `counts`.
            let count = self.counts.get_mut(poly_id).unwrap();
            *count.entry(row).or_insert(0) += 1;
        }
    }

    /// Materializes the multiplicity columns, using the same size for all of them.
    pub fn generate_columns_single_size<T: FieldElement>(
        &self,
        size: DegreeType,
    ) -> BTreeMap<PolyID, Vec<T>> {
        self.generate_columns_with_sizes(
            // All polynomials have size `size`.
            &self.counts.keys().map(|poly_id| (*poly_id, size)).collect(),
        )
    }

    /// Materializes the multiplicity columns, using different sizes for each of them.
    pub fn generate_columns_different_sizes<T: FieldElement>(&self) -> BTreeMap<PolyID, Vec<T>> {
        self.generate_columns_with_sizes(self.sizes.as_ref().expect("Did not provide sizes!"))
    }

    fn generate_columns_with_sizes<T: FieldElement>(
        &self,
        sizes: &BTreeMap<PolyID, DegreeType>,
    ) -> BTreeMap<PolyID, Vec<T>> {
        self.counts
            .iter()
            .map(|(poly_id, counts)| {
                let size: DegreeType = sizes[poly_id];
                let mut column = vec![T::zero(); size as usize];
                for (index, count) in counts {
                    column[*index] = T::from(*count as u64);
                }
                (*poly_id, column)
            })
            .collect()
    }
}
