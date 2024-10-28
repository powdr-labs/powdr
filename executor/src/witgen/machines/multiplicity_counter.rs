use std::collections::BTreeMap;

use powdr_ast::analyzed::PolyID;
use powdr_number::{DegreeType, FieldElement};

pub struct MultiplicityCounter {
    multiplicity_columns: BTreeMap<u64, PolyID>,
    counts: BTreeMap<PolyID, BTreeMap<usize, usize>>,
}

impl MultiplicityCounter {
    pub fn new(multiplicity_columns: BTreeMap<u64, PolyID>) -> Self {
        Self {
            multiplicity_columns,
            counts: BTreeMap::new(),
        }
    }

    pub fn increment(&mut self, identity_id: u64, index: usize) {
        if let Some(poly_id) = self.multiplicity_columns.get(&identity_id) {
            let count = self.counts.entry(*poly_id).or_default();
            *count.entry(index).or_insert(0) += 1;
        }
    }

    pub fn generate_columns_single_size<T: FieldElement>(
        &self,
        size: DegreeType,
    ) -> BTreeMap<PolyID, Vec<T>> {
        self.counts
            .iter()
            .map(|(poly_id, counts)| {
                let mut column = vec![T::zero(); size as usize];
                for (index, count) in counts {
                    column[*index] = T::from(*count as u64);
                }
                (*poly_id, column)
            })
            .collect()
    }

    pub fn generate_columns_different_sizes<T: FieldElement>(
        &self,
        sizes: BTreeMap<PolyID, DegreeType>,
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
