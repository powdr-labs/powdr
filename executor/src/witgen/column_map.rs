use std::{
    collections::BTreeMap,
    ops::{Index, IndexMut},
};

use ast::analyzed::{PolyID, PolynomialType};

/// A Map indexed by polynomial ID, for a specific polynomial type (e.g. fixed or witness).
/// For performance reasons, it uses a Vec<V> internally and assumes that the polynomial IDs
/// are contiguous.
#[derive(Clone)]
pub struct ColumnMap<V> {
    values: BTreeMap<PolyID, V>,
    ptype: PolynomialType,
}

impl<V: Clone> ColumnMap<V> {
    /// Create a new ColumnMap with the given initial value and polynomial type.
    pub fn new(initial_value: V, capacity: usize, ptype: PolynomialType) -> Self {
        ColumnMap::from(vec![initial_value; capacity].into_iter(), ptype)
    }
}

impl<V> ColumnMap<V> {
    pub fn from(values: impl Iterator<Item = V>, ptype: PolynomialType) -> Self {
        ColumnMap {
            values: values
                .enumerate()
                .map(|(i, v)| {
                    (
                        PolyID {
                            id: i as u64,
                            ptype,
                        },
                        v,
                    )
                })
                .collect(),
            ptype,
        }
    }
}

impl<V> ColumnMap<V> {
    pub fn keys(&self) -> impl Iterator<Item = PolyID> + '_ {
        self.values.keys().cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (PolyID, &V)> {
        self.values.iter().map(|(k, v)| (*k, v))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (PolyID, V)> {
        self.values.into_iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.values()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<V: Clone + Default> ColumnMap<Option<V>> {
    pub fn unwrap_or_default(self) -> ColumnMap<V> {
        ColumnMap {
            values: self
                .values
                .into_iter()
                .map(|(k, v)| (k, v.unwrap_or_default()))
                .collect(),
            ptype: self.ptype,
        }
    }
}

impl<V: Clone> ColumnMap<V> {
    pub fn wrap_some(self) -> ColumnMap<Option<V>> {
        ColumnMap {
            values: self.values.into_iter().map(|(k, v)| (k, Some(v))).collect(),
            ptype: self.ptype,
        }
    }
}

impl<V> Index<&PolyID> for ColumnMap<V> {
    type Output = V;

    fn index(&self, poly_id: &PolyID) -> &Self::Output {
        assert!(poly_id.ptype == self.ptype);
        &self.values[poly_id]
    }
}

impl<V> IndexMut<&PolyID> for ColumnMap<V> {
    fn index_mut(&mut self, poly_id: &PolyID) -> &mut Self::Output {
        assert!(poly_id.ptype == self.ptype);
        self.values.get_mut(poly_id).unwrap()
    }
}
