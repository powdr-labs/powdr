use std::ops::{Index, IndexMut};

use ast::analyzed::{PolyID, PolynomialType};

/// A Map indexed by polynomial ID, for a specific polynomial type (e.g. fixed or witness).
/// For performance reasons, it uses a Vec<V> internally and assumes that the polynomial IDs
/// are contiguous.
#[derive(Clone)]
pub struct ColumnMap<V> {
    values: Vec<V>,
    ptype: PolynomialType,
}

impl<V: Clone> ColumnMap<V> {
    /// Create a new ColumnMap with the given initial value and polynomial type.
    pub fn new(initial_value: V, capacity: usize, ptype: PolynomialType) -> Self {
        ColumnMap {
            values: vec![initial_value; capacity],
            ptype,
        }
    }
}

impl<V> ColumnMap<V> {
    pub fn from(values: impl Iterator<Item = V>, ptype: PolynomialType) -> Self {
        ColumnMap {
            values: values.collect(),
            ptype,
        }
    }
}

impl<V> ColumnMap<V> {
    pub fn keys(&self) -> impl Iterator<Item = PolyID> {
        let ptype = self.ptype;
        (0..self.values.len()).map(move |i| PolyID {
            id: i as u64,
            ptype,
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (PolyID, &V)> {
        self.keys().zip(self.values.iter())
    }

    pub fn into_iter(self) -> impl Iterator<Item = (PolyID, V)> {
        self.keys().zip(self.values.into_iter())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.iter()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<V: Clone + Default> ColumnMap<Option<V>> {
    pub fn unwrap_or_default(&self) -> ColumnMap<V> {
        ColumnMap {
            values: self
                .values
                .iter()
                .map(|v| v.clone().unwrap_or_default())
                .collect(),
            ptype: self.ptype,
        }
    }
}

impl<V: Clone> ColumnMap<V> {
    pub fn wrap_some(&self) -> ColumnMap<Option<V>> {
        ColumnMap {
            values: self.values.iter().map(|v| Some(v.clone())).collect(),
            ptype: self.ptype,
        }
    }
}

impl<V> Index<&PolyID> for ColumnMap<V> {
    type Output = V;

    fn index(&self, poly_id: &PolyID) -> &Self::Output {
        assert!(poly_id.ptype == self.ptype);
        &self.values[poly_id.id as usize]
    }
}

impl<V> IndexMut<&PolyID> for ColumnMap<V> {
    fn index_mut(&mut self, poly_id: &PolyID) -> &mut Self::Output {
        assert!(poly_id.ptype == self.ptype);
        &mut self.values[poly_id.id as usize]
    }
}
