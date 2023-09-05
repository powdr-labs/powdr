use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use ast::analyzed::{PolyID, PolynomialType};

// Marker types for each PolynomialType
#[derive(Clone, Copy)]
pub struct Committed;

#[derive(Clone, Copy)]
pub struct Constant;

pub trait PolynomialTypeTrait {
    fn ptype() -> PolynomialType;
}

impl PolynomialTypeTrait for Committed {
    fn ptype() -> PolynomialType {
        PolynomialType::Committed
    }
}

impl PolynomialTypeTrait for Constant {
    fn ptype() -> PolynomialType {
        PolynomialType::Constant
    }
}

/// A Map indexed by polynomial ID, for a specific polynomial type (e.g. fixed or witness).
/// For performance reasons, it uses a Vec<V> internally and assumes that the polynomial IDs
/// are contiguous.
#[derive(Clone)]
pub struct ColumnMap<V, T: PolynomialTypeTrait> {
    values: Vec<V>,
    _ptype: PhantomData<T>,
}

impl<V: Clone, T: PolynomialTypeTrait> ColumnMap<V, T> {
    /// Create a new ColumnMap with the given initial value and size.
    pub fn new(initial_value: V, size: usize) -> Self {
        ColumnMap {
            values: vec![initial_value; size],
            _ptype: PhantomData,
        }
    }
}

impl<V, T: PolynomialTypeTrait> ColumnMap<V, T> {
    pub fn from(values: impl Iterator<Item = V>) -> Self {
        ColumnMap {
            values: values.collect(),
            _ptype: PhantomData,
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = PolyID> {
        (0..self.values.len()).map(move |i| PolyID {
            id: i as u64,
            ptype: T::ptype(),
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (PolyID, &V)> {
        self.keys().zip(self.values.iter())
    }

    pub fn into_iter(self) -> impl Iterator<Item = (PolyID, V)> {
        self.keys().zip(self.values)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.iter()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<V, T: PolynomialTypeTrait> Index<&PolyID> for ColumnMap<V, T> {
    type Output = V;

    fn index(&self, poly_id: &PolyID) -> &Self::Output {
        assert!(poly_id.ptype == T::ptype());
        &self.values[poly_id.id as usize]
    }
}

impl<V, T: PolynomialTypeTrait> IndexMut<&PolyID> for ColumnMap<V, T> {
    fn index_mut(&mut self, poly_id: &PolyID) -> &mut Self::Output {
        assert!(poly_id.ptype == T::ptype());
        &mut self.values[poly_id.id as usize]
    }
}
