use std::{
    marker::PhantomData,
    ops::{Index, IndexMut, Range},
};

use powdr_ast::analyzed::{PolyID, PolynomialType};

// Marker types for each PolynomialType
#[derive(Clone, Copy)]
pub struct Witness;

#[derive(Clone, Copy)]
pub struct Fixed;

pub trait PolynomialTypeTrait {
    const P_TYPE: PolynomialType;
}

impl PolynomialTypeTrait for Witness {
    const P_TYPE: PolynomialType = PolynomialType::Committed;
}

impl PolynomialTypeTrait for Fixed {
    const P_TYPE: PolynomialType = PolynomialType::Constant;
}

pub type WitnessColumnMap<V> = ColumnMap<V, Witness>;
pub type FixedColumnMap<V> = ColumnMap<V, Fixed>;

/// A Map indexed by polynomial ID, for a specific polynomial type (e.g. fixed or witness).
/// For performance reasons, it uses a Vec<V> internally and assumes that the polynomial IDs
/// are contiguous.
/// If the IDs are not contiguous, accessing the columns between the minimal and maximal column ID
/// will not lead to an error.
#[derive(Clone)]
pub struct ColumnMap<V, T: PolynomialTypeTrait> {
    values: Vec<V>,
    /// The first column ID in the map.
    // min_column_id: u64,
    // /// The number of columns in the map.
    // column_count: u64,
    _ptype: PhantomData<T>,
}

impl<V: Clone, T: PolynomialTypeTrait> ColumnMap<V, T> {
    /// Create a new ColumnMap with the given initial value and size.
    pub fn new(initial_value: V, column_range: Range<usize>) -> Self {
        assert_eq!(column_range.start, 0);
        ColumnMap {
            values: vec![initial_value; column_range.end - column_range.start],
            _ptype: PhantomData,
        }
    }
}

impl<V, T: PolynomialTypeTrait> ColumnMap<V, T> {
    pub fn from(column_id_range: Range<usize>, values: impl Iterator<Item = V>) -> Self {
        assert_eq!(column_id_range.start, 0);
        let values: Vec<_> = values.collect();
        assert_eq!(values.len(), column_id_range.end);
        ColumnMap {
            values,
            _ptype: PhantomData,
        }
    }

    /// Creates a ColumnMap from an iterator over PolyIDs and values.
    pub fn from_indexed(items: impl Iterator<Item = (PolyID, V)>, len: usize) -> Self
    where
        V: Default,
    {
        let mut values: Vec<V> = (0..len).map(|_| V::default()).collect();
        for (poly, value) in items {
            values[poly.id as usize] = value;
            debug_assert_eq!(poly.ptype, T::P_TYPE);
        }

        ColumnMap {
            values,
            _ptype: PhantomData,
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = PolyID> {
        (0..self.values.len()).map(move |i| PolyID {
            id: i as u64,
            ptype: T::P_TYPE,
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (PolyID, &V)> {
        self.keys().zip(self.values.iter())
    }

    pub fn into_iter(self) -> impl Iterator<Item = (PolyID, V)> {
        self.keys().zip(self.values)
    }

    // TODO check
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.iter()
    }

    // TODO check
    pub fn values_into_iter(self) -> impl Iterator<Item = V> {
        self.values.into_iter()
    }

    pub fn values_iter_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.values.iter_mut()
    }

    // TODO remove in the long term
    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn column_id_range(&self) -> Range<usize> {
        0..self.values.len()
    }
}

impl<V, T: PolynomialTypeTrait> Default for ColumnMap<V, T> {
    fn default() -> Self {
        ColumnMap {
            values: Vec::new(),
            _ptype: PhantomData,
        }
    }
}

impl<V: PartialEq, T: PolynomialTypeTrait> PartialEq for ColumnMap<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<V, T: PolynomialTypeTrait> Index<&PolyID> for ColumnMap<V, T> {
    type Output = V;

    #[inline]
    fn index(&self, poly_id: &PolyID) -> &Self::Output {
        debug_assert!(poly_id.ptype == T::P_TYPE);
        &self.values[poly_id.id as usize]
    }
}

impl<V, T: PolynomialTypeTrait> IndexMut<&PolyID> for ColumnMap<V, T> {
    #[inline]
    fn index_mut(&mut self, poly_id: &PolyID) -> &mut Self::Output {
        debug_assert!(poly_id.ptype == T::P_TYPE);
        &mut self.values[poly_id.id as usize]
    }
}
