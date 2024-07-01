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
    /// The range of column IDs in the vector.
    column_id_range: Range<usize>,
    _ptype: PhantomData<T>,
}

impl<V: Clone, T: PolynomialTypeTrait> ColumnMap<V, T> {
    /// Create a new ColumnMap with the given initial value and size.
    pub fn new(initial_value: V, column_id_range: Range<usize>) -> Self {
        ColumnMap {
            values: vec![initial_value; column_id_range.len()],
            column_id_range,
            _ptype: PhantomData,
        }
    }
}

impl<V, T: PolynomialTypeTrait> ColumnMap<V, T> {
    /// Creates a new ColumnMap from an iterator over values, corresponding
    /// to the keys in the given range, in the iteration order.
    pub fn from(column_id_range: Range<usize>, values: impl Iterator<Item = V>) -> Self {
        let values: Vec<_> = values.collect();
        assert_eq!(values.len(), column_id_range.len());
        ColumnMap {
            values,
            column_id_range,
            _ptype: PhantomData,
        }
    }

    /// Creates a ColumnMap from an iterator over PolyIDs and values.
    pub fn from_indexed(
        column_id_range: Range<usize>,
        items: impl Iterator<Item = (PolyID, V)>,
    ) -> Self
    where
        V: Default,
    {
        let mut values: Vec<V> = (0..column_id_range.len()).map(|_| V::default()).collect();
        for (poly, value) in items {
            values[poly.id as usize - column_id_range.start] = value;
            debug_assert_eq!(poly.ptype, T::P_TYPE);
        }

        ColumnMap {
            values,
            column_id_range,
            _ptype: PhantomData,
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = PolyID> {
        self.column_id_range.clone().into_iter().map(|i| PolyID {
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

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PolyID, &mut V)> {
        self.keys().zip(self.values.iter_mut())
    }

    /// Returns an iterator over the values, in the order of the keys.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.iter()
    }

    /// Returns an iterator over the values, in the order of the keys.
    pub fn values_into_iter(self) -> impl Iterator<Item = V> {
        self.values.into_iter()
    }

    /// Returns a mutating iterator over the values, in the order of the keys.
    pub fn values_iter_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.values.iter_mut()
    }

    pub fn column_id_range(&self) -> Range<usize> {
        self.column_id_range.clone()
    }
}

impl<V, T: PolynomialTypeTrait> Default for ColumnMap<V, T> {
    fn default() -> Self {
        ColumnMap {
            values: Vec::new(),
            column_id_range: Default::default(),
            _ptype: PhantomData,
        }
    }
}

impl<V: PartialEq, T: PolynomialTypeTrait> PartialEq for ColumnMap<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.column_id_range == other.column_id_range && self.values == other.values
    }
}

impl<V, T: PolynomialTypeTrait> Index<&PolyID> for ColumnMap<V, T> {
    type Output = V;

    #[inline]
    fn index(&self, poly_id: &PolyID) -> &Self::Output {
        debug_assert!(poly_id.ptype == T::P_TYPE);
        &self.values[poly_id.id as usize - self.column_id_range.start]
    }
}

impl<V, T: PolynomialTypeTrait> IndexMut<&PolyID> for ColumnMap<V, T> {
    #[inline]
    fn index_mut(&mut self, poly_id: &PolyID) -> &mut Self::Output {
        debug_assert!(poly_id.ptype == T::P_TYPE);
        &mut self.values[poly_id.id as usize - self.column_id_range.start]
    }
}
