use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

/// An equivalence class, i.e, a set of values of type `T` which are considered equivalent
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are guaranteed to have at least two elements
/// This is enforced by construction of this type only happening through collection, where we ignore empty and singleton classes
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(bound(deserialize = "T: Ord + Deserialize<'de>"))]
pub struct EquivalenceClasses<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
}

impl<T: Ord> FromIterator<EquivalenceClass<T>> for EquivalenceClasses<T> {
    fn from_iter<I: IntoIterator<Item = EquivalenceClass<T>>>(iter: I) -> Self {
        // When collecting, we ignore classes with 0 or 1 elements as they are useless
        Self {
            inner: iter.into_iter().filter(|class| class.len() > 1).collect(),
        }
    }
}

impl<T> EquivalenceClasses<T> {
    pub fn iter(&self) -> impl Iterator<Item = &EquivalenceClass<T>> {
        self.inner.iter()
    }
}
