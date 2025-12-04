use std::collections::BTreeSet;

use serde::Serialize;

/// An equivalence class 
pub type EquivalenceClass<T> = BTreeSet<T>;

/// A collection of equivalence classes where all classes are guaranteed to have at least two elements
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct EquivalenceClasses<T> {
    inner: BTreeSet<EquivalenceClass<T>>,
}

// TODO: derive
impl<T> Default for EquivalenceClasses<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
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
