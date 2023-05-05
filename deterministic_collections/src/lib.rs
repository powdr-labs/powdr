//! Deterministic versions of HashMap and HashSet.
//!
//! They are deterministic for a given rust std library version. Beware of
//! HashDoS attacks.

type DefaultHasherBuilder =
    std::hash::BuildHasherDefault<std::collections::hash_map::DefaultHasher>;

/// Deterministic HashMap.
pub type DetHashMap<K, V> = std::collections::HashMap<K, V, DefaultHasherBuilder>;

/// Deterministic HashSet.
pub type DetHashSet<T> = std::collections::HashSet<T, DefaultHasherBuilder>;
