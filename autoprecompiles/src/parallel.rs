//! Helpers for parallel processing of autoprecompile candidates.

use std::cmp::Reverse;
use std::sync::Mutex;

use rayon::prelude::*;

/// Applies `f` to all items in parallel, processing the items in decreasing
/// `weight` order (longest-processing-time-first scheduling): every idle
/// worker picks up the heaviest remaining item, so that expensive items
/// cannot start late and extend the overall runtime.
///
/// The results are returned in the original order of `items`, like
/// `items.into_par_iter().filter_map(f).collect()` would.
///
/// Note that sorting the items and using a plain parallel iterator would not
/// achieve this: rayon splits the input range into per-worker segments,
/// which concentrates the heaviest items on few workers.
pub fn filter_map_largest_first<T: Send, R: Send>(
    items: Vec<T>,
    weight: impl Fn(&T) -> usize,
    f: impl Fn(T) -> Option<R> + Send + Sync,
) -> Vec<R> {
    let mut indexed: Vec<(usize, T)> = items.into_iter().enumerate().collect();
    // Ties are broken by the original index for determinism of the
    // processing order (the result is deterministic either way).
    indexed.sort_by_key(|(index, item)| (Reverse(weight(item)), *index));

    let queue = Mutex::new(indexed.into_iter());
    let mut results = (0..rayon::current_num_threads())
        .into_par_iter()
        .map(|_| {
            let mut results = Vec::new();
            loop {
                // Note that the lock is only held to take the next item,
                // not while processing it.
                let Some((index, item)) = queue.lock().unwrap().next() else {
                    return results;
                };
                if let Some(result) = f(item) {
                    results.push((index, result));
                }
            }
        })
        .flatten_iter()
        .collect::<Vec<_>>();
    results.sort_by_key(|(index, _)| *index);
    results.into_iter().map(|(_, result)| result).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keeps_original_order_and_filters() {
        let items = (0..1000).collect::<Vec<_>>();
        let results = filter_map_largest_first(items, |i| *i, |i| (i % 3 != 0).then_some(i * 2));
        let expected = (0..1000)
            .filter(|i| i % 3 != 0)
            .map(|i| i * 2)
            .collect::<Vec<_>>();
        assert_eq!(results, expected);
    }
}
