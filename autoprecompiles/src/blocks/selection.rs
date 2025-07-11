use std::{
    cmp::{Ordering, Reverse},
    collections::BinaryHeap,
    iter::once,
};

use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub trait KnapsackItem {
    /// Cost of the item, used for sorting and knapsack algorithm.
    fn cost(&self) -> usize;
    /// Value of the item, used for sorting and knapsack algorithm. Should be much larger than `cost` to avoid ties.
    fn value(&self) -> usize;
    /// Tie breaker for the case when two candidates have the same cost and value. When a tie occurs, the item with higher value of this function is chosen.
    fn tie_breaker(&self) -> usize;
}

/// Fractional knapsack algorithm that uses parallel iterators to find the best items.
/// It returns an iterator over the items that fit into the knapsack, sorted by their density (value/cost).
pub(crate) fn parallel_fractional_knapsack<E: KnapsackItem + Send>(
    elements: impl IntoParallelIterator<Item = E>,
    max_count: usize,
    max_cost: Option<usize>,
) -> impl Iterator<Item = E> {
    struct KnapsackItemWrapper<E> {
        item: E,
    }

    impl<E: KnapsackItem> KnapsackItemWrapper<E> {
        fn density(&self) -> usize {
            // Note: If the value and cost are of similar magnitude, this would lead to ties.
            self.item.value() / self.item.cost()
        }
    }

    impl<E: KnapsackItem> Ord for KnapsackItemWrapper<E> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.density()
                .cmp(&other.density())
                .then_with(|| self.item.tie_breaker().cmp(&other.item.tie_breaker()))
        }
    }

    impl<E: KnapsackItem> PartialEq for KnapsackItemWrapper<E> {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == Ordering::Equal
        }
    }
    impl<E: KnapsackItem> Eq for KnapsackItemWrapper<E> {}
    impl<E: KnapsackItem> PartialOrd for KnapsackItemWrapper<E> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    elements
        .into_par_iter()
        .map(|e| once(Reverse(KnapsackItemWrapper { item: e })).collect())
        .reduce(BinaryHeap::new, |mut acc, mut heap| {
            for elem in heap.drain() {
                acc.push(elem);
                if acc.len() > max_count {
                    acc.pop();
                }
            }
            acc
        })
        // TODO: use `into_sorted_iter` when it is available without nightly feature
        .into_sorted_vec()
        .into_iter()
        .map(|Reverse(e)| e.item)
        .scan(0, move |cumulative_cost, e| {
            if let Some(max_cost) = max_cost {
                // Try to add the item
                if *cumulative_cost + e.cost() <= max_cost {
                    // The item fits, increment the cumulative cost
                    *cumulative_cost += e.cost();
                    Some(Some(e))
                } else {
                    // The item does not fit, skip it
                    Some(None)
                }
            } else {
                // No max cost, just return the item
                Some(Some(e))
            }
        })
        .flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct TestItem {
        index: usize,
        cost: usize,
        value: usize,
    }

    impl TestItem {
        fn new(index: usize, cost: usize, density: usize) -> Self {
            Self {
                index,
                cost,
                value: cost * density,
            }
        }
    }

    impl KnapsackItem for TestItem {
        fn cost(&self) -> usize {
            self.cost
        }

        fn value(&self) -> usize {
            self.value
        }

        fn tie_breaker(&self) -> usize {
            self.index
        }
    }

    #[test]
    fn tie() {
        let items = vec![TestItem::new(0, 1, 10), TestItem::new(1, 1, 10)];

        let max_count = 10;
        let max_cost = 1;

        // In case of tie, the second item (with larger index) should be chosen
        for _ in 0..10 {
            let result: Vec<_> =
                parallel_fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.len(), 1);
            assert_eq!(result[0].index, 1);
        }
    }

    #[test]
    fn all_items_fit() {
        let items = vec![TestItem::new(0, 1, 2), TestItem::new(1, 2, 1)];

        let max_count = 10;
        let max_cost = 3;

        // All items fit, so both should be returned in the order of their (density, index)
        for _ in 0..10 {
            let result: Vec<_> =
                parallel_fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result, items);
        }
    }

    #[test]
    fn some_items_fit() {
        let items = vec![
            TestItem::new(0, 1, 3),
            TestItem::new(1, 2, 2),
            TestItem::new(2, 3, 1),
        ];

        let max_count = 10;
        let max_cost = 3;

        // Only the first two items fit, since their costs add up to 3 and they have the highest density
        for _ in 0..10 {
            let result: Vec<_> =
                parallel_fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.as_slice(), &items[0..2]);
        }
    }

    #[test]
    fn many_with_ties_and_skips() {
        let items = vec![
            TestItem::new(1, 1, 10),
            TestItem::new(0, 1, 10),
            TestItem::new(3, 2, 5),
            TestItem::new(2, 2, 5),
            TestItem::new(4, 3, 3), // should be skipped due to cost
            TestItem::new(5, 1, 2),
        ];

        let max_count = 10;
        let max_cost = 7;

        // Only the first four items fit, since they have the highest density and their costs add up to 6 with the final item blowing up the max_cost.
        // Due to the same density, tie is broken by items with higher index coming up first.
        for _ in 0..10 {
            let result: Vec<_> =
                parallel_fractional_knapsack(items.clone(), max_count, Some(max_cost)).collect();
            assert_eq!(result.len(), 5);
            assert_eq!(result[0].index, 1);
            assert_eq!(result[1].index, 0);
            assert_eq!(result[2].index, 3);
            assert_eq!(result[3].index, 2);
            assert_eq!(result[4].index, 5);
        }
    }
}
