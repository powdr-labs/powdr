/// This is a BTree, which means that each node has
/// up to `max_items` items and a number of children
/// that is one more than the number of items.
/// The items are key-value-pairs sorted by key and
/// the key of the i-th item is greater than all keys
/// in children[i] and less than all keys in children[i + 1].
/// Leaf nodes do not have children.
enum BTree<K, V> {
    Inner((K, V)[], BTree<K, V>[]),
    Leaf((K, V)[]),
}

// Rust actually uses 11, but I have the impression that longer
// arrays do not have that much of an advantage here.
let max_items = 5;

enum CmpResult {
    Less,
    Equal,
    Greater,
}

/// Creates an empty BTree.
let<K, V> new: -> BTree<K, V> = || BTree::Leaf([]);

/// Returns Some(v) if v is the value corresponding to key `k` or None otherwise.
let<K, V> get: BTree<K, V>, K, (K, K -> CmpResult) -> Option<V> = internal::get::<K, V>;

/// Inserts the key-value-pair (k, v) into the BTree and returns the updated BTree.
let<K, V> insert: BTree<K, V>, (K, V), (K, K -> CmpResult) -> BTree<K, V> =
    |b_tree, (k, v), cmp| match internal::insert(b_tree, (k, v), cmp) {
        internal::InsertResult::Split(item, left, right) => BTree::Inner([item], [left, right]),
        internal::InsertResult::Updated(t) => t,
    };

mod internal {
    use super::BTree;
    use super::CmpResult;

    use std::utils::unwrap_or_else;
    use std::array::find_map;
    use std::array::find_map_enumerated;
    use std::array::sub_array;

    let <K, V> get: BTree<K, V>, K, (K, K -> CmpResult) -> Option<V> = |b_tree, k, cmp| match b_tree {
        BTree::Inner(items, children) => match search_in_node(items, k, cmp) {
            NodeSearchResult::InNode(i) => Option::Some(value_of_item(items[i])),
            NodeSearchResult::InChild(i) => get(children[i], k, cmp),
        },
        BTree::Leaf(items) => find_map(items, |(key, value)| match cmp(k, key) {
            CmpResult::Equal => Option::Some(value),
            _ => Option::None,
        })
    };

    let<K, V> value_of_item: (K, V) -> V = |(_, value)| value;

    enum NodeSearchResult {
        InNode(int),
        InChild(int),
    }

    let<K1, V, K2> search_in_node: (K1, V)[], K2, (K2, K1 -> CmpResult) -> NodeSearchResult = |items, k, cmp| {
        let r = find_map_enumerated(items, |i, (key, _)| match cmp(k, key) {
            CmpResult::Less => Option::Some(NodeSearchResult::InChild(i)),
            CmpResult::Equal => Option::Some(NodeSearchResult::InNode(i)),
            CmpResult::Greater => Option::None,
        });
        unwrap_or_else(r, || NodeSearchResult::InChild(std::array::len(items)))
    };

    enum InsertResult<K, V> {
        Split((K, V), BTree<K, V>, BTree<K, V>),
        Updated(BTree<K, V>)
    }
    let<K, V> insert: BTree<K, V>, (K, V), (K, K -> CmpResult) -> InsertResult<K, V> =
        |b_tree, (k, v), cmp| match b_tree {
            BTree::Leaf(items) => insert_into_leaf(items, (k, v), cmp),
            BTree::Inner(items, children) =>
                match search_in_node(items, k, cmp) {
                    NodeSearchResult::InNode(i) => InsertResult::Updated(BTree::Inner(std::array::set_element(items, i, (k, v)), children)),
                    NodeSearchResult::InChild(i) => match insert(children[i], (k, v), cmp) {
                        InsertResult::Updated(child) => InsertResult::Updated(BTree::Inner(items, std::array::set_element(children, i, child))),
                        InsertResult::Split((k1, v1), left, right) =>
                            insert_into_inner(items, children, (k1, v1), i, left, right),
                    }                    
                }
        };
    let<K, V> insert_into_leaf: (K, V)[], (K, V), (K, K -> CmpResult) -> InsertResult<K, V> = |items, (k, v), cmp| {
        let new_items = items_insert(items, (k, v), cmp);
        if std::array::len(new_items) <= super::max_items {
            InsertResult::Updated(BTree::Leaf(new_items))
        } else {
            split_leaf(new_items)
        }
    };
    let<K, V> split_leaf: (K, V)[] -> InsertResult<K, V> = |items| {
        let split = (std::array::len(items) - 1) / 2;
        let (left, center, right) = array_split_pivot(items, split);
        InsertResult::Split(center, BTree::Leaf(left), BTree::Leaf(right))
    };
    /// Splits an array into left and right part. The element at index i is
    /// not part of either and returned separately.
    let<T> array_split_pivot: T[], int -> (T[], T, T[]) = |arr, i| {
        let left = sub_array(arr, 0, i);
        let right = sub_array(arr, i + 1, std::array::len(arr) - i - 1);
        (left, arr[i], right)

    };
    /// Split an array into a sub-array of length l and the rest.
    let<T> array_split: T[], int -> (T[], T[]) = |arr, l| {
        let left = sub_array(arr, 0, l);
        let right = sub_array(arr, l, std::array::len(arr) - l);
        (left, right)

    };
    /// Inserts x at index i, shifting instead of replacing.
    let<T> array_insert_at: T[], int, T -> T[] = |arr, i, x| {
        let (left, right) = array_split(arr, i);
        left + [x] + right
    };
    let<K, V> insert_into_inner:
        (K, V)[], BTree<K, V>[], (K, V), int, BTree<K, V>, BTree<K, V> -> InsertResult<K, V> =
        |items, children, (k, v), i, left, right| {
            let new_items = array_insert_at(items, i, (k, v));
            // We replace the old children[i] by [left, right]
            let (children_left, _, children_right) = array_split_pivot(children, i);
            let new_children = children_left + [left, right] + children_right;
            if std::array::len(new_items) <= super::max_items {
                InsertResult::Updated(BTree::Inner(new_items, new_children))
            } else {
                split_inner(new_items, new_children)
            }
        };
    let<K, V> split_inner: (K, V)[], BTree<K, V>[] -> InsertResult<K, V> = |items, children| {
        let split = (std::array::len(items) - 1) / 2;
        let (left_items, push_up, right_items) = array_split_pivot(items, split);
        let (left_children, right_children) = array_split(children, split + 1);
        InsertResult::Split(
            push_up,
            BTree::Inner(left_items, left_children),
            BTree::Inner(right_items, right_children)
        )
    };
    let one: int = 1;
    /// Inserts (k, v) into the sorted list items.
    /// Returns the new list.
    let<K, V> items_insert: (K, V)[], (K, V), (K, K -> CmpResult) -> (K, V)[] = |items, (k, v), cmp| {
        let (new_items, ins) = std::array::fold(items, ([], false), |(acc, inserted), (key, value)|
            if inserted {
                (acc + [(key, value)], inserted)
            } else {
                match cmp(k, key) {
                    CmpResult::Less => (acc + [(k, v), (key, value)], true),
                    CmpResult::Equal => (acc + [(k, v)], true),
                    CmpResult::Greater => (acc + [(key, value)], false),
                }
            }
        );
        if ins {
            new_items
        } else {
            new_items + [(k, v)]
        }
    };
        
}
