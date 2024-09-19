use std::btree::new;
use std::btree::insert;
use std::btree::get;
use std::btree::CmpResult;
use std::btree::BTree;

let cmp: int, int -> CmpResult = |a, b|
    if a < b {
        CmpResult::Less
    } else {
        if a == b {
            CmpResult::Equal
        } else {
            CmpResult::Greater
        }
    };
let b1 = new::<int, string>();
let b2 = insert(b1, (1, "a"), cmp);
let b3 = insert(b2, (8, "b"), cmp);
let b4 = insert(b3, (4, "c"), cmp);
let b5 = insert(b4, (2, "d"), cmp);
let b6 = insert(b5, (9, "e"), cmp);
let b7 = insert(b6, (3, "f"), cmp);
let b8 = insert(b7, (7, "g"), cmp);
let b9 = insert(b8, (5, "h"), cmp);
let b10 = insert(b9, (6, "i"), cmp);
let b11 = insert(b10, (0, "j"), cmp);
let b12 = insert(b11, (10, "k"), cmp);
// this one replaces
let b13 = insert(b12, (4, "l"), cmp);
let b14 = insert(b13, (6, "m"), cmp);

let one: int = 1;
let false: bool = one == 0;

use std::debug::print;
use std::debug::println;
let spaces: int -> string = |n|
    if n == 0 {
        ""
    } else {
        let s = spaces(n / 2);
        match n % 2 {
            0 => s + s,
            1 => s + s +  " ",
        }
    };
let print_item: (int, string), int -> () = |(k, v), indent| {
    let _ = print(spaces(indent * 4));
    let _ = print(k);
    let _ = print(": ");
    let _ = println(v);
    ()
};

let print_btree: BTree<int, string>, int -> () = |n, indent| match n {
    BTree::Inner(items, children) => {
        let _ = std::array::map_enumerated(children, |i, child| {
            let _ = print_btree(child, indent + 1);
            if i < std::array::len(items) {
                print_item(items[i], indent)
            } else {
                ()
            }
        });
        ()
    },
    BTree::Leaf(items) => {
        let _ = std::array::map(items, |(k, v)| print_item((k, v), indent));
        ()
    }
};

let expect: BTree<int, string>, int, string -> () = |b_tree, k, v|
    match get(b_tree, k, cmp) {
        Option::Some(x) => std::check::assert(x == v, || ""),
        _ => std::check::assert(false, || ""),
    };

let do_print: BTree<int, string> -> () = |b_tree| {
    let _ = print_btree(b_tree, 0);
    ()
};

machine Main {
    expect(b14, 6, "m");
    expect(b13, 6, "i");
    expect(b14, 4, "l");
    expect(b14, 8, "b");
    expect(b14, 1, "a");
    expect(b14, 7, "g");
    expect(b14, 0, "j");
    expect(b14, 9, "e");
    do_print(b14);

    let skewed = std::utils::fold(200, |i| i, new::<int, string>(), |tree, i| insert(tree, (i, ""), cmp));
    do_print(skewed);
}
