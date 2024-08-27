use powdr_number::GoldilocksField;
use powdr_pil_analyzer::analyze_string;
use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn new_witness_column() {
    let input = r#"namespace N(16);
    let even: col = |i| i * 2;
    let new_wit = constr || { let x; x };
    let new_wit_arr = constr || { let x; [x, x] };
    let x;
    let y;
    let z = new_wit();
    z = y;
    z $ [z] in [even];
    let t = new_wit_arr();
    t[0] = t[1];
    "#;
    let expected = r#"namespace N(16);
    col fixed even(i) { i * 2 };
    let new_wit: -> expr = (constr || {
        let x: col;
        x
    });
    let new_wit_arr: -> expr[] = (constr || {
        let x: col;
        [x, x]
    });
    col witness x;
    col witness y;
    let z: expr = N::new_wit();
    col witness x_1;
    N::x_1 = N::y;
    N::x_1 $ [N::x_1] in [N::even];
    let t: expr[] = N::new_wit_arr();
    col witness x_2;
    N::x_2 = N::x_2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn new_witness_column_name_clash() {
    let input = r#"namespace N(16);
    let new_wit = constr || { let x; x };
    new_wit() = new_wit() + new_wit();
    "#;
    let expected = r#"namespace N(16);
    let new_wit: -> expr = (constr || {
        let x: col;
        x
    });
    col witness x;
    col witness x_1;
    col witness x_2;
    N::x = N::x_1 + N::x_2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn create_constraints() {
    let input = r#"namespace N(16);
    let force_bool: expr -> Constr = |c| c * (1 - c) = 0;
    let new_bool: -> expr = constr || { let x; force_bool(x); x };
    let is_zero: expr -> expr = constr |x| {
        let x_is_zero;
        force_bool(x_is_zero);
        let x_inv;
        x_is_zero = 1 - x * x_inv;
        x_is_zero * x = 0;
        x_is_zero
    };
    let x;
    let x_is_zero = is_zero(x);
    let y;
    y = x_is_zero + 2;
    "#;
    let expected = r#"namespace N(16);
    let force_bool: expr -> std::prelude::Constr = (|c| c * (1 - c) = 0);
    let new_bool: -> expr = (constr || {
        let x: col;
        N::force_bool(x);
        x
    });
    let is_zero: expr -> expr = (constr |x| {
        let x_is_zero: col;
        N::force_bool(x_is_zero);
        let x_inv: col;
        x_is_zero = 1 - x * x_inv;
        x_is_zero * x = 0;
        x_is_zero
    });
    col witness x;
    let x_is_zero: expr = N::is_zero(N::x);
    col witness y;
    col witness x_is_zero_1;
    col witness x_inv;
    N::x_is_zero_1 * (1 - N::x_is_zero_1) = 0;
    N::x_is_zero_1 = 1 - N::x * N::x_inv;
    N::x_is_zero_1 * N::x = 0;
    N::y = N::x_is_zero_1 + 2;
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
pub fn degree() {
    let input = r#"
        namespace std::convert;
            let expr = [];
        namespace std::prover;
            let degree = [];
        namespace Main(8);
            let d = std::prover::degree();
            let w;
            w = std::convert::expr(d);
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace std::convert;
    let expr = [];
namespace std::prover;
    let degree = [];
namespace Main(8);
    let d: int = std::prover::degree();
    col witness w;
    Main::w = 8;
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Error: DataNotAvailable"]
pub fn degree_unset() {
    let input = r#"
        namespace std::convert;
            let expr = [];
        namespace std::prover;
            let degree = [];
        namespace Main;
            let d = std::prover::degree();
            let w;
            w = std::convert::expr(d);
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
pub fn constructed_constraints() {
    let input = r#"
        namespace Main(1024);
            let x;
            let y;
            let z;
            Constr::Identity(x, y);
            Constr::Lookup((Option::Some(1), Option::None), [(x, y), (3, z)]);
            Constr::Permutation((Option::None, Option::Some(x)), [(x, y), (3, z)]);
            Constr::Connection([(x, z), (y, 3)]);
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace Main(1024);
    col witness x;
    col witness y;
    col witness z;
    Main::x = Main::y;
    1 $ [Main::x, 3] in [Main::y, Main::z];
    [Main::x, 3] is Main::x $ [Main::y, Main::z];
    [Main::x, Main::y] connect [Main::z, 3];
"#;
    assert_eq!(formatted, expected);
}

#[test]
fn next() {
    let input = r#"namespace N(16);
        col witness x;
        col witness y;
        x * y = 1';
        x * y = (1 + x)';
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace N(16);
    col witness x;
    col witness y;
    N::x * N::y = 1;
    N::x * N::y = 1 + N::x';
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Double application of \\\"'\\\" on: N::x"]
fn double_next() {
    let input = r#"namespace N(16);
        col witness x;
        col witness y;
        x * y = (1 + x')';
    "#;
    analyze_string::<GoldilocksField>(input).to_string();
}

#[test]
fn new_fixed_column() {
    let input = r#"namespace N(16);
        let f = constr || {
            let even: col = |i| i * 2;
            even
        };
        let ev = f();
        let x;
        x = ev;
    "#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    let expected = r#"namespace N(16);
    let f: -> expr = (constr || {
        let even: col = (|i| i * 2);
        even
    });
    let ev: expr = N::f();
    col witness x;
    col fixed even(i) { i * 2 };
    N::x = N::even;
"#;
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Error creating fixed column N::fi: Lambda expression must not reference outer variables: (|i| (i + j) * 2)"]
fn new_fixed_column_as_closure() {
    let input = r#"namespace N(16);
        let f = constr |j| {
            let fi: col = |i| (i + j) * 2;
            fi
        };
        let ev = f(2);
        let x;
        x = ev;
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint() {
    let input = r#"
    namespace std::prover;
        let eval = 8;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        let y;
        std::prelude::set_hint(y, |i| std::prelude::Query::Hint(std::prover::eval(x)));
        {
            let z;
            std::prelude::set_hint(z, query |_| std::prelude::Query::Hint(1));
        };
    "#;
    let expected = r#"namespace std::prover;
    let eval = 8;
    enum Query {
        Hint(fe),
        None,
    }
namespace N(16);
    col witness x;
    col witness y;
    std::prelude::set_hint(N::y, (query |i| std::prelude::Query::Hint(std::prover::eval(N::x))));
    col witness z;
    std::prelude::set_hint(N::z, (query |_| std::prelude::Query::Hint(1)));
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Expected type: int -> std::prelude::Query"]
fn set_hint_invalid_function() {
    let input = r#"
    namespace std::prover;
        let eval = 8;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        std::prelude::set_hint(x, query |_, _| std::prelude::Query::Hint(1));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Array elements are not supported for std::prelude::set_hint (called on N::x[0])."]
fn set_hint_array_element() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x: col[2];
        std::prelude::set_hint(x[0], query |_| std::prelude::Query::Hint(1));
    "#;
    let expected = r#"namespace std::prover;
    let set_hint = 8;
    let eval = 8;
    enum Query {
        Hint(fe),
        None,
    }
namespace N(16);
    col witness x(_) query std::prelude::Query::Hint(1);
    col witness y(i) query std::prelude::Query::Hint(std::prover::eval(N::x));
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
#[should_panic = "Expected reference to witness column as first argument for std::prelude::set_hint, but got intermediate column N::y."]
fn set_hint_no_col() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        let y: inter = x;
        std::prelude::set_hint(y, query |_| std::prelude::Query::Hint(1));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Column N::x already has a hint set, but tried to add another one."]
fn set_hint_twice() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        std::prelude::set_hint(x, query |_| std::prelude::Query::Hint(1));
        std::prelude::set_hint(x, query |_| std::prelude::Query::Hint(2));
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
#[should_panic = "Column N::x already has a hint set, but tried to add another one."]
fn set_hint_twice_in_constr() {
    let input = r#"
    namespace std::prover;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let y;
        {
            let x;
            std::prelude::set_hint(x, query |_| std::prelude::Query::Hint(1));
            std::prelude::set_hint(x, query |_| std::prelude::Query::Hint(2));
        };
    "#;
    analyze_string::<GoldilocksField>(input);
}

#[test]
fn set_hint_outside() {
    let input = r#"
    namespace std::prover;
        let eval = 8;
        enum Query { Hint(fe), None, }
    namespace N(16);
        let x;
        let y;
        let create_wit = constr || { let w; w };
        let z = create_wit();
        let set_hint = constr |c| { std::prelude::set_hint(c, query |_| std::prelude::Query::Hint(8)); };
        set_hint(x);
        set_hint(y);
        (|| { set_hint(z); })();
    "#;
    let expected = r#"namespace std::prover;
    let eval = 8;
    enum Query {
        Hint(fe),
        None,
    }
namespace N(16);
    col witness x;
    std::prelude::set_hint(N::x, (query |_| std::prelude::Query::Hint(8)));
    col witness y;
    std::prelude::set_hint(N::y, (query |_| std::prelude::Query::Hint(8)));
    let create_wit: -> expr = (constr || {
        let w: col;
        w
    });
    let z: expr = N::create_wit();
    let set_hint: expr -> () = (constr |c| {
        std::prelude::set_hint(c, (query |_| std::prelude::Query::Hint(8)));

    });
    col witness w;
    std::prelude::set_hint(N::w, (query |_| std::prelude::Query::Hint(8)));
"#;
    let formatted = analyze_string::<GoldilocksField>(input).to_string();
    assert_eq!(formatted, expected);
}

#[test]
fn intermediate_syntax() {
    let input = r#"namespace N(65536);
    col witness x[5];
    let inter: inter = x[2];
    let inter_arr: inter[5] = x;
"#;
    let analyzed = analyze_string::<GoldilocksField>(input);
    assert_eq!(analyzed.intermediate_count(), 6);
    let expected = r#"namespace N(65536);
    col witness x[5];
    col inter = N::x[2];
    col inter_arr[5] = [N::x[0], N::x[1], N::x[2], N::x[3], N::x[4]];
"#;
    assert_eq!(analyzed.to_string(), expected);
}

#[test]
fn intermediate_dynamic() {
    let input = r#"namespace N(65536);
    col witness x[5];
    {
        let inte: inter = x[2];
        let inter_arr: inter[5] = x;
        inte = 8;
        inter_arr[3] = 9;
    };
"#;
    let analyzed = analyze_string::<GoldilocksField>(input);
    assert_eq!(analyzed.intermediate_count(), 6);
    let expected = r#"namespace N(65536);
    col witness x[5];
    col inte = N::x[2];
    col inter_arr[5] = [N::x[0], N::x[1], N::x[2], N::x[3], N::x[4]];
    N::inte = 8;
    N::inter_arr[3] = 9;
"#;
    assert_eq!(analyzed.to_string(), expected);
}

#[test]
fn intermediate_arr_no_length() {
    let input = r#"namespace N(65536);
    col witness x[5];
    {
        let inte: inter[] = x;
    };
"#;
    let analyzed = analyze_string::<GoldilocksField>(input);
    assert_eq!(analyzed.intermediate_count(), 5);
    let expected = r#"namespace N(65536);
    col witness x[5];
    col inte[5] = [N::x[0], N::x[1], N::x[2], N::x[3], N::x[4]];
"#;
    assert_eq!(analyzed.to_string(), expected);
}

#[test]
#[should_panic = "Error creating intermediate column array N::inte: Expected array of length 6 as value but it has 2 elements."]
fn intermediate_arr_wrong_length() {
    let input = r#"namespace N(65536);
    col witness x[2];
    {
        let inte: inter[6] = x;
    };
"#;
    analyze_string::<GoldilocksField>(input);
}
