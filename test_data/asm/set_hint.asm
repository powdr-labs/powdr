let new_col_with_hint: -> expr = constr || {
    let x;
    std::prelude::set_hint(x, |_| std::prelude::Query::Hint(2));
    x
};

machine Main with degree: 4 {
    let x;
    let w = new_col_with_hint();
    x = w + 1;
}