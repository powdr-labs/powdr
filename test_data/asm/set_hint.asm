let new_col_with_hint: -> expr = constr || {
    let x;
    std::prover::set_hint(x, |_| std::prover::Query::Hint(2));
    x
};

machine Main with degree: 4 {
    let x;
    let w = new_col_with_hint();
    x = w;
}