/// Makes a constraint conditional on a condition, i.e. it is only active if tthe condition is non-zero.
/// We assume that the condition is either zero or on.
/// For lookups and permutations, only the left-hand-side is made conditional.
let make_conditional: std::prelude::Constr, expr = |constr, condition| match constr {
    Constr::Identity(l, r) => condition * l = condition * r,
    Constr::Lookup((None, sel_r), exprs) => Constr::Lookup((Some(condition), sel_r), exprs),
    Constr::Lookup((Some(sel_l), sel_r), exprs) => Constr::Lookup((Some(sel_l * condition), sel_r), exprs),
    Constr::Permutation((None, sel_r), exprs) => Permutation::Lookup((Some(condition), sel_r), exprs),
    Constr::Permutation((Some(sel_l), sel_r), exprs) => Permutation::Lookup((Some(sel_l * condition), sel_r), exprs),
    Constr::Connection(_) => std::check::panic("Connection constraints cannot be conditional"),
};
