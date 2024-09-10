/// Makes a constraint conditional on a condition, i.e. it is only active if tthe condition is non-zero.
/// We assume that the condition is either zero or on.
/// For lookups and permutations, only the left-hand-side is made conditional.
let make_conditional: Constr, expr -> Constr = |constraint, condition| match constraint {
    Constr::Identity(l, r) => condition * (l - r) = 0,
    Constr::Lookup((Option::None, sel_r), exprs) => Constr::Lookup((Option::Some(condition), sel_r), exprs),
    Constr::Lookup((Option::Some(sel_l), sel_r), exprs) => Constr::Lookup((Option::Some(sel_l * condition), sel_r), exprs),
    Constr::Permutation((Option::None, sel_r), exprs) => Constr::Permutation((Option::Some(condition), sel_r), exprs),
    Constr::Permutation((Option::Some(sel_l), sel_r), exprs) => Constr::Permutation((Option::Some(sel_l * condition), sel_r), exprs),
    Constr::Connection(_) => std::check::panic("Connection constraints cannot be conditional"),
};
