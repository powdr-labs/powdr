/// Makes a constraint conditional on a condition, i.e. it is only active if the condition is non-zero.
/// We assume that the condition is either zero or one.
/// For lookups and permutations, only the left-hand-side is made conditional.
let make_conditional: Constr, expr -> Constr = |constraint, condition| match constraint {
    Constr::Identity(l, r) => condition * (l - r) = 0,
    Constr::Lookup((Option::None, sel_r), exprs) => Constr::Lookup((Option::Some(condition), sel_r), exprs),
    Constr::Lookup((Option::Some(sel_l), sel_r), exprs) => Constr::Lookup((Option::Some(sel_l * condition), sel_r), exprs),
    Constr::Permutation((Option::None, sel_r), exprs) => Constr::Permutation((Option::Some(condition), sel_r), exprs),
    Constr::Permutation((Option::Some(sel_l), sel_r), exprs) => Constr::Permutation((Option::Some(sel_l * condition), sel_r), exprs),
    Constr::Connection(_) => std::check::panic("Connection constraints cannot be conditional"),
};

/// Either one constraint or the other, depending on a boolean condition.
let if_else: expr, Constr, Constr -> Constr = |condition, if_true, if_false| match (if_true, if_false) {
    (Constr::Identity(l_t, r_t), Constr::Identity(l_f, r_f)) =>
        condition * (l_t - r_t) +
        (1 - condition) * (l_f - r_f) = 0,
    _ => std::check::panic("if_else can only be used with two identity constraints"),
};
