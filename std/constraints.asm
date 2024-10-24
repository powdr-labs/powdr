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

/// Converts a lookup constraint to a phantom lookup constraint.
let to_phantom_lookup: Constr, expr -> Constr = |constraint, multiplicities| match constraint {
    Constr::Lookup(selectors, exprs) => Constr::PhantomLookup(selectors, exprs, multiplicities),
    _ => std::check::panic("Expected a lookup constraint."),
};

/// Converts a permutation constraint to a phantom permutation constraint.
let to_phantom_permutation: Constr -> Constr = |constraint| match constraint {
    Constr::Permutation(selectors, exprs) => Constr::PhantomPermutation(selectors, exprs),
    _ => std::check::panic("Expected a permutation constraint."),
};