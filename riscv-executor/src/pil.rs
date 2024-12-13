use std::ops::ControlFlow;

use powdr_ast::{
    analyzed::{AlgebraicExpression, Analyzed, Identity, PermutationIdentity},
    parsed::visitor::ExpressionVisitable,
};
use powdr_number::FieldElement;

pub fn links_from_pil<F: FieldElement>(pil: &Analyzed<F>) -> Vec<Identity<F>> {
    pil.identities
        .iter()
        .filter(|id| matches!(id, Identity::Permutation(_) | Identity::Lookup(_)))
        .cloned()
        .collect()
}

fn extract_selector<F: FieldElement>(permutation: &PermutationIdentity<F>) -> String {
    match permutation
        .right
        .selector
        .pre_visit_expressions_return(&mut |e| {
            if let AlgebraicExpression::Reference(r) = e {
                // this makes the assumption that selector names always start
                // with "sel" and there is no other array with a name starting
                // with "sel" defined in the machine/namespace
                if r.name.contains("::sel") && r.name.contains("[") {
                    return ControlFlow::Break(r.name.clone());
                }
            }
            ControlFlow::Continue(())
        }) {
        ControlFlow::Break(s) => s,
        ControlFlow::Continue(_) => panic!("couldn't find selector"),
    }
}

pub fn selector_for_link<F: FieldElement>(links: &[Identity<F>], id: u64) -> Option<String> {
    for l in links.iter() {
        if let Identity::Permutation(perm) = l {
            if perm.id == id {
                return Some(extract_selector(perm));
            }
        }
        if let Identity::Lookup(lookup) = l {
            if lookup.id == id {
                return None;
            }
        }
    }
    panic!("identity {id} doesnt exist")
}

/// Find links referencing columns containing the string `from` on the LHS and `to` on the RHS.
/// For permutations, only looks at the identity selectors.
/// For lookups, looks at the left selector and the right expression.
pub fn find_links<F: FieldElement>(
    links: &[Identity<F>],
    from: &str,
    to: &str,
) -> Vec<Identity<F>> {
    links
        .iter()
        .filter(|id| match id {
            Identity::Permutation(id) => {
                let left = id.left.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(from);
                    }
                    false
                });
                let right = id.right.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(to);
                    }
                    false
                });
                left && right
            }
            Identity::Lookup(id) => {
                let left = id.left.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(from);
                    }
                    false
                });
                let right = id.right.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(to);
                    }
                    false
                });
                left && right
            }
            _ => false,
        })
        .cloned()
        .collect()
}

/// Find links referencing the exact `instruction_flag` on the LHS.
/// On the RHS it looks for columns containing the string `to`.
pub fn find_instruction_links<F: FieldElement>(
    links: &[Identity<F>],
    instruction_flag: &str,
    to: &str,
) -> Vec<Identity<F>> {
    links
        .iter()
        .filter(|id| match id {
            Identity::Permutation(id) => {
                let left = id.left.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name == instruction_flag;
                    }
                    false
                });
                let right = id.right.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(to);
                    }
                    false
                });
                left && right
            }
            Identity::Lookup(id) => {
                let left = id.left.selector.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name == instruction_flag;
                    }
                    false
                });
                let right = id.right.expr_any(|e| {
                    if let AlgebraicExpression::Reference(r) = e {
                        return r.name.contains(to);
                    }
                    false
                });
                left && right
            }
            _ => false,
        })
        .cloned()
        .collect()
}
