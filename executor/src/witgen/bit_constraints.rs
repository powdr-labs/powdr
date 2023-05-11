use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use crate::witgen::util::contains_next_ref;
use number::{BigInt, FieldElement, FieldElementTrait};
use pil_analyzer::{BinaryOperator, Expression, Identity, IdentityKind, PolynomialReference};

use super::expression_evaluator::ExpressionEvaluator;
use super::symbolic_evaluator::SymbolicEvaluator;
use super::util::try_to_simple_poly;
use super::{Constraint, FixedData};

/// Constraint on the bit values of a variable X.
/// All bits smaller than min_bit have to be zero
/// and all bits larger than max_bit have to be zero.
/// The least significant bit is bit zero.
#[derive(PartialEq, Eq, Clone)]
pub struct BitConstraint {
    mask: <FieldElement as FieldElementTrait>::Integer,
}

impl BitConstraint {
    pub fn from_max_bit(max_bit: u64) -> Self {
        assert!(max_bit < 1024);
        BitConstraint {
            mask: <FieldElement as FieldElementTrait>::Integer::from(
                ((1 << (max_bit + 1)) - 1) as u32,
            ),
        }
    }

    pub fn from_mask<M: Into<<FieldElement as FieldElementTrait>::Integer>>(mask: M) -> Self {
        BitConstraint { mask: mask.into() }
    }

    /// The bit constraint of the sum of two expressions.
    pub fn try_combine_sum(&self, other: &BitConstraint) -> Option<BitConstraint> {
        if self.mask & other.mask == 0u32.into() {
            Some(BitConstraint {
                mask: self.mask | other.mask,
            })
        } else {
            None
        }
    }

    /// Returns the conjunction of this constraint and the other.
    pub fn conjunction(self, other: &BitConstraint) -> BitConstraint {
        BitConstraint {
            mask: self.mask & other.mask,
        }
    }

    /// The bit constraint of an integer multiple of an expression.
    /// TODO this assumes goldilocks
    pub fn multiple(&self, factor: FieldElement) -> Option<BitConstraint> {
        if factor.to_arbitrary_integer() * self.mask.to_arbitrary_integer()
            >= FieldElement::modulus().to_arbitrary_integer()
        {
            None
        } else {
            // TODO use binary logarithm
            (0..64).find_map(|i| {
                if factor == (1u64 << i).into() {
                    Some(BitConstraint {
                        mask: self.mask << i,
                    })
                } else {
                    None
                }
            })
        }
    }

    pub fn mask(&self) -> &<FieldElement as FieldElementTrait>::Integer {
        &self.mask
    }
}

impl Display for BitConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.mask().to_arbitrary_integer())
    }
}

impl core::fmt::Debug for BitConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BitConstraint")
            .field("mask", &format!("0x{:x}", self.mask))
            .finish()
    }
}

/// Trait that provides a bit constraint on a symbolic variable if given by ID.
pub trait BitConstraintSet<K> {
    fn bit_constraint(&self, id: K) -> Option<BitConstraint>;
}

pub struct SimpleBitConstraintSet<'a> {
    bit_constraints: &'a BTreeMap<&'a PolynomialReference, BitConstraint>,
}

impl<'a> BitConstraintSet<&PolynomialReference> for SimpleBitConstraintSet<'a> {
    fn bit_constraint(&self, id: &PolynomialReference) -> Option<BitConstraint> {
        assert!(!id.next);
        self.bit_constraints.get(id).cloned()
    }
}

/// Determines global constraints on witness and fixed columns.
/// Removes identities that only serve to create bit constraints from
/// the identities vector.
/// TODO at some point, we should check that they still hold.
pub fn determine_global_constraints<'a>(
    fixed_data: &'a FixedData,
    identities: Vec<&'a Identity>,
) -> (
    BTreeMap<&'a PolynomialReference, BitConstraint>,
    Vec<&'a Identity>,
) {
    let mut known_constraints = BTreeMap::new();
    // For these columns, we know that they are not only constrained to those bits
    // but also have one row for each possible value.
    // It allows us to completely remove some lookups.
    let mut full_span = BTreeSet::new();
    for (&poly, &values) in fixed_data
        .fixed_cols
        .iter()
        .zip(fixed_data.fixed_col_values.iter())
    {
        if let Some((cons, full)) = process_fixed_column(values) {
            assert!(known_constraints.insert(poly, cons).is_none());
            if full {
                full_span.insert(poly);
            }
        }
    }

    let mut retained_identities = vec![];
    let mut removed_identities = vec![];
    for identity in identities {
        let remove;
        (known_constraints, remove) =
            propagate_constraints(known_constraints, identity, &full_span);
        (if remove {
            &mut removed_identities
        } else {
            &mut retained_identities
        })
        .push(identity);
    }

    log::debug!("Determined the following global bit constraints:");
    for (name, con) in &known_constraints {
        log::debug!("  {name}: {con}");
    }
    log::debug!("Determined the following identities to be purely bit/range constraints:");
    for id in removed_identities {
        log::debug!("  {id}");
    }

    (known_constraints, retained_identities)
}

/// Analyzes a fixed column and checks if its values correspond exactly
/// to a certain bit pattern.
/// TODO do this on the symbolic definition instead of the values.
fn process_fixed_column(fixed: &[FieldElement]) -> Option<(BitConstraint, bool)> {
    if let Some(bit) = smallest_period_candidate(fixed) {
        let mask = <FieldElement as FieldElementTrait>::Integer::from(((1 << bit) - 1) as u32);
        if fixed.iter().enumerate().all(|(i, v)| {
            v.to_integer() == <FieldElement as FieldElementTrait>::Integer::from(i as u32) & mask
        }) {
            return Some((BitConstraint::from_mask(mask), true));
        }
    }
    let mut mask = <FieldElement as FieldElementTrait>::Integer::from(0u32);
    for v in fixed.iter() {
        mask |= v.to_integer();
    }

    Some((BitConstraint::from_mask(mask), false))
}

/// Deduces new bit constraints on witness columns from constraints on fixed columns
/// and identities. Note that these constraints hold globally, i.e. for all rows.
/// If the returned flag is true, the identity can be removed, because it contains
/// no further information than the bit constraint.
fn propagate_constraints<'a>(
    mut known_constraints: BTreeMap<&'a PolynomialReference, BitConstraint>,
    identity: &'a Identity,
    full_span: &BTreeSet<&'a PolynomialReference>,
) -> (BTreeMap<&'a PolynomialReference, BitConstraint>, bool) {
    let mut remove = false;
    match identity.kind {
        IdentityKind::Polynomial => {
            if let Some(p) = is_binary_constraint(identity.left.selector.as_ref().unwrap()) {
                assert!(known_constraints
                    .insert(p, BitConstraint::from_max_bit(0))
                    .is_none());
                remove = true;
            } else if let Some((p, c)) = try_transfer_constraints(
                identity.left.selector.as_ref().unwrap(),
                &known_constraints,
            ) {
                known_constraints
                    .entry(p)
                    .and_modify(|existing| *existing = existing.clone().conjunction(&c))
                    .or_insert(c);
            }
        }
        IdentityKind::Plookup | IdentityKind::Permutation | IdentityKind::Connect => {
            if identity.left.selector.is_some() || identity.right.selector.is_some() {
                return (known_constraints, false);
            }
            for (left, right) in identity
                .left
                .expressions
                .iter()
                .zip(identity.right.expressions.iter())
            {
                if let (Some(left), Some(right)) =
                    (try_to_simple_poly(left), try_to_simple_poly(right))
                {
                    if let Some(constraint) = known_constraints.get(right).cloned() {
                        known_constraints
                            .entry(left)
                            .and_modify(|existing| {
                                *existing = existing.clone().conjunction(&constraint)
                            })
                            .or_insert(constraint);
                    }
                }
            }
            if identity.kind == IdentityKind::Plookup && identity.right.expressions.len() == 1 {
                // We can only remove the lookup if the RHS is a fixed polynomial that
                // provides all values in the span.
                if let Some(name) = try_to_simple_poly(&identity.right.expressions[0]) {
                    if full_span.contains(name) {
                        remove = true;
                    }
                }
            }
        }
    }

    (known_constraints, remove)
}

/// Tries to find "X * (1 - X) = 0"
fn is_binary_constraint(expr: &Expression) -> Option<&PolynomialReference> {
    // TODO Write a proper pattern matching engine.
    if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expr {
        if let Expression::Number(n) = right.as_ref() {
            if *n == 0.into() {
                return is_binary_constraint(left.as_ref());
            }
        }
    } else if let Expression::BinaryOperation(left, BinaryOperator::Mul, right) = expr {
        let symbolic_ev = SymbolicEvaluator;
        let left_root = ExpressionEvaluator::new(symbolic_ev.clone())
            .evaluate(left)
            .ok()
            .and_then(|l| l.solve().ok())?;
        let right_root = ExpressionEvaluator::new(symbolic_ev)
            .evaluate(right)
            .ok()
            .and_then(|r| r.solve().ok())?;
        if let ([(id1, Constraint::Assignment(value1))], [(id2, Constraint::Assignment(value2))]) =
            (&left_root.constraints[..], &right_root.constraints[..])
        {
            if id1 != id2 || !id2.is_witness() {
                return None;
            }
            if (*value1 == 0.into() && *value2 == 1.into())
                || (*value1 == 1.into() && *value2 == 0.into())
            {
                return Some(id1);
            }
        }
    }
    None
}

/// Tries to transfer constraints in a linear expression.
fn try_transfer_constraints<'a, 'b>(
    expr: &'a Expression,
    known_constraints: &'b BTreeMap<&'b PolynomialReference, BitConstraint>,
) -> Option<(&'a PolynomialReference, BitConstraint)> {
    if contains_next_ref(expr) {
        return None;
    }

    let symbolic_ev = SymbolicEvaluator;
    let aff_expr = ExpressionEvaluator::new(symbolic_ev).evaluate(expr).ok()?;

    let bit_constraints = SimpleBitConstraintSet {
        bit_constraints: known_constraints,
    };
    let result = aff_expr.solve_with_bit_constraints(&bit_constraints).ok()?;
    assert!(result.constraints.len() <= 1);
    result.constraints.get(0).cloned().and_then(|(poly, cons)| {
        if let Constraint::BitConstraint(cons) = cons {
            assert!(!poly.next);
            Some((poly, cons))
        } else {
            None
        }
    })
}

fn smallest_period_candidate(fixed: &[FieldElement]) -> Option<u64> {
    if fixed.first() != Some(&0.into()) {
        return None;
    }
    (1..63).find(|bit| fixed.get(1usize << bit) == Some(&0.into()))
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use pil_analyzer::{PolyID, PolynomialReference};

    use crate::witgen::bit_constraints::{propagate_constraints, BitConstraint};

    use super::process_fixed_column;

    #[test]
    fn all_zeros() {
        let fixed = [0, 0, 0, 0].iter().map(|v| (*v).into()).collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column(&fixed),
            Some((BitConstraint::from_mask(0_u32), false))
        );
    }

    #[test]
    fn zero_one() {
        let fixed = [0, 1, 0, 1, 0]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column(&fixed),
            Some((BitConstraint::from_mask(1_u32), true))
        );
    }

    #[test]
    fn zero_one_two_three() {
        let fixed = [0, 1, 2, 3, 0]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column(&fixed),
            Some((BitConstraint::from_mask(3_u32), true))
        );
    }

    #[test]
    fn various_with_bit_mask() {
        let fixed = [0, 6, 0x0100, 0x1100, 2]
            .iter()
            .map(|v| (*v).into())
            .collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column(&fixed),
            Some((BitConstraint::from_mask(0x1106_u32), false))
        );
    }

    fn convert_constraints<'a>(
        (poly, constr): (&&'a PolynomialReference, &BitConstraint),
    ) -> (&'a str, BitConstraint) {
        (poly.name.as_str(), constr.clone())
    }

    #[test]
    fn test_propagate_constraints() {
        let pil_source = r"
namespace Global(2**20);
    col fixed BYTE(i) { i & 0xff };
    col fixed BYTE2(i) { i & 0xffff };
    col fixed SHIFTED(i) { i & 0xff0 };
    col witness A;
    // A bit more complicated to see that the 'pattern matcher' works properly.
    (1 - A + 0) * (A + 1 - 1) = 0;
    col witness B;
    { B } in { BYTE };
    col witness C;
    C = A * 512 + B;
    col witness D;
    { D } in { BYTE };
    { D } in { SHIFTED };
";
        let analyzed = pil_analyzer::analyze_string(pil_source);
        let (constants, _) = crate::constant_evaluator::generate(&analyzed);
        let fixed_polys = constants
            .iter()
            .enumerate()
            .map(|(i, (name, _))| PolynomialReference {
                name: name.to_string(),
                poly_id: Some(PolyID {
                    id: i as u64,
                    ptype: pil_analyzer::PolynomialType::Constant,
                }),
                index: None,
                next: false,
            })
            .collect::<Vec<_>>();
        let mut known_constraints = fixed_polys
            .iter()
            .zip(&constants)
            .filter_map(|(poly, (_, values))| {
                process_fixed_column(values).map(|(constraint, _full)| (poly, constraint))
            })
            .collect::<BTreeMap<_, _>>();
        assert_eq!(
            known_constraints
                .iter()
                .map(convert_constraints)
                .collect::<BTreeMap<_, _>>(),
            vec![
                ("Global.BYTE", BitConstraint::from_max_bit(7)),
                ("Global.BYTE2", BitConstraint::from_max_bit(15)),
                ("Global.SHIFTED", BitConstraint::from_mask(0xff0_u32)),
            ]
            .into_iter()
            .collect::<BTreeMap<_, _>>()
        );
        for identity in &analyzed.identities {
            (known_constraints, _) =
                propagate_constraints(known_constraints, identity, &Default::default());
        }
        assert_eq!(
            known_constraints
                .iter()
                .map(convert_constraints)
                .collect::<BTreeMap<_, _>>(),
            vec![
                ("Global.A", BitConstraint::from_max_bit(0)),
                ("Global.B", BitConstraint::from_max_bit(7)),
                ("Global.C", BitConstraint::from_mask(0x2ff_u32)),
                ("Global.D", BitConstraint::from_mask(0xf0_u32)),
                ("Global.BYTE", BitConstraint::from_max_bit(7)),
                ("Global.BYTE2", BitConstraint::from_max_bit(15)),
                ("Global.SHIFTED", BitConstraint::from_mask(0xff0_u32)),
            ]
            .into_iter()
            .collect::<BTreeMap<_, _>>()
        );
    }

    #[test]
    fn combinations() {
        let a = BitConstraint::from_max_bit(7);
        assert_eq!(a, BitConstraint::from_mask(0xff_u32));
        let b = a.multiple(256.into()).unwrap();
        assert_eq!(b, BitConstraint::from_mask(0xff00_u32));
        assert_eq!(
            b.try_combine_sum(&a).unwrap(),
            BitConstraint::from_mask(0xffff_u32)
        );
    }

    #[test]
    fn weird_combinations() {
        let a = BitConstraint::from_mask(0xf00f_u32);
        let b = a.multiple(256.into()).unwrap();
        assert_eq!(b, BitConstraint::from_mask(0xf00f00_u32));
        assert_eq!(
            b.try_combine_sum(&a).unwrap(),
            BitConstraint::from_mask(0xf0ff0f_u32)
        );
    }
}
