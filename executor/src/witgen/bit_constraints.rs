use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};

use crate::analyzer::{BinaryOperator, Expression, Identity, IdentityKind};
use crate::witgen::util::{contains_next_ref, WitnessColumnNamer};
use number::{AbstractNumberType, FieldElement};

use super::expression_evaluator::ExpressionEvaluator;
use super::symbolic_evaluator::SymbolicEvaluator;
use super::util::is_simple_poly;
use super::{Constraint, FixedData};

/// Constraint on the bit values of a variable X.
/// All bits smaller than min_bit have to be zero
/// and all bits larger than max_bit have to be zero.
/// The least significant bit is bit zero.
#[derive(PartialEq, Clone)]
pub struct BitConstraint {
    mask: AbstractNumberType,
}

impl BitConstraint {
    pub fn from_max_bit(max_bit: u64) -> Self {
        assert!(max_bit < 1024);
        BitConstraint {
            mask: (1 << (max_bit + 1)) - 1,
        }
    }

    pub fn from_mask(mask: AbstractNumberType) -> Self {
        BitConstraint { mask }
    }

    /// The bit constraint of the sum of two expressions.
    pub fn try_combine_sum(&self, other: &BitConstraint) -> Option<BitConstraint> {
        if self.mask & other.mask == 0 {
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
        if factor.to_integer() * self.mask >= FieldElement::modulus() {
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

    pub fn mask(&self) -> AbstractNumberType {
        self.mask
    }
}

impl Display for BitConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.mask())
    }
}

impl core::fmt::Debug for BitConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BitConstraint")
            .field("mask", &format!("0x{:x}", &self.mask))
            .finish()
    }
}

/// Trait that provides a bit constraint on a symbolic variable if given by ID.
pub trait BitConstraintSet {
    fn bit_constraint(&self, id: usize) -> Option<BitConstraint>;
}

pub struct SimpleBitConstraintSet<'a, Namer: WitnessColumnNamer> {
    bit_constraints: &'a BTreeMap<&'a str, BitConstraint>,
    names: &'a Namer,
}

impl<'a, Namer: WitnessColumnNamer> BitConstraintSet for SimpleBitConstraintSet<'a, Namer> {
    fn bit_constraint(&self, id: usize) -> Option<BitConstraint> {
        self.bit_constraints
            .get(self.names.name(id).as_str())
            .cloned()
    }
}

/// Determines global constraints on witness and fixed columns.
/// Removes identities that only serve to create bit constraints from
/// the identities vector.
/// TODO at some point, we should check that they still hold.
pub fn determine_global_constraints<'a>(
    fixed_data: &'a FixedData,
    identities: Vec<&'a Identity>,
) -> (BTreeMap<&'a str, BitConstraint>, Vec<&'a Identity>) {
    let mut known_constraints = BTreeMap::new();
    // For these columns, we know that they are not only constrained to those bits
    // but also have one row for each possible value.
    // It allows us to completely remove some lookups.
    let mut full_span = BTreeSet::new();
    for (&name, &values) in &fixed_data.fixed_cols {
        if let Some((cons, full)) = process_fixed_column(values) {
            assert!(known_constraints.insert(name, cons).is_none());
            if full {
                full_span.insert(name);
            }
        }
    }

    let mut retained_identities = vec![];
    let mut removed_identities = vec![];
    for identity in identities {
        let remove;
        (known_constraints, remove) =
            propagate_constraints(fixed_data, known_constraints, identity, &full_span);
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
        let mask: AbstractNumberType = (1 << bit) - 1;
        if fixed
            .iter()
            .enumerate()
            .all(|(i, v)| v.to_integer() == i as AbstractNumberType & mask)
        {
            return Some((BitConstraint::from_mask(mask), true));
        }
    }
    let mut mask = 0;
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
    fixed_data: &'a FixedData,
    mut known_constraints: BTreeMap<&'a str, BitConstraint>,
    identity: &'a Identity,
    full_span: &BTreeSet<&'a str>,
) -> (BTreeMap<&'a str, BitConstraint>, bool) {
    let mut remove = false;
    match identity.kind {
        IdentityKind::Polynomial => {
            if let Some(p) =
                is_binary_constraint(fixed_data, identity.left.selector.as_ref().unwrap())
            {
                assert!(known_constraints
                    .insert(p, BitConstraint::from_max_bit(0))
                    .is_none());
                remove = true;
            } else if let Some((p, c)) = try_transfer_constraints(
                fixed_data,
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
                if let (Some(left), Some(right)) = (is_simple_poly(left), is_simple_poly(right)) {
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
                if let Some(name) = is_simple_poly(&identity.right.expressions[0]) {
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
fn is_binary_constraint<'a>(fixed_data: &'a FixedData, expr: &Expression) -> Option<&'a str> {
    // TODO Write a proper pattern matching engine.
    if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expr {
        if let Expression::Number(n) = right.as_ref() {
            if *n == 0.into() {
                return is_binary_constraint(fixed_data, left.as_ref());
            }
        }
    } else if let Expression::BinaryOperation(left, BinaryOperator::Mul, right) = expr {
        let symbolic_ev = SymbolicEvaluator::new(fixed_data);
        let left_root = ExpressionEvaluator::new(symbolic_ev.clone())
            .evaluate(left)
            .ok()
            .and_then(|l| l.solve().ok())?;
        let right_root = ExpressionEvaluator::new(symbolic_ev.clone())
            .evaluate(right)
            .ok()
            .and_then(|r| r.solve().ok())?;
        if let ([(id1, Constraint::Assignment(value1))], [(id2, Constraint::Assignment(value2))]) =
            (&left_root[..], &right_root[..])
        {
            let poly1 = symbolic_ev.poly_from_id(*id1);
            let poly2 = symbolic_ev.poly_from_id(*id2);
            if poly1 != poly2 || !fixed_data.witness_ids.contains_key(poly1.0) {
                return None;
            }
            if (*value1 == 0.into() && *value2 == 1.into())
                || (*value1 == 1.into() && *value2 == 0.into())
            {
                return Some(poly1.0);
            }
        }
    }
    None
}

/// Tries to transfer constraints in a linear expression.
fn try_transfer_constraints<'a>(
    fixed_data: &'a FixedData,
    expr: &'a Expression,
    known_constraints: &BTreeMap<&str, BitConstraint>,
) -> Option<(&'a str, BitConstraint)> {
    if contains_next_ref(expr) {
        return None;
    }

    let symbolic_ev = SymbolicEvaluator::new(fixed_data);
    let aff_expr = ExpressionEvaluator::new(symbolic_ev.clone())
        .evaluate(expr)
        .ok()?;

    let result = aff_expr
        .solve_with_bit_constraints(&SimpleBitConstraintSet {
            bit_constraints: known_constraints,
            names: &symbolic_ev,
        })
        .ok()?;
    assert!(result.len() <= 1);
    result.get(0).and_then(|(id, cons)| {
        if let Constraint::BitConstraint(cons) = cons {
            let (poly, next) = symbolic_ev.poly_from_id(*id);
            assert!(!next);
            Some((poly, cons.clone()))
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

    use crate::witgen::bit_constraints::{propagate_constraints, BitConstraint};
    use crate::witgen::{FixedData, WitnessColumn};

    use super::process_fixed_column;

    #[test]
    fn all_zeros() {
        let fixed = [0, 0, 0, 0].iter().map(|v| (*v).into()).collect::<Vec<_>>();
        assert_eq!(
            process_fixed_column(&fixed),
            Some((BitConstraint::from_mask(0), false))
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
            Some((BitConstraint::from_mask(1), true))
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
            Some((BitConstraint::from_mask(3), true))
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
            Some((BitConstraint::from_mask(0x1106), false))
        );
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
        let analyzed = crate::analyzer::analyze_string(pil_source);
        let (constants, degree) = crate::constant_evaluator::generate(&analyzed);
        let mut known_constraints = constants
            .iter()
            .filter_map(|(name, values)| {
                process_fixed_column(values).map(|(constraint, _full)| (*name, constraint))
            })
            .collect::<BTreeMap<_, _>>();
        assert_eq!(
            known_constraints,
            vec![
                ("Global.BYTE", BitConstraint::from_max_bit(7)),
                ("Global.BYTE2", BitConstraint::from_max_bit(15)),
                ("Global.SHIFTED", BitConstraint::from_mask(0xff0)),
            ]
            .into_iter()
            .collect()
        );
        // TODO write some test code to generate FixedData directly from `analyzed`
        let witness_cols: Vec<WitnessColumn> = analyzed
            .committed_polys_in_source_order()
            .iter()
            .enumerate()
            .map(|(i, (poly, value))| {
                if poly.length.is_some() {
                    unimplemented!("Committed arrays not implemented.")
                }
                WitnessColumn::new(i, &poly.absolute_name, value)
            })
            .collect();
        let fixed_data = FixedData::new(
            degree,
            &analyzed.constants,
            constants.iter().map(|(n, v)| (*n, v)).collect(),
            &witness_cols,
            witness_cols.iter().map(|w| (w.name, w.id)).collect(),
        );
        for identity in &analyzed.identities {
            (known_constraints, _) = propagate_constraints(
                &fixed_data,
                known_constraints,
                identity,
                &Default::default(),
            );
        }
        assert_eq!(
            known_constraints,
            vec![
                ("Global.A", BitConstraint::from_max_bit(0)),
                ("Global.B", BitConstraint::from_max_bit(7)),
                ("Global.C", BitConstraint::from_mask(0x2ff)),
                ("Global.D", BitConstraint::from_mask(0xf0)),
                ("Global.BYTE", BitConstraint::from_max_bit(7)),
                ("Global.BYTE2", BitConstraint::from_max_bit(15)),
                ("Global.SHIFTED", BitConstraint::from_mask(0xff0)),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn combinations() {
        let a = BitConstraint::from_max_bit(7);
        assert_eq!(a, BitConstraint::from_mask(0xff));
        let b = a.multiple(256.into()).unwrap();
        assert_eq!(b, BitConstraint::from_mask(0xff00));
        assert_eq!(
            b.try_combine_sum(&a).unwrap(),
            BitConstraint::from_mask(0xffff)
        );
    }

    #[test]
    fn weird_combinations() {
        let a = BitConstraint::from_mask(0xf00f);
        let b = a.multiple(256.into()).unwrap();
        assert_eq!(b, BitConstraint::from_mask(0xf00f00));
        assert_eq!(
            b.try_combine_sum(&a).unwrap(),
            BitConstraint::from_mask(0xf0ff0f)
        );
    }
}
