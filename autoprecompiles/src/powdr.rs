use std::collections::BTreeMap;
use std::ops::ControlFlow;

use itertools::Itertools;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    BusInteractionIdentity, PolyID, PolynomialType,
};
use powdr_ast::parsed::asm::SymbolPath;
use powdr_ast::parsed::visitor::AllChildren;
use powdr_ast::parsed::{
    visitor::{ExpressionVisitable, VisitOrder},
    NamespacedPolynomialReference, UnaryOperator,
};
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{BusInteractionKind, SymbolicBusInteraction};

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

// After powdr and lib are adjusted, this function can be renamed and the old substitute removed
/// Substitute all references in the algebraic expression if they are in the provided substitution map.
pub fn substitute_algebraic<T: Clone>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<Column, AlgebraicExpression<T>>,
) {
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(r) = expr {
                if let Some(sub_expr) = sub.get(&Column::from(&*r)) {
                    *expr = sub_expr.clone();
                }
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
}

/// Substitute all references in the algebraic expression with the zero literal.
pub fn make_refs_zero<T: FieldElement>(expr: &mut AlgebraicExpression<T>) {
    let zero = AlgebraicExpression::Number(T::zero());
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { .. }) = expr {
                *expr = zero.clone();
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
}

/// Returns whether the expression is a zero literal.
/// Note that this assumes that the expression is already maximally simplified, so for example `1 - 1` returns false.
pub fn is_zero<T: FieldElement>(expr: &AlgebraicExpression<T>) -> bool {
    match expr {
        AlgebraicExpression::Number(n) => *n == T::zero(),
        _ => false,
    }
}

/// Returns the first match of the pattern `a + b * 2^17` applied to an expression and all its subexpressions.
/// The first match is returned as a tuple `(a, b)`.
/// Panics if no match is found.
pub fn find_byte_decomp<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> (AlgebraicExpression<T>, AlgebraicExpression<T>) {
    let mut result = None;
    expr.visit_expressions(
        &mut |expr| {
            if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left,
                op: AlgebraicBinaryOperator::Add,
                right,
            }) = expr
            {
                if let AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                    left: inner_left,
                    op: AlgebraicBinaryOperator::Mul,
                    right: inner_right,
                }) = &**right
                {
                    if matches!(&**inner_right, AlgebraicExpression::Number(n) if *n == 2u32.pow(17).into())
                        && is_ref(&**left)
                        && is_ref(&**inner_left)
                    {
                        result = Some((left.as_ref().clone(), inner_left.as_ref().clone()));
                        return ControlFlow::Break(());
                    }
                }
            }
            ControlFlow::Continue(())
        },
        VisitOrder::Pre,
    );
    result.expect("No byte decomposition found")
}

/// Returns true iff a given expression features another expression as a subexpression.
pub fn has_ref<T: Clone + std::cmp::PartialEq>(
    expr: &AlgebraicExpression<T>,
    r: &AlgebraicExpression<T>,
) -> bool {
    expr.all_children().any(|e| e == r)
}

/// Returns true iff a given expression is a reference.
pub fn is_ref<T: Clone>(expr: &AlgebraicExpression<T>) -> bool {
    matches!(expr, AlgebraicExpression::Reference(_))
}

/// Substitute all subexpressions in the algebraic expression with the corresponding subexpression in the provided substitution map.
pub fn substitute_algebraic_algebraic<T: Clone + std::cmp::Ord>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<AlgebraicExpression<T>, AlgebraicExpression<T>>,
) {
    expr.visit_expressions_mut(
        &mut |expr| {
            if let Some(sub_expr) = sub.get(expr) {
                *expr = sub_expr.clone();
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, Serialize, Deserialize)]
pub struct Column {
    pub name: String,
    pub id: PolyID,
}

impl From<&AlgebraicReference> for Column {
    fn from(r: &AlgebraicReference) -> Self {
        assert!(!r.next);
        Column {
            name: r.name.clone(),
            id: r.poly_id,
        }
    }
}

pub trait UniqueColumns<'a, T: 'a> {
    /// Returns an iterator over the unique columns
    fn unique_columns(&'a self) -> impl Iterator<Item = Column>;
}

impl<'a, T: Clone + Ord + std::fmt::Display + 'a, E: AllChildren<AlgebraicExpression<T>>>
    UniqueColumns<'a, T> for E
{
    fn unique_columns(&'a self) -> impl Iterator<Item = Column> {
        self.all_children()
            .filter_map(|e| {
                if let AlgebraicExpression::Reference(r) = e {
                    Some(Column::from(r))
                } else {
                    None
                }
            })
            .unique()
    }
}

/// Given an expression `expr`
/// - Reassign the IDs of all references in the expression to be unique and larger or equal than `curr_id`
/// - Add a suffix to the name of each reference, so that `foo` becomes `foo_suffix`
///
/// Return the next available ID.
/// Assumptions:
/// - The substitution map already contains substitutions for constant columns. Failing to do so will result in a panic.
pub fn reassign_ids_algebraic<T: Clone + Ord>(
    expr: &mut AlgebraicExpression<T>,
    mut curr_id: u64,
    subs: &mut BTreeMap<Column, Column>,
    suffix: usize,
) -> u64 {
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(r) = expr {
                let column = Column::from(&*r);
                match subs.get(&column) {
                    Some(c) => {
                        r.poly_id = c.id;
                        r.name = c.name.clone();
                    }
                    None => {
                        assert_eq!(r.poly_id.ptype, PolynomialType::Committed);
                        let new_column = Column {
                            name: format!("{}_{}", column.name, suffix),
                            id: PolyID {
                                id: curr_id,
                                ptype: PolynomialType::Committed,
                            },
                        };
                        r.poly_id = new_column.id;
                        r.name = new_column.name.clone();
                        subs.insert(column.clone(), new_column.clone());
                        curr_id += 1;
                    }
                }
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    curr_id
}

pub fn substitute(expr: &mut Expression, sub: &BTreeMap<String, Expression>) {
    expr.visit_expressions_mut(
        &mut |expr| {
            match expr {
                Expression::Reference(_, ref mut r) => {
                    if let Some(sub_expr) = sub.get(&r.path.to_string()) {
                        *expr = sub_expr.clone();
                    }
                }
                Expression::UnaryOperation(_, ref mut un_op) => {
                    if matches!(un_op.op, UnaryOperator::Next) {
                        if let Expression::Reference(_, ref r) = &*un_op.expr {
                            let name = r.path.try_last_part().unwrap();
                            if name == "pc" {
                                let pc_next_symbol =
                                    SymbolPath::from_identifier("pc_next".to_string());
                                let pc_next_ref: NamespacedPolynomialReference =
                                    pc_next_symbol.into();
                                let pc_next_ref =
                                    Expression::Reference(Default::default(), pc_next_ref);
                                *expr = pc_next_ref.clone();
                            }
                        }
                    }
                }
                _ => (),
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
}

pub fn powdr_interaction_to_symbolic<T: FieldElement>(
    powdr_interaction: BusInteractionIdentity<T>,
) -> SymbolicBusInteraction<T> {
    let kind = match powdr_interaction.latch {
        AlgebraicExpression::Number(n) => {
            let n: u64 = n.to_arbitrary_integer().try_into().unwrap();
            if n == 0 {
                BusInteractionKind::Receive
            } else if n == 1 {
                BusInteractionKind::Send
            } else {
                panic!("Expected latch = 0 or 1 for interaction kind")
            }
        }
        //AlgebraicExpression::UnaryOperation(_) => BusInteractionKind::Receive,
        // TODO complex expressions are handled as Sends for now
        //_ => BusInteractionKind::Send,
        _ => panic!("Expected latch = 0 or 1 for interaction kind"),
    };

    let id: u64 = match powdr_interaction.bus_id {
        AlgebraicExpression::Number(n) => n.to_arbitrary_integer().try_into().unwrap(),
        _ => panic!("Bus ID must be a Number"),
    };

    SymbolicBusInteraction {
        kind,
        id,
        mult: powdr_interaction.multiplicity,
        args: powdr_interaction.payload.0,
    }
}
