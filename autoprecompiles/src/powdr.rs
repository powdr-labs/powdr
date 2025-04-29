use std::collections::{BTreeMap, BTreeSet};
use std::ops::ControlFlow;

use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicReferenceThin, AlgebraicUnaryOperation, BusInteractionIdentity, PolyID,
    PolynomialType,
};
use powdr_ast::parsed::asm::SymbolPath;
use powdr_ast::parsed::{
    visitor::{ExpressionVisitable, VisitOrder},
    NamespacedPolynomialReference, UnaryOperator,
};
use powdr_number::FieldElement;

use crate::{BusInteractionKind, SymbolicBusInteraction};

type Expression = powdr_ast::asm_analysis::Expression<NamespacedPolynomialReference>;

// After powdr and lib are adjusted, this function can be renamed and the old substitute removed
pub fn substitute_algebraic<T: Clone>(
    expr: &mut AlgebraicExpression<T>,
    sub: &BTreeMap<String, AlgebraicExpression<T>>,
) {
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { name, .. }) = expr {
                if let Some(sub_expr) = sub.get(name) {
                    *expr = sub_expr.clone();
                }
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
}

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

pub fn is_zero<T: FieldElement>(expr: &AlgebraicExpression<T>) -> bool {
    match expr {
        AlgebraicExpression::Number(n) => *n == T::zero(),
        _ => false,
    }
}

pub fn find_byte_decomp<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
) -> (AlgebraicExpression<T>, AlgebraicExpression<T>) {
    let mut e1 = None;
    let mut e2 = None;
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
                    let is_mul_by_2_17 = matches!(&**inner_right, AlgebraicExpression::Number(n) if *n == 131072u32.into());
                    if is_ref(&**left) && is_ref(&**inner_left) && is_mul_by_2_17 {
                        assert!(e1.is_none());
                        assert!(e2.is_none());
                        e1 = Some((**left).clone());
                        e2 = Some((**inner_left).clone());
                        return ControlFlow::Break(());
                    }
                }
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    (e1.unwrap(), e2.unwrap())
}

pub fn has_ref<T: Clone + std::cmp::PartialEq>(
    expr: &AlgebraicExpression<T>,
    r: &AlgebraicExpression<T>,
) -> bool {
    let mut seen = false;
    expr.visit_expressions(
        &mut |expr| {
            if expr == r {
                seen = true;
                ControlFlow::Break::<()>(())
            } else {
                ControlFlow::Continue::<()>(())
            }
        },
        VisitOrder::Pre,
    );
    seen
}

pub fn is_ref<T: Clone>(expr: &AlgebraicExpression<T>) -> bool {
    matches!(expr, AlgebraicExpression::Reference(_))
}

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

pub fn append_suffix_algebraic<T: Clone>(
    expr: &mut AlgebraicExpression<T>,
    suffix: &str,
) -> BTreeMap<String, String> {
    let mut subs = BTreeMap::new();
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { name, .. }) = expr {
                if !["is_first_row", "is_transition", "is_last_row"].contains(&name.as_str()) {
                    let new_name = format!("{name}_{suffix}");
                    subs.insert(name.clone(), new_name.clone());
                    *name = new_name;
                }
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    subs
}

// After powdr and lib are adjusted, this function can be renamed and the old collect_cols removed
pub fn collect_cols_algebraic<T: Clone + Ord>(
    expr: &AlgebraicExpression<T>,
) -> BTreeSet<AlgebraicExpression<T>> {
    let mut cols: BTreeSet<AlgebraicExpression<T>> = Default::default();
    expr.visit_expressions(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference {
                poly_id:
                    PolyID {
                        ptype: PolynomialType::Committed,
                        ..
                    },
                ..
            }) = expr
            {
                cols.insert(expr.clone());
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    cols
}

pub fn collect_cols_names_algebraic<T: Clone + Ord + std::fmt::Display>(
    expr: &AlgebraicExpression<T>,
) -> BTreeSet<String> {
    let mut cols: BTreeSet<String> = Default::default();
    expr.visit_expressions(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { name, .. }) = expr {
                cols.insert(name.clone());
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    cols
}

pub fn collect_cols_ids_algebraic<T: Clone + Ord + std::fmt::Display>(
    expr: &AlgebraicExpression<T>,
) -> BTreeSet<u64> {
    let mut cols: BTreeSet<u64> = Default::default();
    expr.visit_expressions(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { poly_id, .. }) = expr {
                cols.insert(poly_id.id);
            }
            ControlFlow::Continue::<()>(())
        },
        VisitOrder::Pre,
    );
    cols
}

pub fn reassign_ids_algebraic<T: Clone + Ord>(
    expr: &mut AlgebraicExpression<T>,
    mut curr_id: usize,
    subs: &mut BTreeMap<String, usize>,
    rev_subs: &mut BTreeMap<usize, String>,
) -> usize {
    expr.visit_expressions_mut(
        &mut |expr| {
            if let AlgebraicExpression::Reference(AlgebraicReference { name, poly_id, .. }) = expr {
                match subs.get_mut(name.as_str()) {
                    Some(id) => {
                        match rev_subs.get(id) {
                            Some(r_name) => {
                                assert_eq!(r_name, name, "Id {id} already assigned to {name}");
                            }
                            None => {
                                rev_subs.insert(*id, name.clone());
                            }
                        }
                        poly_id.id = *id as u64;
                    }
                    None => {
                        poly_id.id = curr_id as u64;
                        subs.insert(name.clone(), curr_id);
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

pub fn append_suffix_mut(expr: &mut Expression, suffix: &str) {
    expr.visit_expressions_mut(
        &mut |expr| match expr {
            Expression::FunctionCall(_, ref mut fun_call) => {
                for arg in &mut fun_call.arguments {
                    append_suffix_mut(arg, suffix);
                }
                ControlFlow::Break::<()>(())
            }
            Expression::Reference(_, ref mut r) => {
                let name = r.path.try_last_part().unwrap();
                if name != "pc" && name != "pc_next" && name != "dest" {
                    let name = format!("{name}_{suffix}");
                    *r.path.try_last_part_mut().unwrap() = name;
                }
                ControlFlow::Continue::<()>(())
            }
            _ => ControlFlow::Continue::<()>(()),
        },
        VisitOrder::Pre,
    );
}

pub fn powdr_interaction_to_symbolic<T: FieldElement>(
    powdr_interaction: &BusInteractionIdentity<T>,
    intermediates: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
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
        mult: inline_intermediates(&powdr_interaction.multiplicity, intermediates),
        args: powdr_interaction
            .payload
            .0
            .iter()
            .map(|e| inline_intermediates(e, intermediates))
            .collect(),
    }
}

/// Replaces any reference of intermediates with their definitions.
/// This is needed because powdr Autoprecompiles currently does not implement
/// intermediates.
pub fn inline_intermediates<T: FieldElement>(
    expr: &AlgebraicExpression<T>,
    intermediates: &BTreeMap<AlgebraicReferenceThin, AlgebraicExpression<T>>,
) -> AlgebraicExpression<T> {
    match expr {
        AlgebraicExpression::Reference(algebraic_reference) => {
            if algebraic_reference.poly_id.ptype == PolynomialType::Intermediate {
                inline_intermediates(
                    intermediates
                        .get(&algebraic_reference.to_thin())
                        .expect("Intermediate not found"),
                    intermediates,
                )
            } else {
                expr.clone()
            }
        }
        AlgebraicExpression::PublicReference(..)
        | AlgebraicExpression::Challenge(..)
        | AlgebraicExpression::Number(..) => expr.clone(),
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                left: Box::new(inline_intermediates(left, intermediates)),
                op: *op,
                right: Box::new(inline_intermediates(right, intermediates)),
            })
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => {
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation {
                op: *op,
                expr: Box::new(inline_intermediates(expr, intermediates)),
            })
        }
    }
}
