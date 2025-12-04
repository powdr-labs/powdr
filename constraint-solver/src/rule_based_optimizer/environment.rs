use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use itertools::{EitherOrBoth, Itertools};
use powdr_number::FieldElement;

use crate::{
    constraint_system::ComputationMethod,
    grouped_expression::{GroupedExpression, RangeConstraintProvider},
    range_constraint::RangeConstraint,
    rule_based_optimizer::{
        item_db::ItemDB,
        new_var_generator::NewVarGenerator,
        types::{Expr, Var},
    },
    runtime_constant::VarTransformable,
};

pub struct Environment<T: FieldElement> {
    expressions: RefCell<ItemDB<GroupedExpression<T, Var>, Expr>>,
    var_to_string: HashMap<Var, String>,

    /// Variables that only occurr once in the system
    /// (also only once in the constraint they occur in).
    single_occurrence_variables: HashSet<Var>,
    range_constraints_on_vars: HashMap<Var, RangeConstraint<T>>,
    new_var_generator: RefCell<NewVarGenerator<T>>,
}

impl<T: FieldElement> RangeConstraintProvider<T, Var> for Environment<T> {
    fn get(&self, var: &Var) -> RangeConstraint<T> {
        self.range_constraints_on_vars
            .get(var)
            .cloned()
            .unwrap_or(RangeConstraint::unconstrained())
    }
}

impl<T: FieldElement> PartialEq for Environment<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T: FieldElement> Eq for Environment<T> {}

impl<T: FieldElement> PartialOrd for Environment<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: FieldElement> Ord for Environment<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T: FieldElement> Hash for Environment<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        0.hash(state);
    }
}

impl<T: FieldElement> Environment<T> {
    pub fn new(
        expressions: ItemDB<GroupedExpression<T, Var>, Expr>,
        var_to_string: HashMap<Var, String>,
        single_occurrence_variables: HashSet<Var>,
        range_constraints_on_vars: HashMap<Var, RangeConstraint<T>>,
        new_var_generator: NewVarGenerator<T>,
    ) -> Self {
        Self {
            expressions: RefCell::new(expressions),
            var_to_string,
            single_occurrence_variables,
            range_constraints_on_vars,
            new_var_generator: RefCell::new(new_var_generator),
        }
    }

    pub fn terminate(self) -> (ItemDB<GroupedExpression<T, Var>, Expr>, NewVarGenerator<T>) {
        (
            self.expressions.into_inner(),
            self.new_var_generator.into_inner(),
        )
    }

    #[allow(dead_code)]
    pub fn insert(&self, expr: &GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert(expr)
    }

    pub fn insert_owned(&self, expr: GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert_owned(expr)
    }

    /// Extract an Expr into a free GroupedExpression.
    /// This is expensive since it clones the expression.
    #[allow(dead_code)]
    pub fn extract(&self, expr: Expr) -> GroupedExpression<T, Var> {
        self.expressions.borrow()[expr].clone()
    }

    #[allow(dead_code)]
    pub fn new_var(
        &self,
        prefix: &str,
        method: ComputationMethod<T, GroupedExpression<T, Var>>,
    ) -> Var {
        self.new_var_generator.borrow_mut().generate(prefix, method)
    }

    #[allow(dead_code)]
    pub fn single_occurrence_variables(&self) -> impl Iterator<Item = &Var> {
        self.single_occurrence_variables.iter()
    }

    #[allow(dead_code)]
    pub fn affine_var_count(&self, expr: Expr) -> Option<usize> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        expr.is_affine().then(|| expr.linear_components().len())
    }

    pub fn try_to_affine(&self, expr: Expr) -> Option<(T, Var, T)> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        if !expr.is_affine() {
            return None;
        }
        let (var, coeff) = expr.linear_components().exactly_one().ok()?;
        Some((*coeff, *var, *expr.constant_offset()))
    }

    pub fn on_expr<Args, Ret>(
        &self,
        expr: Expr,
        args: Args,
        f: impl Fn(&GroupedExpression<T, Var>, Args) -> Ret,
    ) -> Ret {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        f(expr, args)
    }

    #[allow(dead_code)]
    pub fn try_as_single_product(&self, expr: Expr) -> Option<(Expr, Expr)> {
        let (l, r) = {
            let db = self.expressions.borrow();
            let (l, r) = db[expr].try_as_single_product()?;
            (l.clone(), r.clone())
        };
        // TODO eventually, l and r are cloned.
        // if we change GroupedExpression to use `Expr` for the recursion, we do not
        // have to insert everything multiple times.
        Some((self.insert(&l), self.insert(&r)))
    }

    /// Returns Some(C) if `a - b = C' and both are affine.
    pub fn constant_difference(&self, a: Expr, b: Expr) -> Option<T> {
        let db = self.expressions.borrow();
        let a = &db[a];
        let b = &db[b];
        (a.is_affine()
            && b.is_affine()
            && a.linear_components()
                .zip(b.linear_components())
                .all(|(x, y)| x == y))
        .then(|| *a.constant_offset() - *b.constant_offset())
    }

    /// If this returns `Some((v1, v2, coeff))`, then `a` and `b` are affine expressions
    /// such that `b` is obtained from `a` when replacing `v1` by `v2` and
    /// `coeff` is the coefficient of `v1` in `a` (and also of `v2` in `b`)
    /// also `a` and `b` have at least two variables each.
    pub fn differ_in_exactly_one_variable(&self, a_id: Expr, b_id: Expr) -> Option<(Var, Var, T)> {
        let db = self.expressions.borrow();
        let a = &db[a_id];
        let b = &db[b_id];
        if !a.is_affine()
            || !b.is_affine()
            || a.constant_offset() != b.constant_offset()
            || a.linear_components().len() != b.linear_components().len()
            || a.linear_components().len() < 2
        {
            return None;
        }
        let mut joined = a
            .linear_components()
            // Join the sorted iterators into another sorted list,
            // noting where the items came from.
            .merge_join_by(b.linear_components(), Ord::cmp)
            // Remove those that are equal in both iterators.
            .filter(|either| !matches!(either, EitherOrBoth::Both(_, _)));
        let first_diff = joined.next()?;
        let second_diff = joined.next()?;
        if joined.next().is_some() {
            return None;
        }
        let (left_var, right_var, coeff) = match (first_diff, second_diff) {
            (EitherOrBoth::Left((lv, lc)), EitherOrBoth::Right((rv, rc)))
            | (EitherOrBoth::Right((rv, rc)), EitherOrBoth::Left((lv, lc))) => {
                if lc != rc {
                    return None;
                }
                (*lv, *rv, *lc)
            }
            _ => return None,
        };
        Some((left_var, right_var, coeff))
    }

    pub fn substitute_by_known(&self, e: Expr, var: Var, value: T) -> Expr {
        let expr = {
            let db = self.expressions.borrow();
            let mut expr = db[e].clone();
            // expr.substitute_by_known(&var, &value);
            expr.substitute_simple(&var, value);
            expr
        };
        self.insert_owned(expr)
    }

    pub fn substitute_by_var(&self, e: Expr, var: Var, replacement: Var) -> Expr {
        let expr = {
            let db = self.expressions.borrow();
            let mut expr = db[e].clone();
            expr.substitute_by_unknown(
                &var,
                &GroupedExpression::from_unknown_variable(replacement),
            );
            expr
        };
        self.insert_owned(expr)
    }

    #[allow(dead_code)]
    pub fn format_expr(&self, expr: Expr) -> String {
        let db = self.expressions.borrow();
        db[expr]
            .transform_var_type(&mut |v| &self.var_to_string[v])
            .to_string()
    }

    #[allow(dead_code)]
    pub fn format_var(&self, var: Var) -> String {
        self.var_to_string
            .get(&var)
            .cloned()
            .unwrap_or_else(|| var.to_string())
    }
}
