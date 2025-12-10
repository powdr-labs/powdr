use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use itertools::Itertools;
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

/// The Environment in the main method to access information about
/// the constraint system. It allows rules to translate
/// the opaque Expr identifiers into GroupedExpressions and perform
/// actions on them.
/// It is available to the rules as a singleton with interior mutability.
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
        // Environment is a singleton.
        true
    }
}

impl<T: FieldElement> Eq for Environment<T> {}

impl<T: FieldElement> PartialOrd for Environment<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // Environment is a singleton.
        Some(self.cmp(other))
    }
}

impl<T: FieldElement> Ord for Environment<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        // Environment is a singleton.
        std::cmp::Ordering::Equal
    }
}

impl<T: FieldElement> Hash for Environment<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Environment is a singleton.
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

    /// Re-extract re-usable components after the rules have run.
    pub fn terminate(self) -> (ItemDB<GroupedExpression<T, Var>, Expr>, NewVarGenerator<T>) {
        (
            self.expressions.into_inner(),
            self.new_var_generator.into_inner(),
        )
    }

    /// Turns a GroupedExpression into the corresponding Expr,
    /// allocating a new ID if it is not yet present.
    /// Use this function when you only have a reference to the expression.
    pub fn insert(&self, expr: &GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert(expr)
    }

    /// Turns a GroupedExpression into the corresponding Expr,
    /// allocating a new ID if it is not yet present.
    /// Use this function instead of `insert` when you own the expression.
    pub fn insert_owned(&self, expr: GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert_owned(expr)
    }

    /// Turns an Expr into an owned GroupedExpression.
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
    /// If this returns Some(n) then the expression is affine
    /// and contains n variables.
    pub fn affine_var_count(&self, expr: Expr) -> Option<usize> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        expr.is_affine().then(|| expr.linear_components().len())
    }

    /// If this returns Some((coeff, var, offset)) then the expression is affine
    /// and equals `coeff * var + offset`.
    pub fn try_to_affine(&self, expr: Expr) -> Option<(T, Var, T)> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        if !expr.is_affine() {
            return None;
        }
        let (var, coeff) = expr.linear_components().exactly_one().ok()?;
        Some((*coeff, *var, *expr.constant_offset()))
    }

    /// Runs the function `f` on the expression identified by `expr`,
    /// passing `args` as additional arguments.
    /// This function is needed because we cannot return
    /// references to GroupedExpression due to the interior mutability.
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
    /// If this returns Some(e1, e2) then the expression equals e1 * e2.
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

    /// Substitutes the variable `var` by the constant `value` in the expression `e`
    /// and returns the resulting expression.
    #[allow(dead_code)]
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

    /// Substitutes the variable `var` by the variable `replacement` in the expression `e`
    /// and returns the resulting expression.
    #[allow(dead_code)]
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
