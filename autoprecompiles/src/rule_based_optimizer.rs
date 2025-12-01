#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `Env`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Index,
};

use itertools::{EitherOrBoth, Itertools};
use powdr_constraint_solver::{
    algebraic_constraint,
    constraint_system::{
        BusInteraction, BusInteractionHandler, ComputationMethod, ConstraintSystem, DerivedVariable,
    },
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, RangeConstraintProvider},
    indexed_constraint_system::IndexedConstraintSystem,
    inliner::DegreeBound,
    range_constraint::RangeConstraint,
    runtime_constant::VarTransformable,
};
use powdr_number::FieldElement;

use derive_more::{From, Into};
#[allow(unused_imports)]
use num_traits::Zero;

use crepe::crepe;

const SIZE_LIMIT: usize = 1600;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into)]
struct Var(usize);

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v_{}", self.0)
    }
}

/// A database of items that are assigned consecutive identifiers
/// and which can translate back and forth between identifiers
/// and items.
struct ItemDB<Item, Ident> {
    items: Vec<Item>,
    reverse: HashMap<Item, usize>,
    _phantom: std::marker::PhantomData<Ident>,
}

impl<Item, Ident> Default for ItemDB<Item, Ident> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            reverse: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Item, Ident> FromIterator<Item> for ItemDB<Item, Ident>
where
    Item: Clone + Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = Item>>(iter: T) -> Self {
        let items = iter.into_iter().collect::<Vec<_>>();
        let reverse = items
            .iter()
            .enumerate()
            .map(|(i, v)| (v.clone(), i))
            .collect();
        Self {
            items,
            reverse,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Item, Ident> Index<Ident> for ItemDB<Item, Ident>
where
    Ident: Into<usize>,
{
    type Output = Item;
    fn index(&self, index: Ident) -> &Self::Output {
        &self.items[index.into()]
    }
}

impl<Item, Ident> ItemDB<Item, Ident>
where
    Item: Clone + Hash + Eq,
    Ident: From<usize> + Copy,
{
    fn insert_owned_new(&mut self, item: Item) -> Ident {
        let id = self.items.len();
        self.items.push(item.clone());
        self.reverse.insert(item, id);
        Ident::from(id)
    }

    pub fn insert(&mut self, item: &Item) -> Ident {
        if let Some(&id) = self.reverse.get(item) {
            Ident::from(id)
        } else {
            self.insert_owned_new(item.clone())
        }
    }

    pub fn insert_owned(&mut self, item: Item) -> Ident {
        if let Some(&id) = self.reverse.get(&item) {
            Ident::from(id)
        } else {
            self.insert_owned_new(item)
        }
    }

    pub fn id(&self, item: &Item) -> Ident {
        self.reverse.get(item).map(|&id| Ident::from(id)).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Ident, &Item)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (Ident::from(i), item))
    }

    // TODO avoid using this (as pub)
    pub fn next_free_id(&self) -> usize {
        self.items.len()
    }
}

struct Environment<T: FieldElement> {
    expressions: RefCell<ItemDB<GroupedExpression<T, Var>, Expr>>,
    var_to_string: HashMap<Var, String>,

    /// Variables that only occurr once in the system
    /// (also only once in the constraint they occur in).
    single_occurrence_variables: HashSet<Var>,
    range_constraints_on_vars: HashMap<Var, RangeConstraint<T>>,
    new_var_generator: RefCell<NewVarGenerator<T>>,
}

impl<T: FieldElement> Environment<T> {
    fn new(
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

    fn terminate(self) -> (ItemDB<GroupedExpression<T, Var>, Expr>, NewVarGenerator<T>) {
        (
            self.expressions.into_inner(),
            self.new_var_generator.into_inner(),
        )
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
    pub fn insert(&self, expr: &GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert(expr)
    }

    pub fn insert_owned(&self, expr: GroupedExpression<T, Var>) -> Expr {
        self.expressions.borrow_mut().insert_owned(expr)
    }

    /// Extract an Expr into a free GroupedExpression.
    /// This is expensive since it clones the expression.
    pub fn extract(&self, expr: Expr) -> GroupedExpression<T, Var> {
        self.expressions.borrow()[expr].clone()
    }

    pub fn new_var(
        &self,
        prefix: &str,
        method: ComputationMethod<T, GroupedExpression<T, Var>>,
    ) -> Var {
        self.new_var_generator.borrow_mut().generate(prefix, method)
    }

    pub fn referenced_variables(&self, expr: Expr) -> BTreeSet<Var> {
        let db = self.expressions.borrow();
        db[expr].referenced_unknown_variables().cloned().collect()
    }

    pub fn single_occurrence_variables(&self) -> impl Iterator<Item = &Var> {
        self.single_occurrence_variables.iter()
    }

    pub fn affine_var_count(&self, expr: Expr) -> Option<usize> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        expr.is_affine().then(|| expr.linear_components().count())
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

    pub fn range_constraint_on_expr(&self, expr: Expr) -> RangeConstraint<T> {
        let db = self.expressions.borrow();
        db[expr].range_constraint(self)
    }

    // TODO potentially make this a more generic "matches structure" function
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
            || a.referenced_unknown_variables().count() != b.referenced_unknown_variables().count()
            || a.referenced_unknown_variables().count() < 2
        {
            return None;
        }
        if a.constant_offset() != b.constant_offset() {
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

struct NewVarGenerator<T> {
    counter: usize,
    requests: Vec<(Var, String)>,
    computation_methods: HashMap<Var, ComputationMethod<T, GroupedExpression<T, Var>>>,
}

impl<T> NewVarGenerator<T> {
    fn new(initial_counter: usize) -> Self {
        Self {
            counter: initial_counter,
            requests: Default::default(),
            computation_methods: Default::default(),
        }
    }

    fn generate(
        &mut self,
        prefix: &str,
        computation_method: ComputationMethod<T, GroupedExpression<T, Var>>,
    ) -> Var {
        let var = Var::from(self.counter);
        self.requests.push((var, prefix.to_string()));
        self.computation_methods.insert(var, computation_method);
        self.counter += 1;
        var
    }
}

impl<T: FieldElement> RangeConstraintProvider<T, Var> for Environment<T> {
    fn get(&self, var: &Var) -> RangeConstraint<T> {
        self.range_constraints_on_vars
            .get(var)
            .cloned()
            .unwrap_or(RangeConstraint::unconstrained())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into)]
struct Expr(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Action<T> {
    SubstituteVariableByConstant(Var, T),
    SubstituteVariableByVariable(Var, Var),
    ReplaceAlgebraicConstraintBy(Expr, Expr),
    ReplacePairOfAlgebraicConstraintsBy(Expr, Expr, Expr),
}

crepe! {
    @input
    struct Env<'a, T: FieldElement>(&'a Environment<T>);

    @input
    struct InitialAlgebraicConstraint(Expr);

    struct AlgebraicConstraint(Expr);
    AlgebraicConstraint(e) <- InitialAlgebraicConstraint(e);

    // @input
    // struct BusInteractionConstraint<'a>(&'a BusInteraction<GroupedExpression<F, Var>>);

    struct RangeConstraintOnExpression<T: FieldElement>(Expr, RangeConstraint<T>);

    struct Expression(Expr);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- RangeConstraintOnExpression(e, _);

    RangeConstraintOnExpression(e, rc) <-
      Env(env),
      Expression(e),
      let rc = env.range_constraint_on_expr(e);


    struct ContainsVariable(Expr, Var);
    ContainsVariable(e, v) <-
      Expression(e),
      Env(env),
      for v in env.referenced_variables(e);

    struct AffineExpression<T: FieldElement>(Expr, T, Var, T);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      Env(env),
      let Some((coeff, var, offset)) = env.try_to_affine(e);

    struct RangeConstraintOnVar<T: FieldElement>(Var, RangeConstraint<T>);
    RangeConstraintOnVar(v, rc) <- Env(env), ContainsVariable(_, v), let rc = env.get(&v);
    // RC(coeff * var + offset) = rc <=>
    // coeff * RC(var) + offset = rc <=>
    // RC(var) = (rc - offset) / coeff
    RangeConstraintOnVar(v, rc.combine_sum(&RangeConstraint::from_value(-offset)).multiple(T::one() / coeff)) <-
      RangeConstraintOnExpression(e, rc),
      AffineExpression(e, coeff, v, offset),
      (coeff != T::zero());

    RangeConstraintOnVar(v, v_rc1.conjunction(&v_rc2)) <-
      RangeConstraintOnVar(v, v_rc1),
      RangeConstraintOnVar(v, v_rc2);

    struct Product(Expr, Expr, Expr);
    Product(e, l, r) <-
      Expression(e),
      Env(env),
      let Some((l, r)) = env.try_as_single_product(e);
    Product(e, r, l) <- Product(e, l, r);

    // (E, expr, offset) <-> E = (expr) * (expr + offset) is a constraint
    struct QuadraticEquivalenceCandidate<T: FieldElement>(Expr, Expr, T);
    QuadraticEquivalenceCandidate(e, r, offset) <-
       AlgebraicConstraint(e),
       Env(env),
       Product(e, l, r),
       ({env.affine_var_count(l).unwrap_or(0) > 1 && env.affine_var_count(r).unwrap_or(0) > 1}),
       let Some(offset) = env.constant_difference(l, r);

    struct QuadraticEquivalence(Var, Var);
    QuadraticEquivalence(v1, v2) <-
      QuadraticEquivalenceCandidate(_, expr1, offset),
      QuadraticEquivalenceCandidate(_, expr2, offset),
      Env(env),
      let Some((v1, v2, coeff)) = env.differ_in_exactly_one_variable(expr1, expr2),
      RangeConstraintOnVar(v1, rc),
      RangeConstraintOnVar(v2, rc),
      (rc.is_disjoint(&rc.combine_sum(&RangeConstraint::from_value(offset / coeff))));

    struct ReplaceAlgebraicConstraintBy(Expr, Expr);

    // Combine multiple variables that only occur in the same algebraic constraint.
    //
    // Assume we have an algebraic constraint of the form `X * V1 + Y * V2 = R`,
    // where `V1` and `V2` only occur in this constraint and only once.
    // The only combination of values for `X`, `Y` and `R` where this is _not_ satisfiable
    // is `X = 0`, `Y = 0`, `R != 0`. So the constraint is equivalent to the statement
    // `(X = 0 and Y = 0) -> R = 0`.
    //
    // Considering the simpler case where both `X` and `Y` are non-negative such that
    // `X + Y` does not wrap.
    // Then `X = 0 and Y = 0` is equivalent to `X + Y = 0`. So we can replace the constraint
    // by `(X + Y) * V3 = C`, where `V3` is a new variable that only occurs here.
    //
    // For the general case, where e.g. `X` can be negative, we replace it by `X * X`,
    // if that value is still small enough.
    struct SingleOccurrenceVariable(Expr, Var);
    SingleOccurrenceVariable(e, v) <-
      Env(env),
      for v in env.single_occurrence_variables().cloned(),
      AlgebraicConstraint(e),
      // We somehow cannot use "v" directly here.
      ContainsVariable(e, v2),
      (v == v2);

    struct LargestSingleOccurrenceVariablePairInExpr(Expr, Var, Var);
    LargestSingleOccurrenceVariablePairInExpr(e, v1, v2) <-
      Env(env),
      SingleOccurrenceVariable(e, v1),
      SingleOccurrenceVariable(e, v2),
      (v1 < v2),
      (env
        .single_occurrence_variables()
        .filter(|v3| env.referenced_variables(e).contains(v3))
        .all(|&v3| v3 == v1 || v3 == v2 || v3 < v1));

    struct HasProductSummand(Expr, Expr, Expr);
    HasProductSummand(e, l, r) <-
      Env(env),
      Expression(e),
      (!env.on_expr(e, (), |e, _| e.is_affine())),
      for (l, r) in env.extract(e).into_summands().filter_map(|s| {
          if let GroupedExpressionComponent::Quadratic(l, r) = s {
              Some((env.insert_owned(l), env.insert_owned(r)))
          } else {
              None
          }
      });
    HasProductSummand(e, r, l) <- HasProductSummand(e, l, r);
    Expression(l) <- HasProductSummand(_, l, _);
    Expression(r) <- HasProductSummand(_, _, r);

    // FreeVariableCombinationCandidate(e, coeff1, v1, coeff2, v2, x1, x2)
    // if e is the expression of an algebraic constraint and
    // e = coeff1 * v1 * x1 + coeff2 * v2 * x2 + ...
    // where v1 and v2 are different variables that only occur here and only once.
    struct FreeVariableCombinationCandidate<T: FieldElement>(Expr, T, Var, Expr, T, Var, Expr);
    FreeVariableCombinationCandidate(e, coeff1, v1, x1, coeff2, v2, x2) <-
    //   SingleOccurrenceVariable(e, v1),
    //   SingleOccurrenceVariable(e, v2),
      // TODO this way, we could miss optimization opportunities
      // if this is not working out because of range constraints,
      // but at least the replacement becomes deterministic.
      LargestSingleOccurrenceVariablePairInExpr(e, v1, v2),
      AlgebraicConstraint(e),
      HasProductSummand(e, x1, v1_),
      AffineExpression(v1_, coeff1, v1, offset1),
      (offset1.is_zero()),
      HasProductSummand(e, x2, v2_),
      (x2 != v1_),
      (x1 != v2_),
      AffineExpression(v2_, coeff2, v2, offset2),
      (offset2.is_zero());

    struct PotentiallyReplaceAlgebraicConstraintBy(Expr, Expr);

    PotentiallyReplaceAlgebraicConstraintBy(e, replacement) <-
      Env(env),
      FreeVariableCombinationCandidate(e, coeff1, v1, x1, coeff2, v2, x2),
      // Here, we have e = coeff1 * v1 * x1 + coeff2 * v2 * x2 + ...
      RangeConstraintOnExpression(x1, rc1),
      RangeConstraintOnExpression(x2, rc2),
      let Some(replacement) = (|| {
        // If the expression is not known to be non-negative, we square it.
        let square_if_needed = |expr: Expr, rc: RangeConstraint<T>| {
            let expr = env.extract(expr);
            if rc.range().0 == T::zero() {
                (expr, rc)
            } else {
                (expr.clone() * expr, rc.square())
            }
        };
        let (x1, rc1) = square_if_needed(x1, rc1);
        let (x2, rc2) = square_if_needed(x2, rc2);
        if !rc1.range().0.is_zero() || !rc2.range().0.is_zero() {
            return None;
        }
        let sum_rc = rc1.multiple(coeff1).combine_sum(&rc2.multiple(coeff2));
        if !(sum_rc.range().0.is_zero() && sum_rc.range().1 < T::from(-1)) {
            return None;
        }
        // Remove the summands with v1 and v2 from the expression.
        let r = env.extract(e).into_summands().filter(|s|{
            if let GroupedExpressionComponent::Quadratic(l, r) = s {
                let mut vars = l.referenced_unknown_variables().chain(r.referenced_unknown_variables());
                if vars.any(|v| v == &v1 || v == &v2) {
                    return false;
                }
            };
            true
        }).map(GroupedExpression::from).sum::<GroupedExpression<T, Var>>();
        let factor = x1 * coeff1 + x2 * coeff2;
        let combined_var = env.new_var("free_var", ComputationMethod::QuotientOrZero(-r.clone(), factor.clone()));
        let replacement = r + GroupedExpression::from_unknown_variable(combined_var) * factor;
        Some(env.insert_owned(replacement))
      })();

    // If we have x * a = 0 and x * b = 0 and (a = 0 and b = 0) is equivalent to (a + b = 0),
    // replace those two by x * (a + b) = 0.
    struct PotentiallyReplacePairOfAlgebraicConstraintsBy(Expr, Expr, Expr);
    PotentiallyReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement) <-
      Env(env),
      AlgebraicConstraint(e1),
      AlgebraicConstraint(e2),
      Product(e1, x, a),
      Product(e2, x, b),
      (e1 < e2),
      RangeConstraintOnExpression(a, rc_a),
      RangeConstraintOnExpression(b, rc_b),
      (rc_a.range().0 == T::zero()
        && rc_b.range().0 == T::zero() && rc_a.combine_sum(&rc_b).range().1 < T::from(-1)),
      let replacement = env.insert_owned(env.extract(x) * (env.extract(a) + env.extract(b)));


    // TODO wait a second. We can craete range constraints on expressions for all
    // algebraic constraints. Then we just work on range constraints on expressions
    // instead of algebraic constraints. Might be more difficult with the scaling, though.

    struct Solvable<T: FieldElement>(Expr, Var, T);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    // Boolean range constraint
    RangeConstraintOnVar(v, RangeConstraint::from_range(x1, x1 + T::from(1))) <-
      AlgebraicConstraint(e),
      Product(e, l, r),
      Solvable(l, v, x1),
      Solvable(r, v, x1 + T::from(1));

    struct Assignment<T: FieldElement>(Var, T);
    Assignment(var, v) <-
      AlgebraicConstraint(e),
      Solvable(e, var, v);

    struct Equivalence(Var, Var);
    Equivalence(v1, v2) <- QuadraticEquivalence(v1, v2);

    // Do not do this because it is rather expensive.

    ReplaceAlgebraicConstraintBy(e, env.substitute_by_known(e, v, val)) <-
      Env(env),
      AlgebraicConstraint(e),
      ContainsVariable(e, v),
      Assignment(v, val);
    ReplaceAlgebraicConstraintBy(e, env.substitute_by_var(e, v, v2)) <-
       Env(env),
       AlgebraicConstraint(e),
       ContainsVariable(e, v),
       Equivalence(v, v2);

    // TODO this way, one constraint could be replaced by multiple
    // alternative constraints.
    AlgebraicConstraint(e) <-
      ReplaceAlgebraicConstraintBy(_, e);


    @output
    struct ActionRule<T>(Action<T>);
    ActionRule(Action::SubstituteVariableByConstant(v, val)) <-
      Assignment(v, val);
    ActionRule(Action::SubstituteVariableByVariable(v, v2)) <-
      Equivalence(v, v2);
    ActionRule(Action::ReplaceAlgebraicConstraintBy(old, new)) <-
      PotentiallyReplaceAlgebraicConstraintBy(old, new);
    ActionRule(Action::ReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement)) <-
      PotentiallyReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement);
}

pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    range_constraints: impl RangeConstraintProvider<T, V>,
    _bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
    new_var_outer: &mut impl FnMut(&str) -> V,
    degree_bound: Option<DegreeBound>,
) -> (
    IndexedConstraintSystem<T, V>,
    Vec<algebraic_constraint::AlgebraicConstraint<GroupedExpression<T, V>>>,
) {
    if system.system().algebraic_constraints.len() > SIZE_LIMIT {
        log::debug!(
            "Skipping rule-based optimization because the system is too large ({} > {}).",
            system.system().algebraic_constraints.len(),
            SIZE_LIMIT
        );
        return (system, vec![]);
    }

    let mut additional_algebraic_constraints = vec![];
    let mut var_mapper = system
        .referenced_unknown_variables()
        .cloned()
        .collect::<ItemDB<V, Var>>();

    let mut expr_db = Some(ItemDB::<GroupedExpression<T, Var>, Expr>::default());

    loop {
        let (algebraic_constraints, _bus_interactions) =
            transform_constraint_system(&system, &var_mapper, expr_db.as_mut().unwrap());

        let env = Environment::<T>::new(
            expr_db.take().unwrap(),
            var_mapper
                .iter()
                .map(|(id, var)| (id, var.to_string()))
                .collect(),
            system
                .single_occurrence_variables()
                .map(|v| var_mapper.id(v))
                .collect(),
            system
                .referenced_unknown_variables()
                .map(|v| (var_mapper.id(v), range_constraints.get(v)))
                .collect(),
            // TODO or we just clone the var mapper?
            NewVarGenerator::new(var_mapper.next_free_id()),
        );

        let mut rt = Crepe::new();
        rt.extend(
            algebraic_constraints
                .iter()
                .copied()
                .map(InitialAlgebraicConstraint),
        );
        rt.extend(std::iter::once(Env(&env)));

        let (actions,) = rt.run();
        let (db, new_var_generator) = env.terminate();
        expr_db = Some(db);

        // TODO we do not need all of those variables.
        for (var, prefix) in new_var_generator.requests {
            let v = new_var_outer(&prefix);
            assert_eq!(var_mapper.insert(&v), var);
            let computation_method = untransform_computation_method(
                new_var_generator.computation_methods.get(&var).unwrap(),
                &var_mapper,
            );
            system.extend(ConstraintSystem {
                derived_variables: vec![DerivedVariable {
                    variable: v.clone(),
                    computation_method,
                }],
                ..Default::default()
            });
        }

        let mut progress = false;
        for action in actions.into_iter().map(|a| a.0).sorted() {
            match action {
                Action::SubstituteVariableByConstant(var, val) => {
                    system.substitute_by_known(&var_mapper[var], &val);
                    progress = true;
                }
                Action::SubstituteVariableByVariable(v1, v2) => {
                    let (v1, v2) = if var_mapper[v1] < var_mapper[v2] {
                        (v1, v2)
                    } else {
                        (v2, v1)
                    };
                    // We need to notify the solver of the equivalence.
                    additional_algebraic_constraints.push(
                        algebraic_constraint::AlgebraicConstraint::assert_zero(
                            GroupedExpression::from_unknown_variable(var_mapper[v1].clone())
                                - GroupedExpression::from_unknown_variable(var_mapper[v2].clone()),
                        ),
                    );
                    system.substitute_by_unknown(
                        &var_mapper[v1],
                        &GroupedExpression::from_unknown_variable(var_mapper[v2].clone()),
                    );
                    progress = true;
                }
                Action::ReplaceAlgebraicConstraintBy(e1, e2) => {
                    let expr1 =
                        untransform_grouped_expression(&expr_db.as_ref().unwrap()[e1], &var_mapper);
                    let expr2 =
                        untransform_grouped_expression(&expr_db.as_ref().unwrap()[e2], &var_mapper);
                    // If the degree does not increase, we do it in any case. If the degree increases, we
                    // only do it if a degree bound is given and it stays within the bound.
                    if expr2.degree() > expr1.degree()
                        && (degree_bound.is_none()
                            || expr2.degree() > degree_bound.unwrap().identities)
                    {
                        log::debug!(
                            "Skipping replacement of {expr1} by {expr2} due to degree constraints."
                        );
                        continue;
                    }
                    // TODO more efficient?
                    let mut found = false;
                    system.retain_algebraic_constraints(|c| {
                        if c.expression == expr1 {
                            found = true;
                            false
                        } else {
                            true
                        }
                    });
                    if found {
                        system.add_algebraic_constraints([
                            algebraic_constraint::AlgebraicConstraint::assert_zero(expr2),
                        ]);
                        progress = true;
                    } else {
                        log::warn!(
                            "Was about to replace {expr1} but did not find it in the system."
                        );
                    }
                }
                Action::ReplacePairOfAlgebraicConstraintsBy(e1, e2, replacement) => {
                    let expr1 =
                        untransform_grouped_expression(&expr_db.as_ref().unwrap()[e1], &var_mapper);
                    let expr2 =
                        untransform_grouped_expression(&expr_db.as_ref().unwrap()[e2], &var_mapper);
                    let mut found1 = false;
                    let mut found2 = false;
                    for c in system.algebraic_constraints() {
                        if c.expression == expr1 {
                            found1 = true;
                        } else if c.expression == expr2 {
                            found2 = true;
                        }
                    }
                    if found1 && found2 {
                        system.retain_algebraic_constraints(|c| {
                            c.expression != expr1 && c.expression != expr2
                        });
                        let replacement = untransform_grouped_expression(
                            &expr_db.as_ref().unwrap()[replacement],
                            &var_mapper,
                        );
                        assert!(replacement.degree() <= expr1.degree().max(expr2.degree()));
                        system.add_algebraic_constraints([
                            algebraic_constraint::AlgebraicConstraint::assert_zero(replacement),
                        ]);
                        progress = true;
                    } else {
                        log::warn!(
                            "Was about to replace {expr1} and {expr2} but did not find them in the system."
                        );
                    }
                }
            }
        }
        if !progress {
            break;
        }
    }
    system.retain_algebraic_constraints(|c| !c.is_redundant());
    (system, additional_algebraic_constraints)
}

fn transform_constraint_system<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    system: &IndexedConstraintSystem<T, V>,
    var_mapper: &ItemDB<V, Var>,
    expression_db: &mut ItemDB<GroupedExpression<T, Var>, Expr>,
) -> (Vec<Expr>, Vec<BusInteraction<Expr>>) {
    let algebraic_constraints = system
        .system()
        .algebraic_constraints
        .iter()
        .map(|c| transform_grouped_expression(&c.expression, var_mapper))
        .map(|e| expression_db.insert_owned(e))
        .collect_vec();
    let bus_interactions: Vec<BusInteraction<Expr>> = system
        .system()
        .bus_interactions
        .iter()
        .map(|bus_inter| {
            bus_inter
                .fields()
                .map(|f| transform_grouped_expression(f, var_mapper))
                .map(|e| expression_db.insert_owned(e))
                .collect()
        })
        .collect_vec();
    (algebraic_constraints, bus_interactions)
}

fn transform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, Var> {
    expr.transform_var_type(&mut |v| var_mapper.id(v))
}

fn untransform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, Var>,
    var_mapper: &ItemDB<V, Var>,
) -> GroupedExpression<T, V> {
    expr.transform_var_type(&mut |v| var_mapper[*v].clone())
}

fn untransform_computation_method<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    method: &ComputationMethod<T, GroupedExpression<T, Var>>,
    var_mapper: &ItemDB<V, Var>,
) -> ComputationMethod<T, GroupedExpression<T, V>> {
    match method {
        ComputationMethod::Constant(c) => ComputationMethod::Constant(*c),
        ComputationMethod::QuotientOrZero(numerator, denominator) => {
            ComputationMethod::QuotientOrZero(
                untransform_grouped_expression(numerator, var_mapper),
                untransform_grouped_expression(denominator, var_mapper),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use powdr_constraint_solver::{
        algebraic_constraint, constraint_system::DefaultBusInteractionHandler,
        grouped_expression::NoRangeConstraints,
    };
    use powdr_number::{BabyBearField, LargeInt};

    use super::*;

    fn assert_zero<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
        expr: GroupedExpression<T, V>,
    ) -> algebraic_constraint::AlgebraicConstraint<GroupedExpression<T, V>> {
        algebraic_constraint::AlgebraicConstraint::assert_zero(expr)
    }

    fn v(name: &str) -> GroupedExpression<BabyBearField, String> {
        GroupedExpression::from_unknown_variable(name.to_string())
    }

    fn c(value: i64) -> GroupedExpression<BabyBearField, String> {
        GroupedExpression::from_number(BabyBearField::from(value))
    }

    fn new_var() -> impl FnMut(&str) -> String {
        let mut counter = 0;
        move |prefix: &str| {
            let name = format!("{prefix}_{counter}");
            counter += 1;
            name
        }
    }

    fn handle_variable_range_checker<T: FieldElement>(
        payload: &[RangeConstraint<T>],
    ) -> Vec<RangeConstraint<T>> {
        const MAX_BITS: u64 = 25;
        // See: https://github.com/openvm-org/openvm/blob/v1.0.0/crates/circuits/primitives/src/var_range/bus.rs
        // Expects (x, bits), where `x` is in the range [0, 2^bits - 1]
        let [_x, bits] = payload else {
            panic!("Expected arguments (x, bits)");
        };
        match bits.try_to_single_value() {
            Some(bits_value) if bits_value.to_degree() <= MAX_BITS => {
                let bits_value = bits_value.to_integer().try_into_u64().unwrap();
                let mask = (1u64 << bits_value) - 1;
                vec![RangeConstraint::from_mask(mask), *bits]
            }
            _ => {
                vec![
                    RangeConstraint::from_mask((1u64 << MAX_BITS) - 1),
                    RangeConstraint::from_range(T::from(0), T::from(MAX_BITS)),
                ]
            }
        }
    }

    fn try_handle_bus_interaction<T: FieldElement>(
        bus_interaction: &BusInteraction<RangeConstraint<T>>,
    ) -> Option<BusInteraction<RangeConstraint<T>>> {
        let mult = bus_interaction.multiplicity.try_to_single_value()?;
        if mult == Zero::zero() {
            return None;
        }
        let bus_id = bus_interaction
            .bus_id
            .try_to_single_value()?
            .to_integer()
            .try_into_u64()?;
        let payload_constraints = match bus_id {
            3 => handle_variable_range_checker(&bus_interaction.payload),
            _ => return None,
        };
        Some(BusInteraction {
            payload: payload_constraints,
            ..bus_interaction.clone()
        })
    }

    #[derive(Clone)]
    struct TestBusInteractionHandler;

    impl<T: FieldElement> BusInteractionHandler<T> for TestBusInteractionHandler {
        fn handle_bus_interaction(
            &self,
            bus_interaction: BusInteraction<RangeConstraint<T>>,
        ) -> BusInteraction<RangeConstraint<T>> {
            try_handle_bus_interaction(&bus_interaction).unwrap_or(bus_interaction)
        }
    }

    fn bit_constraint(
        variable: &str,
        bits: u32,
    ) -> BusInteraction<GroupedExpression<BabyBearField, String>> {
        BusInteraction {
            bus_id: c(3),
            payload: vec![v(variable), c(bits as i64)],
            multiplicity: c(1),
        }
    }

    #[test]
    fn test_rule_based_optimization_empty() {
        let system: IndexedConstraintSystem<BabyBearField, String> =
            IndexedConstraintSystem::default();
        let optimized_system = rule_based_optimization(
            system,
            NoRangeConstraints,
            DefaultBusInteractionHandler::default(),
            &mut new_var(),
            None,
        );
        assert_eq!(optimized_system.0.system().algebraic_constraints.len(), 0);
    }

    #[test]
    fn test_rule_based_optimization_simple_assignment() {
        let mut system = IndexedConstraintSystem::default();
        let x = v("x");
        system.add_algebraic_constraints([
            assert_zero(x * BabyBearField::from(7) - c(21)),
            assert_zero(v("y") * (v("y") - c(1)) - v("x")),
        ]);
        let optimized_system = rule_based_optimization(
            system,
            NoRangeConstraints,
            DefaultBusInteractionHandler::default(),
            &mut new_var(),
            None,
        );
        expect!["(y) * (y - 1) - 3 = 0"].assert_eq(&optimized_system.0.to_string());
    }

    #[test]
    fn test_rule_based_optimization_quadratic_equality() {
        let mut system = IndexedConstraintSystem::default();
        system.add_algebraic_constraints([
            assert_zero(
                (c(30720) * v("rs1_data__0_1") + c(7864320) * v("rs1_data__1_1")
                    - c(30720) * v("mem_ptr_limbs__0_1")
                    + c(737280))
                    * (c(30720) * v("rs1_data__0_1") + c(7864320) * v("rs1_data__1_1")
                        - c(30720) * v("mem_ptr_limbs__0_1")
                        + c(737281)),
            ),
            assert_zero(
                (c(30720) * v("rs1_data__0_1") + c(7864320) * v("rs1_data__1_1")
                    - c(30720) * v("mem_ptr_limbs__0_2")
                    + c(737280))
                    * (c(30720) * v("rs1_data__0_1") + c(7864320) * v("rs1_data__1_1")
                        - c(30720) * v("mem_ptr_limbs__0_2")
                        + c(737281)),
            ),
        ]);
        system.add_bus_interactions([
            bit_constraint("rs1_data__0_1", 8),
            bit_constraint("rs1_data__1_1", 8),
            BusInteraction {
                bus_id: c(3),
                multiplicity: c(1),
                payload: vec![c(-503316480) * v("mem_ptr_limbs__0_1"), c(14)],
            },
            BusInteraction {
                bus_id: c(3),
                multiplicity: c(1),
                payload: vec![c(-503316480) * v("mem_ptr_limbs__0_2"), c(14)],
            },
        ]);
        let optimized_system = rule_based_optimization(
            system,
            NoRangeConstraints,
            TestBusInteractionHandler,
            &mut new_var(),
            None,
        );
        // Note that in the system below, mem_ptr_limbs__0_2 has been eliminated
        expect![[r#"
            (30720 * mem_ptr_limbs__0_1 - 30720 * rs1_data__0_1 - 7864320 * rs1_data__1_1 - 737280) * (30720 * mem_ptr_limbs__0_1 - 30720 * rs1_data__0_1 - 7864320 * rs1_data__1_1 - 737281) = 0
            (30720 * mem_ptr_limbs__0_2 - 30720 * rs1_data__0_1 - 7864320 * rs1_data__1_1 - 737280) * (30720 * mem_ptr_limbs__0_2 - 30720 * rs1_data__0_1 - 7864320 * rs1_data__1_1 - 737281) = 0
            BusInteraction { bus_id: 3, multiplicity: 1, payload: rs1_data__0_1, 8 }
            BusInteraction { bus_id: 3, multiplicity: 1, payload: rs1_data__1_1, 8 }
            BusInteraction { bus_id: 3, multiplicity: 1, payload: -(503316480 * mem_ptr_limbs__0_1), 14 }
            BusInteraction { bus_id: 3, multiplicity: 1, payload: -(503316480 * mem_ptr_limbs__0_2), 14 }"#]].assert_eq(&optimized_system.0.to_string());
    }
}
