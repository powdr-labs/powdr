#![allow(clippy::iter_over_hash_type)]
// This is about a warning about interior mutability for the key
// `S`. We need it and it is probably fine.
#![allow(clippy::mutable_key_type)]
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    fmt::Display,
    hash::Hash,
    ops::Index,
};

use itertools::{EitherOrBoth, Itertools};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, NoRangeConstraints},
    indexed_constraint_system::IndexedConstraintSystem,
    range_constraint::RangeConstraint,
    runtime_constant::VarTransformable,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use num_traits::{One, Zero};

use crepe::crepe;

type F = BabyBearField;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Var(u32);

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v_{}", self.0)
    }
}

struct DB {
    expressions: Vec<GroupedExpression<F, Var>>,
    reverse: HashMap<GroupedExpression<F, Var>, usize>,
}

impl Index<Expr> for DB {
    type Output = GroupedExpression<F, Var>;
    fn index(&self, index: Expr) -> &Self::Output {
        &self.expressions[index.0]
    }
}

struct System {
    expressions: RefCell<DB>,
    var_to_string: Option<HashMap<Var, String>>,
}

impl Default for System {
    fn default() -> Self {
        Self {
            expressions: RefCell::new(DB {
                expressions: Vec::new(),
                reverse: HashMap::new(),
            }),
            var_to_string: None,
        }
    }
}

impl PartialEq for System {
    fn eq(&self, _other: &Self) -> bool {
        // TODO change this as soon as we have different systems
        true
    }
}

impl Eq for System {}

impl PartialOrd for System {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for System {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        // TODO change this as soon as we have different systems
        std::cmp::Ordering::Equal
    }
}

impl Hash for System {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO change this as soon as we have different systems
        0.hash(state);
    }
}

impl System {
    pub fn set_var_to_string_mapping(&mut self, mapping: HashMap<Var, String>) {
        assert!(self.var_to_string.is_none());
        self.var_to_string = Some(mapping);
    }

    pub fn insert(&self, expr: &GroupedExpression<F, Var>) -> Expr {
        let mut db = self.expressions.borrow_mut();
        if let Some(&id) = db.reverse.get(expr) {
            Expr(id)
        } else {
            self.insert_owned_new(&mut db, expr.clone())
        }
    }

    pub fn insert_owned(&self, expr: GroupedExpression<F, Var>) -> Expr {
        let mut db = self.expressions.borrow_mut();
        if let Some(&id) = db.reverse.get(&expr) {
            Expr(id)
        } else {
            self.insert_owned_new(&mut db, expr)
        }
    }

    fn insert_owned_new(&self, db: &mut DB, expr: GroupedExpression<F, Var>) -> Expr {
        db.expressions.push(expr.clone());
        let id = db.expressions.len() - 1;
        db.reverse.insert(expr, id);
        Expr(id)
    }

    pub fn referenced_variable(&self, expr: Expr) -> BTreeSet<Var> {
        let db = self.expressions.borrow();
        db[expr].referenced_unknown_variables().cloned().collect()
    }

    pub fn try_to_affine(&self, expr: Expr) -> Option<(F, Var, F)> {
        let db = self.expressions.borrow();
        let expr = &db[expr];
        if !expr.is_affine() {
            return None;
        }
        let (var, coeff) = expr.linear_components().exactly_one().ok()?;
        Some((*coeff, *var, *expr.constant_offset()))
    }

    pub fn is_zero(&self, expr: Expr) -> bool {
        self.expressions.borrow()[expr].is_zero()
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

    #[allow(dead_code)]
    pub fn try_as_single_var(&self, expr: Expr) -> Option<Var> {
        let db = self.expressions.borrow();
        db[expr].try_to_simple_unknown()
    }

    /// Returns Some(C) if `a - b = C' and both are affine.
    pub fn constant_difference(&self, a: Expr, b: Expr) -> Option<F> {
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
    pub fn differ_in_exactly_one_variable(&self, a_id: Expr, b_id: Expr) -> Option<(Var, Var, F)> {
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

    pub fn substitute_by_known(&self, e: Expr, var: Var, value: F) -> Expr {
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
        if let Some(var_to_string) = &self.var_to_string {
            db[expr]
                .transform_var_type(&mut |v| &var_to_string[v])
                .to_string()
        } else {
            db[expr].to_string()
        }
    }

    #[allow(dead_code)]
    pub fn format_var(&self, var: Var) -> String {
        if let Some(var_to_string) = &self.var_to_string {
            var_to_string
                .get(&var)
                .cloned()
                .unwrap_or_else(|| var.to_string())
        } else {
            var.to_string()
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Expr(usize);

crepe! {
    @input
    struct S<'a>(&'a System);

    @input
    struct InitialAlgebraicConstraint(Expr);

    struct AlgebraicConstraint(Expr);
    AlgebraicConstraint(e) <- InitialAlgebraicConstraint(e);

    // @input
    // struct BusInteractionConstraint<'a>(&'a BusInteraction<GroupedExpression<F, Var>>);

    @input
    struct RangeConstraintOnExpression(Expr, RangeConstraint<F>);

    struct Expression(Expr);
    Expression(e) <- AlgebraicConstraint(e);
    Expression(e) <- RangeConstraintOnExpression(e, _);

    struct ContainsVariable(Expr, Var);
    ContainsVariable(e, v) <-
      Expression(e),
      S(sys),
      for v in sys.referenced_variable(e);

    struct AffineExpression(Expr, F, Var, F);
    AffineExpression(e, coeff, var, offset) <-
      Expression(e),
      S(sys),
      let Some((coeff, var, offset)) = sys.try_to_affine(e);

    @output
    struct RangeConstraintOnVar(Var, RangeConstraint<F>);
    // RC(coeff * var + offset) = rc <=>
    // coeff * RC(var) + offset = rc <=>
    // RC(var) = (rc - offset) / coeff
    RangeConstraintOnVar(v, rc.combine_sum(&RangeConstraint::from_value(-offset)).multiple(F::one() / coeff)) <-
      RangeConstraintOnExpression(e, rc),
      AffineExpression(e, coeff, v, offset),
      (coeff != F::zero());

    RangeConstraintOnVar(v, v_rc1.conjunction(&v_rc2)) <-
      RangeConstraintOnVar(v, v_rc1),
      RangeConstraintOnVar(v, v_rc2);

    struct Product(Expr, Expr, Expr);
    Product(e, l, r) <-
      Expression(e),
      S(sys),
      let Some((l, r)) = sys.try_as_single_product(e);
    Product(e, r, l) <- Product(e, l, r);

    // (E, expr, offset) <-> E = (expr) * (expr + offset) is a constraint
    struct QuadraticEquivalenceCandidate(Expr, Expr, F);
    QuadraticEquivalenceCandidate(e, r, offset) <-
       AlgebraicConstraint(e),
       S(sys),
       Product(e, l, r),
       let Some(offset) = sys.constant_difference(l, r);

    struct QuadraticEquivalence(Var, Var);
    QuadraticEquivalence(v1, v2) <-
      QuadraticEquivalenceCandidate(_, expr1, offset),
      QuadraticEquivalenceCandidate(_, expr2, offset),
      S(sys),
      let Some((v1, v2, coeff)) = sys.differ_in_exactly_one_variable(expr1, expr2),
      RangeConstraintOnVar(v1, rc),
      RangeConstraintOnVar(v2, rc),
      (rc.is_disjoint(&rc.combine_sum(&RangeConstraint::from_value(offset / coeff))));


    // TODO wait a second. We can craete range constraints on expressions for all
    // algebraic constraints. Then we just work on range constraints on expressions
    // instead of algebraic constraints. Might be more difficult with the scaling, though.

    struct Solvable(Expr, Var, F);
    Solvable(e, var, -offset / coeff) <-
      AffineExpression(e, coeff, var, offset);

    // Boolean range constraint
    RangeConstraintOnVar(v, RangeConstraint::from_range(x1, x1 + F::from(1))) <-
      AlgebraicConstraint(e),
      Product(e, l, r),
      Solvable(l, v, x1),
      Solvable(r, v, x1 + F::from(1));

    @output
    struct Assignment(Var, F);
    Assignment(var, v) <-
      AlgebraicConstraint(e),
      Solvable(e, var, v);

    @output
    struct Equivalence(Var, Var);
    Equivalence(v1, v2) <- QuadraticEquivalence(v1, v2);

    struct ReplaceAlgebraicConstraintBy(Expr, Expr);
    ReplaceAlgebraicConstraintBy(e, sys.substitute_by_known(e, v, val)) <-
      S(sys),
      AlgebraicConstraint(e),
      ContainsVariable(e, v),
      Assignment(v, val);
    // ReplaceAlgebraicConstraintBy(e, sys.substitute_by_var(e, v, v2)) <-
    //   S(sys),
    //   AlgebraicConstraint(e),
    //   ContainsVariable(e, v),
    //   Equivalence(v, v2);
    AlgebraicConstraint(e) <-
      ReplaceAlgebraicConstraintBy(_, e);


    // This constraint has been replaced by a different one (or is redundant).
    struct AlgebraicConstraintDeleted(Expr);
    AlgebraicConstraintDeleted(e) <-
      ReplaceAlgebraicConstraintBy(e, _);
    AlgebraicConstraintDeleted(e) <-
      S(sys),
      AlgebraicConstraint(e),
      (sys.is_zero(e));

    @output
    struct FinalAlgebraicConstraint(Expr);
    FinalAlgebraicConstraint(e) <- AlgebraicConstraint(e), !AlgebraicConstraintDeleted(e);
}

pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
) -> IndexedConstraintSystem<T, V> {
    if T::modulus().to_arbitrary_integer() != BabyBearField::modulus().to_arbitrary_integer() {
        return system;
    }
    let start = std::time::Instant::now();
    let mut rt = Crepe::new();

    let mut var_mapper = Default::default();
    let mut db = System::default();

    let transformed_expressions = system
        .system()
        .algebraic_constraints
        .iter()
        .map(|c| transform_grouped_expression(&c.expression, &mut var_mapper))
        .map(|e| db.insert(&e))
        .collect_vec();
    let bus_interactions: Vec<BusInteraction<Expr>> = system
        .system()
        .bus_interactions
        .iter()
        .map(|bus_inter| {
            bus_inter
                .fields()
                .map(|f| transform_grouped_expression(f, &mut var_mapper))
                .map(|e| db.insert(&e))
                .collect()
        })
        .collect_vec();

    // TODO we should do that inside the system, but the generic range constraint
    // handler makes it difficult.
    let range_constraints = system
        .system()
        .bus_interactions
        .iter()
        .enumerate()
        .flat_map(|(i, bus_interaction)| {
            let range_constraints = bus_interaction_handler
                .handle_bus_interaction(bus_interaction.to_range_constraints(&NoRangeConstraints))
                .fields()
                .cloned()
                .collect_vec();
            bus_interactions[i]
                .fields()
                .zip_eq(range_constraints)
                .map(|(expr, rc)| {
                    let (min, max) = rc.range();
                    let mask = *rc.mask();
                    let rc = RangeConstraint::from_range(
                        F::from(min.to_arbitrary_integer()),
                        F::from(max.to_arbitrary_integer()),
                    )
                    .conjunction(&RangeConstraint::from_mask(mask.try_into_u64().unwrap()));
                    RangeConstraintOnExpression(*expr, rc)
                })
        })
        .collect_vec();

    let transform_end = std::time::Instant::now();

    rt.extend(
        transformed_expressions
            .iter()
            .copied()
            .map(InitialAlgebraicConstraint),
    );
    // rt.extend(bus_interactions.iter().map(BusInteractionConstraint));
    rt.extend(range_constraints);

    db.set_var_to_string_mapping(
        var_mapper
            .backward
            .iter()
            .map(|(var, v)| (*var, v.to_string()))
            .collect(),
    );
    rt.extend(std::iter::once(S(&db)));

    let insert_end = std::time::Instant::now();

    let (rcs, assignments, equivalences, constrs) = rt.run();
    let run_end = std::time::Instant::now();
    // for RangeConstraint(var, min, max) in &rcs {
    //     println!(
    //         "Rule-based range constraint: {} in [{}, {}]",
    //         var_mapper.backward(&var),
    //         min,
    //         max
    //     );
    // }
    log::debug!("Found {} rule-based RCs", rcs.len());
    log::debug!("Found {} rule-based assignments", assignments.len());
    log::debug!("Found {} rule-based equivalences", equivalences.len());
    log::debug!("Final algebraic constraints: {}", constrs.len());
    // for FinalAlgebraicConstraint(e) in &constrs {
    //     println!("{}", db.format_expr(*e));
    // }
    for (var, value) in assignments
        .into_iter()
        .map(|Assignment(var, value)| {
            (
                var_mapper.backward(&var),
                T::from(value.to_arbitrary_integer()),
            )
        })
        .sorted()
    {
        log::trace!("Rule-based assignment: {var} = {value}",);
        system.substitute_by_known(var, &value);
    }
    for Equivalence(v1, v2) in &equivalences {
        // log::info!("Rule-based equivalence: {v1} == {v2}",);
        let v1 = var_mapper.backward(v1).clone();
        let v2 = var_mapper.backward(v2).clone();
        let (v1, v2) = if v1 < v2 { (v1, v2) } else { (v2, v1) };
        system.substitute_by_unknown(&v2, &GroupedExpression::from_unknown_variable(v1.clone()));
    }
    system.retain_algebraic_constraints(|constraint| !constraint.is_redundant());
    system.retain_bus_interactions(|bus_interaction| !bus_interaction.multiplicity.is_zero());
    let substitution_end = std::time::Instant::now();

    log::info!(
        "Rule-based optimization timings:\n\
           Transform: {}\n\
           Insert: {}\n\
           Run: {}\n\
           Substitution: {}\n\
         Total: {}",
        (transform_end - start).as_secs_f32(),
        (insert_end - transform_end).as_secs_f32(),
        (run_end - insert_end).as_secs_f32(),
        (substitution_end - run_end).as_secs_f32(),
        (substitution_end - start).as_secs_f32(),
    );

    system
}

#[derive(Clone)]
struct VarMapper<V> {
    forward: HashMap<V, Var>,
    backward: HashMap<Var, V>,
    next_id: u32,
}

impl<V> Default for VarMapper<V> {
    fn default() -> Self {
        Self {
            forward: HashMap::new(),
            backward: HashMap::new(),
            next_id: 0,
        }
    }
}

impl<V: Hash + Eq + Clone + Display> VarMapper<V> {
    fn forward(&mut self, v: &V) -> Var {
        if let Some(var) = self.forward.get(v) {
            *var
        } else {
            let var = Var(self.next_id);
            self.forward.insert(v.clone(), var);
            self.backward.insert(var, v.clone());
            self.next_id += 1;
            var
        }
    }

    fn backward(&self, var: &Var) -> &V {
        self.backward.get(var).unwrap()
    }
}

fn transform_grouped_expression<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
    var_mapper: &mut VarMapper<V>,
) -> GroupedExpression<BabyBearField, Var> {
    expr.clone()
        .into_summands()
        .map(|s| match s {
            GroupedExpressionComponent::Quadratic(l, r) => {
                transform_grouped_expression(&l, var_mapper)
                    * transform_grouped_expression(&r, var_mapper)
            }
            GroupedExpressionComponent::Linear(v, c) => {
                GroupedExpression::from_unknown_variable(var_mapper.forward(&v))
                    * BabyBearField::from(c.to_arbitrary_integer())
            }
            GroupedExpressionComponent::Constant(c) => {
                GroupedExpression::from_number(c.to_arbitrary_integer().into())
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use powdr_constraint_solver::{
        algebraic_constraint, constraint_system::DefaultBusInteractionHandler,
    };

    use super::*;

    fn assert_zero<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
        expr: GroupedExpression<T, V>,
    ) -> algebraic_constraint::AlgebraicConstraint<GroupedExpression<T, V>> {
        algebraic_constraint::AlgebraicConstraint::assert_zero(expr)
    }

    fn var(name: &str) -> GroupedExpression<BabyBearField, String> {
        GroupedExpression::from_unknown_variable(name.to_string())
    }

    fn constant(value: i64) -> GroupedExpression<BabyBearField, String> {
        GroupedExpression::from_number(BabyBearField::from(value))
    }

    #[test]
    fn test_rule_based_optimization_empty() {
        let system: IndexedConstraintSystem<BabyBearField, String> =
            IndexedConstraintSystem::default();
        let optimized_system =
            rule_based_optimization(system, DefaultBusInteractionHandler::default());
        assert_eq!(optimized_system.system().algebraic_constraints.len(), 0);
    }

    #[test]
    fn test_rule_based_optimization_simple_assignment() {
        let mut system = IndexedConstraintSystem::default();
        let x = var("x");
        system.add_algebraic_constraints([
            assert_zero(x * F::from(7) - constant(21)),
            assert_zero(var("y") * (var("y") - constant(1)) - var("x")),
        ]);
        let optimized_system =
            rule_based_optimization(system, DefaultBusInteractionHandler::default());
        expect!["(y) * (y - 1) - 3 = 0"].assert_eq(&optimized_system.to_string());
    }
}
