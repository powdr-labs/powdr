#![allow(clippy::iter_over_hash_type)]
#![allow(for_loops_over_fallibles)]
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Index,
};

use itertools::{EitherOrBoth, Itertools};
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, NoRangeConstraints},
    indexed_constraint_system::IndexedConstraintSystem,
    runtime_constant::VarTransformable,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use num_traits::Zero;

use crepe::crepe;
use slab::Slab;

use crate::range_constraint_optimizer::RangeConstraintHandler;

type F = BabyBearField;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Var(u32);

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
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
    fn eq(&self, other: &Self) -> bool {
        // TODO change this as soon as we have different systems
        true
    }
}

impl Eq for System {}

impl PartialOrd for System {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // TODO change this as soon as we have different systems
        Some(std::cmp::Ordering::Equal)
    }
}

impl Ord for System {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
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
    // pub fn get(&self, expr: Expr) -> &GroupedExpression<F, Var> {
    //     let b = self.expressions.borrow();
    //     b.get(expr.0).unwrap()
    // }

    pub fn insert(&self, expr: &GroupedExpression<F, Var>) -> Expr {
        let mut db = self.expressions.borrow_mut();
        let id = if let Some(&id) = db.reverse.get(&expr) {
            id
        } else {
            db.expressions.push(expr.clone());
            let id = db.expressions.len() - 1;
            db.reverse.insert(expr.clone(), id);
            id
        };
        Expr(id)
    }

    // TODO potentially make this a more generic "matches structure" function
    pub fn try_to_simple_quadratic(&self, expr: Expr) -> Option<(Expr, Expr)> {
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

    pub fn try_to_simple_var(&self, expr: Expr) -> Option<Var> {
        let db = self.expressions.borrow();
        db[expr].try_to_simple_unknown()
    }

    /// Returns Some(C) if `a - b = C' and both are affine.
    pub fn constant_difference(&self, a: Expr, b: Expr) -> Option<F> {
        println!(
            "Checking constant difference between {} and {}",
            self.format_expr(a),
            self.format_expr(b)
        );
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

    /// Checks if a and b are affine constraints that differ in exactly one variable.
    /// TODO scaling
    pub fn differ_in_exactly_one_variable(&self, a: Expr, b: Expr) -> Option<(Var, Var)> {
        let db = self.expressions.borrow();
        let a = &db[a];
        let b = &db[b];
        if !a.is_affine() || !b.is_affine() {
            return None;
        }
        if a.constant_offset() != b.constant_offset() {
            return None;
        }
        // TODO use merge_join_by? (avoid creating the HashSet)
        let left_vars = a
            .linear_components()
            .map(|(v, _)| *v)
            .collect::<HashSet<_>>();
        let right_vars = b
            .linear_components()
            .map(|(v, _)| *v)
            .collect::<HashSet<_>>();
        let left_var = left_vars.difference(&right_vars).exactly_one().ok()?;
        let right_var = right_vars.difference(&left_vars).exactly_one().ok()?;
        if a.coefficient_of_variable_in_affine_part(left_var)
            != b.coefficient_of_variable_in_affine_part(right_var)
        {
            return None;
        }
        Some((*left_var, *right_var))
    }

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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Expr(usize);

crepe! {
    @input
    struct S<'a>(&'a System);

    @input
    struct AlgebraicConstraint(Expr);

    // @input
    // struct BusInteractionConstraint<'a>(&'a BusInteraction<GroupedExpression<F, Var>>);

    @input
    struct RangeConstraintOnExpression(Expr, F, F);


    @output
    struct RangeConstraint(Var, F, F);
    RangeConstraint(v, min, max) <-
      RangeConstraintOnExpression(e, min, max),
      S(sys),
      let Some(v) = sys.try_to_simple_var(e);

    // struct Product(Expr, Expr, Expr);
    // Product(e, l, r) <-
    //   AlgebraicConstraint(e),
    //   S(sys),
    //   let Some((l, r)) = sys.try_to_simple_quadratic(e);

    // (E, expr, offset) <-> E = (expr) * (expr + offset) is a constraint
    struct QuadraticEquivalenceCandidate(Expr, Expr, F);
    QuadraticEquivalenceCandidate(e, r, offset) <-
       AlgebraicConstraint(e),
       S(sys),
       let Some((l, r)) = sys.try_to_simple_quadratic(e),
       let Some(offset) = sys.constant_difference(l, r);

    struct QuadraticEquivalence(Var, Var);
    QuadraticEquivalence(v1, v2) <-
      QuadraticEquivalenceCandidate(_, expr1, offset),
      QuadraticEquivalenceCandidate(_, expr2, offset),
      S(sys),
      let Some((v1, v2)) = sys.differ_in_exactly_one_variable(expr1, expr2),
      RangeConstraint(v1, min, max),
      RangeConstraint(v2, min, max),
      (min + offset >= max); // TODO not exactly


    // TODO wait a second. We can craete range constraints on expressions for all
    // algebraic constraints. Then we just work on range constraints on expressions
    // instead of algebraic constraints. Might be more difficult with the scaling, though.
    // RangeConstraint(v, x1, x1 + F::from(1)) <-
    //     AlgebraicConstraint(e),
    //     Product(e, l, r),
    //     Solvable(l, v, x1),
    //     Solvable(r, v, x1 + F::from(1));

    // struct IsZero<'a>(&'a GroupedExpression<F, Var>);
    // IsZero(e) <- Expression(e), (e.is_zero());

    // struct IsAffine<'a>(&'a GroupedExpression<F, Var>);
    // IsAffine(e) <- Expression(e), (e.is_affine());

    // struct AffineExpression<'a>(&'a GroupedExpression<F, Var>, F, Var, F);
    // AffineExpression(e, coeff, var, offset) <-
    //   DestructureAffine(e, coeff, var, rest),
    //   for offset in rest.try_to_number();

    // struct Solvable<'a>(&'a GroupedExpression<F, Var>, Var, F);
    // Solvable(e, var, -offset / coeff) <-
    //   AffineExpression(e, coeff, var, offset);

    @output
    struct Assignment(Var, F);
    // Assignment(var, v) <- AlgebraicConstraint(e), Solvable(e, var, v);

    @output
    struct Equivalence(Var, Var);
    Equivalence(v1, v2) <- QuadraticEquivalence(v1, v2);

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
                    RangeConstraintOnExpression(
                        *expr,
                        BabyBearField::from(min.to_arbitrary_integer()),
                        BabyBearField::from(max.to_arbitrary_integer()),
                    )
                })
        })
        .collect_vec();

    let transform_end = std::time::Instant::now();

    rt.extend(
        transformed_expressions
            .iter()
            .copied()
            .map(AlgebraicConstraint),
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

    let (rcs, assignments, equivalences) = rt.run();
    let run_end = std::time::Instant::now();
    // for RangeConstraint(var, min, max) in rcs {
    //     log::info!(
    //         "Rule-based range constraint: {} in [{}, {}]",
    //         var_mapper.backward(&var),
    //         min,
    //         max
    //     );
    // }
    println!("Found {} rule-based RCs", rcs.len());
    println!("Found {} rule-based assignments", assignments.len());
    println!("Found {} rule-based equivalences", equivalences.len());
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
        // log::info!("Rule-based assignment: {var} = {value}",);
        system.substitute_by_known(var, &value);
    }
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

/// Returns a set of expressions that are missing a single affine component.
fn extract_single_vars(
    expr: &GroupedExpression<F, Var>,
) -> HashSet<(GroupedExpression<F, Var>, F, Var, GroupedExpression<F, Var>)> {
    let mut result = expr
        .quadratic_components()
        .iter()
        .flat_map(|(l, r)| {
            extract_single_vars(l)
                .into_iter()
                .chain(extract_single_vars(r).into_iter())
        })
        .collect::<HashSet<_>>();
    result.extend(expr.linear_components().map(|(v, c)| {
        (
            expr.clone(),
            *c,
            *v,
            expr.clone() - GroupedExpression::from_unknown_variable(v.clone()) * (*c),
        )
    }));
    result
}
