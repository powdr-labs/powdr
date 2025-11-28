use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    sync::Arc,
};

use egglog::{
    add_primitive,
    ast::{Literal, RustSpan},
    prelude::BaseSort,
    sort::{BaseValues, BigIntSort},
    BaseValue, EGraph, Term, TermDag, Value,
};
use itertools::Itertools;
use powdr_constraint_solver::{
    constraint_system::{BusInteraction, BusInteractionHandler},
    grouped_expression::{GroupedExpression, GroupedExpressionComponent, NoRangeConstraints},
    indexed_constraint_system::IndexedConstraintSystem,
};
use powdr_number::{BabyBearField, FieldElement, LargeInt};

use num_traits::Zero;

use crate::range_constraint_optimizer::RangeConstraintHandler;

type F = BabyBearField;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Var(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FE(BabyBearField);

impl BaseValue for FE {
    // TODO we should avoid boxing
}

impl BaseSort for FE {
    type Base = FE;

    fn name(&self) -> &str {
        "FieldElement"
    }

    #[rustfmt::skip]
    fn register_primitives(&self, eg: &mut EGraph) {
        add_primitive!(eg, "fe" = |a: i64| -> FE { FE(a.into()) });

        add_primitive!(eg, "+" = |a: FE, b: FE| -> FE { FE(a.0 + b.0) });
        add_primitive!(eg, "-" = |a: FE, b: FE| -> FE { FE(a.0 - b.0) });
        add_primitive!(eg, "*" = |a: FE, b: FE| -> FE { FE(a.0 * b.0) });
        add_primitive!(eg, "/" = |a: FE, b: FE| -?> FE { (!b.0.is_zero()).then(|| FE(a.0 / b.0) ) });
        // add_primitive!(eg, "%" = |a: FE, b: FE| -?> FE { (*b != BigInt::FEERO).then(|| a % b) });

        // add_primitive!(eg, "&" = |a: FE, b: FE| -> FE { a & b });
        // add_primitive!(eg, "|" = |a: FE, b: FE| -> FE { a | b });
        // add_primitive!(eg, "^" = |a: FE, b: FE| -> FE { a ^ b });
        // add_primitive!(eg, "<<" = |a: FE, b: i64| -> FE { (&*a).shl(b).into() });
        // add_primitive!(eg, ">>" = |a: FE, b: i64| -> FE { (&*a).shr(b).into() });
        // add_primitive!(eg, "not-FE" = |a: FE| -> FE { FE::new(!&*a) });

        // add_primitive!(eg, "bits" = |a: FE| -> FE { FE::new(a.bits().into()) });

        // add_primitive!(eg, "<" = |a: FE, b: FE| -?> () { (a < b).then_some(()) });
        // add_primitive!(eg, ">" = |a: FE, b: FE| -?> () { (a > b).then_some(()) });
        // add_primitive!(eg, "<=" = |a: FE, b: FE| -?> () { (a <= b).then_some(()) });
        // add_primitive!(eg, ">=" = |a: FE, b: FE| -?> () { (a >= b).then_some(()) });

        // add_primitive!(eg, "bool-=" = |a: FE, b: FE| -> bool { a == b });
        // add_primitive!(eg, "bool-<" = |a: FE, b: FE| -> bool { a < b });
        // add_primitive!(eg, "bool->" = |a: FE, b: FE| -> bool { a > b });
        // add_primitive!(eg, "bool-<=" = |a: FE, b: FE| -> bool { a <= b });
        // add_primitive!(eg, "bool->=" = |a: FE, b: FE| -> bool { a >= b });

        // add_primitive!(eg, "min" = |a: FE, b: FE| -> FE { a.min(b) });
        // add_primitive!(eg, "max" = |a: FE, b: FE| -> FE { a.max(b) });

        add_primitive!(eg, "to-string" = |a: FE| -> egglog::sort::S { egglog::sort::S::new(a.0.to_string()) });
        // add_primitive!(eg, "from-string" = |a: S| -?> FE {
        //     a.as_str().parse::<BigInt>().ok().map(FE::new)
        // });
    }

    fn reconstruct_termdag(
        &self,
        base_values: &BaseValues,
        value: Value,
        termdag: &mut TermDag,
    ) -> Term {
        let fe = base_values.unwrap::<FE>(value);

        let as_string = termdag.lit(Literal::String(fe.0.to_string()));
        termdag.app("from-string".to_owned(), vec![as_string])
    }
}

const RULES: &str = r#"
(datatype Expr
  (Add Expr Expr)
  (Mul Expr Expr)
  (Num FieldElement)
  (Var String)
)

;;(rewrite (Add x y) (Add y x))
;;(rewrite (Mul x y) (Mul y x))
;;(rewrite (Add (Add x y) z) (Add x (Add y z)))
;;(rewrite (Mul (Mul x y) z) (Mul x (Mul y z)))
(rewrite (Add (Num x) (Num y)) (Num (+ x y)))
(rewrite (Mul (Num x) (Num y)) (Num (* x y)))

(rewrite (Add (Mul y x) (Mul z x)) (Mul (Add y z) x))
(rewrite (Add (Mul x y) (Mul z x)) (Mul (Add y z) x))
(rewrite (Add (Mul y x) (Mul x z)) (Mul (Add y z) x))
(rewrite (Add (Mul x y) (Mul x z)) (Mul (Add y z) x))

(rewrite (Add x (Num (fe 0))) x)
(rewrite (Add (Num (fe 0)) x) x)
(rewrite (Mul x (Num (fe 0))) (Num (fe 0)))
(rewrite (Mul (Num (fe 0)) x) (Num (fe 0)))
(rewrite (Mul x (Num (fe 1))) x)
(rewrite (Mul (Num (fe 1)) x) x)

(datatype Constraint
  (Constr Expr)
)

;; If we add this rule, all constraints will be replaced by zero.
;; (rule ((Constr e)) ((union e (Num (fe 0)))))

(rule (
    (Constr (Var x))
  )
  (
    (union (Var x) (Num (fe 0)))
  )
)

(rule (
    (Constr (Add
      (Mul (Num coeff) (Var v))
      (Num offset)
    ))
    (!= coeff (fe 0))
  )
  (
    (union (Var v) (Num (/ (- (fe 0) offset) coeff)))
  )
)
"#;

const RUN: &str = r#"
;;(let c1 (Constr (Add (Mul (Num (fe 7)) (Var "x")) (Num (fe 9))) ) )

;;(run-schedule (saturate (run)))

;; 8 does max out memory
(run 8)

;; (relation RangeConstraint (Expr fe fe))
;; (rule ((RangeConstraint (Add e (Num x)) lower upper)) ((RangeConstraint e (- lower x) (- upper x))))
;;(extract c1)

"#;

pub fn rule_based_optimization<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    mut system: IndexedConstraintSystem<T, V>,
    bus_interaction_handler: impl BusInteractionHandler<T> + Clone,
) -> IndexedConstraintSystem<T, V> {
    let mut egraph = EGraph::default();
    egglog::prelude::add_base_sort(
        &mut egraph,
        FE(BabyBearField::zero()),
        egglog::ast::Span::Rust(Arc::new(RustSpan {
            file: "rule_based_optimizer.rs",
            line: 0,
            column: 0,
        })),
    )
    .unwrap();
    egraph.parse_and_run_program(None, RULES).unwrap();
    for (i, algebraic_constr) in system.algebraic_constraints().iter().enumerate() {
        println!(
            "(let c_{i} (Constr {}))",
            format_expr(&algebraic_constr.expression)
        );
    }
    for (i, algebraic_constr) in system.algebraic_constraints().iter().enumerate() {
        egraph
            .parse_and_run_program(
                None,
                &format!(
                    "(let c_{i} (Constr {}))\n",
                    format_expr(&algebraic_constr.expression)
                ),
            )
            .unwrap();
    }
    egraph.parse_and_run_program(None, RUN).unwrap();
    for (i, algebraic_constr) in system.algebraic_constraints().iter().enumerate() {
        println!("Original: {}", format_expr(&algebraic_constr.expression));
        egraph
            .parse_and_run_program(None, &format!("(extract c_{i})\n",))
            .unwrap();
    }
    panic!();
    system
}

fn format_expr<T: FieldElement, V: Hash + Eq + Ord + Clone + Display>(
    expr: &GroupedExpression<T, V>,
) -> String {
    expr.clone()
        .into_summands()
        .map(|comp| match comp {
            GroupedExpressionComponent::Quadratic(l, r) => {
                format!("(Mul {} {})", format_expr(&l), format_expr(&r))
            }
            GroupedExpressionComponent::Linear(var, coeff) => {
                format!("(Mul (Var \"{var}\") (Num (fe {coeff})))",)
            }
            GroupedExpressionComponent::Constant(v) => format!("(Num (fe {v}))"),
        })
        .reduce(|a, b| format!("(Add {} {})", a, b))
        .unwrap_or("(Num (fe 0))".to_string())
}
