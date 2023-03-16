use num_bigint::BigUint;
use polyexen::expr::{ColumnKind, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{ColumnFixed, ColumnWitness, Columns, Info, Lookup, Plaf, Poly, Witness};

use num_traits::One;
use number::{BigInt, FieldElement};
use pil_analyzer::{self, BinaryOperator, Expression, IdentityKind};

use super::circuit_data::CircuitData;

pub(crate) fn analyzed_to_circuit<T: FieldElement>(
    analyzed: &pil_analyzer::Analyzed<T>,
    query_callback: Option<impl FnMut(&str) -> Option<T>>,
) -> PlafH2Circuit {
    // The structure of the table is as following
    //
    // | constant columns | __enable_cur | __enable_next |  witness columns | \
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |>  witness + fixed  2^(k-1)
    // |  ...             |   ...        |      ...      |   ...            |  |
    // |  bla_bla_bla     |    1         |       0       |   bla_bla_bla    | /     <- __enable_next == 0 since there's no state transition
    // |  0               |    0         |       0       |   0              | \
    // |  0               |    0         |       0       |   0              |  |
    // |  ...             |   ...        |      ...      |   ...            |  |> 2^2-1
    // |  0               |    0         |       0       |   <unusable>     |  |
    // |  0               |    0         |       0       |   <unusable>     | /

    // generate fixed and witness (witness).

    let query = |column, rotation| Expr::Var(PlonkVar::ColumnQuery { column, rotation });

    let (fixed, degree) = executor::constant_evaluator::generate(analyzed);
    let witness = executor::witgen::generate(analyzed, degree, &fixed, query_callback);

    let mut cd = CircuitData::from(fixed, witness);

    // append to fixed columns:
    // - one that enables constraints that do not have rotations
    // - and another that enables constraints that have a rotation
    // (note this is not activated) in last row.

    let num_rows = cd.len();

    let q_enable_cur = query(
        cd.insert_constant("__enable_cur", itertools::repeat_n(T::from(1), num_rows)),
        0,
    );

    let q_enable_next = query(
        cd.insert_constant(
            "__enable_next",
            itertools::repeat_n(T::from(1), num_rows - 1).chain(std::iter::once(T::from(0))),
        ),
        0,
    );

    let mut lookups = vec![];
    let mut polys = vec![];

    // build Plaf columns -------------------------------------------------

    let columns = Columns {
        fixed: cd
            .fixed
            .iter()
            .map(|(name, _)| ColumnFixed::new(name.to_string()))
            .collect(),
        witness: cd
            .witness
            .iter()
            .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
            .collect(),
        public: vec![],
    };

    // build Plaf info. -------------------------------------------------------------------------

    let info = Info {
        p: T::modulus().to_arbitrary_integer(),
        num_rows: cd.len(),
        challenges: vec![],
    };

    // build Plaf polys. -------------------------------------------------------------------------

    for id in &analyzed.identities {
        if id.kind == IdentityKind::Polynomial {
            // polynomial identities.

            assert_eq!(id.right.expressions.len(), 0);
            assert_eq!(id.right.selector, None);
            assert_eq!(id.left.expressions.len(), 0);

            let (exp, is_next) = expression_2_expr(&cd, id.left.selector.as_ref().unwrap());

            // depending whether this polynomial contains a rotation,
            // enable for all rows or all except the last one.

            let exp = Expr::Mul(vec![
                exp,
                if is_next {
                    q_enable_next.clone()
                } else {
                    q_enable_cur.clone()
                },
            ]);
            polys.push(Poly {
                name: "".to_string(),
                exp,
            });
        } else if id.kind == IdentityKind::Plookup {
            // lookups.

            assert_eq!(id.right.selector, None);

            let left_selector = id
                .left
                .selector
                .clone()
                .map_or(Expr::Const(BigUint::one()), |expr| {
                    expression_2_expr(&cd, &expr).0
                });

            let left = id
                .left
                .expressions
                .iter()
                .map(|expr| left_selector.clone() * expression_2_expr(&cd, expr).0)
                .collect();

            let right = id
                .right
                .expressions
                .iter()
                .map(|expr| expression_2_expr(&cd, expr).0)
                .collect();

            lookups.push(Lookup {
                name: "".to_string(),
                exps: (left, right),
            });
        } else {
            unimplemented!()
        }
    }

    // build Plaf fixed. -------------------------------------------------------------------------

    let fixed: Vec<Vec<_>> = cd
        .fixed
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(value.to_arbitrary_integer()))
                .collect()
        })
        .collect();

    // build witness. -------------------------------------------------------------------------

    let witness: Vec<Vec<_>> = cd
        .witness
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(value.to_arbitrary_integer()))
                .collect()
        })
        .collect();

    let witness_cols = cd
        .witness
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Fixed, n)));

    let wit = Witness {
        num_rows: cd.witness.len(),
        columns: witness_cols
            .map(|(name, _)| ColumnWitness::new(name, 0))
            .collect(),
        witness,
    };

    let copys = vec![];

    // build plaf. -------------------------------------------------------------------------

    let plaf = Plaf {
        info,
        columns,
        polys,
        lookups,
        copys,
        fixed,
    };

    // return circuit description + witness. -------------

    PlafH2Circuit { plaf, wit }
}

fn expression_2_expr<T: FieldElement>(
    cd: &CircuitData<T>,
    expr: &Expression<T>,
) -> (Expr<PlonkVar>, bool) {
    match expr {
        Expression::Number(n) => (Expr::Const(n.to_arbitrary_integer()), false),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);

            let plonkvar = PlonkVar::ColumnQuery {
                column: cd.col(&polyref.name),
                rotation: polyref.next as i32,
            };

            (Expr::Var(plonkvar), polyref.next)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (lhe, lhe_rot) = expression_2_expr(cd, lhe);
            let (rhe, rhe_rot) = expression_2_expr(cd, rhe);
            let res = match op {
                BinaryOperator::Add => Expr::Sum(vec![lhe, rhe]),
                BinaryOperator::Sub => Expr::Sum(vec![lhe, Expr::Neg(Box::new(rhe))]),
                BinaryOperator::Mul => Expr::Mul(vec![lhe, rhe]),
                _ => unimplemented!("{:?}", expr),
            };
            (res, std::cmp::max(lhe_rot, rhe_rot))
        }

        _ => unimplemented!("{:?}", expr),
    }
}
