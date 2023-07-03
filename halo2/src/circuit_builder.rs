use ast::parsed::BinaryOperator;
use num_bigint::BigUint;
use polyexen::expr::{ColumnKind, ColumnQuery, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{
    ColumnFixed, ColumnWitness, Columns, Info, Lookup, Plaf, Poly, Shuffle, Witness,
};

use ast::analyzed::{Analyzed, Expression, IdentityKind, SelectedExpressions};
use num_traits::One;
use number::{BigInt, FieldElement};

use super::circuit_data::CircuitData;

pub(crate) fn analyzed_to_circuit<T: FieldElement>(
    analyzed: &Analyzed<T>,
    fixed: &[(&str, Vec<T>)],
    witness: &Vec<(&str, Vec<T>)>,
) -> PlafH2Circuit {
    // The structure of the table is as following
    //
    // | constant columns | __enable_cur | __enable_next |  witness columns | \
    // |  c[0]            |    1         |       1       |   w[0]           |  |
    // |  c[1]            |    1         |       1       |   w[1]           |  |>  N actual circuit rows
    // |  ...             |   ...        |      ...      |   ...            |  |
    // |  c[N - 2]        |    1         |       1       |   w[N - 2]       |  |
    // |  c[N - 1]        |    1         |       0       |   w[N - 1]       | /     <- __enable_next == 0 since there's no state transition
    // |  0               |    0         |       0       |   0              | \
    // |  0               |    0         |       0       |   0              |  |
    // |  ...             |   ...        |      ...      |   ...            |  |>  (2**(ceil(log2(N)) + 1) - N) padding rows to fit the halo2 unusable rows
    // |  0               |    0         |       0       |   0              |  |
    // |  0               |    0         |       0       |   <unusable>     |  |
    // |  0               |    0         |       0       |   <unusable>     | /

    // generate fixed and witness (witness).

    let query = |column, rotation| Expr::Var(PlonkVar::Query(ColumnQuery { column, rotation }));

    let mut cd = CircuitData::from(fixed.to_owned(), witness.to_owned());

    // append two fixed columns:
    // - one that enables constraints that do not have rotations (__enable_cur) in the actual circuit
    // - and another that enables constraints that have a rotation (__enable_next) in the actual circuit except in the last actual row

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
    let mut shuffles = vec![];
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

    let apply_selectors_to_set = |set: &SelectedExpressions<T>| {
        let selector = set
            .selector
            .clone()
            .map_or(Expr::Const(BigUint::one()), |expr| {
                expression_2_expr(&cd, &expr)
            });

        let contains_next_ref = set.expressions.iter().any(|exp| exp.contains_next_ref());

        let selector = Expr::Mul(vec![
            selector,
            if contains_next_ref {
                q_enable_next.clone()
            } else {
                q_enable_cur.clone()
            },
        ]);

        set.expressions
            .iter()
            .map(|expr| selector.clone() * expression_2_expr(&cd, expr))
            .collect()
    };

    for id in &analyzed.identities {
        match id.kind {
            IdentityKind::Polynomial => {
                // polynomial identities.

                assert_eq!(id.right.expressions.len(), 0);
                assert_eq!(id.right.selector, None);
                assert_eq!(id.left.expressions.len(), 0);

                let exp = id.left.selector.as_ref().unwrap();
                let contains_next_ref = exp.contains_next_ref();

                let exp = expression_2_expr(&cd, exp);

                // depending whether this polynomial contains a rotation,
                // enable for all rows or all except the last one.

                let exp = Expr::Mul(vec![
                    exp,
                    if contains_next_ref {
                        q_enable_next.clone()
                    } else {
                        q_enable_cur.clone()
                    },
                ]);
                polys.push(Poly {
                    name: "".to_string(),
                    exp,
                });
            }
            IdentityKind::Plookup => {
                let left = apply_selectors_to_set(&id.left);
                let right = apply_selectors_to_set(&id.right);

                lookups.push(Lookup {
                    name: "".to_string(),
                    exps: (left, right),
                });
            }
            IdentityKind::Permutation => {
                let left = apply_selectors_to_set(&id.left);
                let right = apply_selectors_to_set(&id.right);

                shuffles.push(Shuffle {
                    name: "".to_string(),
                    exps: (left, right),
                });
            }
            _ => unimplemented!(),
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
        metadata: Default::default(),
        lookups,
        shuffles,
        copys,
        fixed,
    };

    // return circuit description + witness. -------------

    PlafH2Circuit { plaf, wit }
}

fn expression_2_expr<T: FieldElement>(cd: &CircuitData<T>, expr: &Expression<T>) -> Expr<PlonkVar> {
    match expr {
        Expression::Number(n) => Expr::Const(n.to_arbitrary_integer()),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);

            let plonkvar = PlonkVar::Query(ColumnQuery {
                column: cd.col(&polyref.name),
                rotation: polyref.next as i32,
            });

            Expr::Var(plonkvar)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let lhe = expression_2_expr(cd, lhe);
            let rhe = expression_2_expr(cd, rhe);
            match op {
                BinaryOperator::Add => Expr::Sum(vec![lhe, rhe]),
                BinaryOperator::Sub => Expr::Sum(vec![lhe, Expr::Neg(Box::new(rhe))]),
                BinaryOperator::Mul => Expr::Mul(vec![lhe, rhe]),
                _ => unimplemented!("{:?}", expr),
            }
        }

        _ => unimplemented!("{:?}", expr),
    }
}
