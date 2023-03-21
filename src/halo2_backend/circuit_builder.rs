use std::collections::HashMap;

use num_bigint::{BigInt, BigUint, ToBigInt};
use polyexen::expr::{ColumnKind, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{
    ColumnFixed, ColumnPublic, ColumnPublicValue, ColumnWitness, Columns, Info, Lookup, Plaf, Poly,
    Witness,
};

use crate::analyzer::{BinaryOperator, Expression, FunctionValueDefinition, IdentityKind};
use crate::number::{AbstractNumberType, FIELD_MOD};
use crate::{analyzer, commit_evaluator, constant_evaluator};
use num_integer::Integer;
use num_traits::{One, Zero};

use super::circuit_data::CircuitData;

fn modinv(n: &BigInt, p: &BigInt) -> BigInt {
    let inv = n.extended_gcd(p).x;

    if inv < BigInt::zero() {
        inv + p
    } else {
        inv
    }
}

fn expression_2_expr(
    cd: &CircuitData,
    expr: &Expression,
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> (Expr<PlonkVar>, i32) {
    match expr {
        Expression::Number(n) => (Expr::Const(int_to_field(&n)), 0),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);

            let rotation = if polyref.next { 1 } else { 0 };

            let plonkvar = PlonkVar::ColumnQuery {
                column: cd.col(&polyref.name),
                rotation,
            };

            (Expr::Var(plonkvar), rotation)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (lhe, lhe_rot) = expression_2_expr(cd, lhe, int_to_field);
            let (rhe, rhe_rot) = expression_2_expr(cd, rhe, int_to_field);
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

fn eval_expression_at_row(
    expr: &Expression,
    at_row: usize,
    cd: &CircuitData,
    inputs: &[BigInt],
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> AbstractNumberType {
    match expr {
        Expression::Number(n) => n.clone(),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);
            let column = cd.col(&polyref.name);
            let rotation = if polyref.next { 1 } else { 0 };
            cd.val(&column, at_row + rotation).clone()
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let lhe = eval_expression_at_row(lhe, at_row, cd, inputs, int_to_field);
            let rhe = eval_expression_at_row(rhe, at_row, cd, inputs, int_to_field);
            let value = match op {
                BinaryOperator::Add => lhe + rhe,
                BinaryOperator::Sub => lhe - rhe,
                BinaryOperator::Mul => lhe * rhe,
                _ => unimplemented!("{:?}", expr),
            };

            &value % &*FIELD_MOD
        }
        Expression::Tuple(exprs) => {
            if exprs.len() != 2 || exprs.get(0) != Some(&Expression::String(String::from("input")))
            {
                unimplemented!();
            }
            let index =
                eval_expression_at_row(exprs.get(1).unwrap(), at_row, cd, inputs, int_to_field);
            let index: u64 = index.try_into().unwrap();
            inputs[index as usize].clone()
        }
        _ => unimplemented!("{:?}", expr),
    }
}

pub(crate) fn analyzed_to_circuit(
    analyzed: &analyzer::Analyzed,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    inputs: &[BigInt],
    field_mod: BigUint,
    max_public_inputs: usize,
    verbose: bool,
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> PlafH2Circuit {
    // The stucture of the table is as following
    //
    // | constant columns | __enable_cur | __enable_next |  witness columns | \
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |>  witness + constants  2^(k-1)
    // |  ...             |   ...        |      ...      |   ...            |  |
    // |  bla_bla_bla     |    1         |       0       |   bla_bla_bla    | /     <- __enable_next ==0 since there's no state transition
    // |  0               |    0         |       0       |   0              | \
    // |  0               |    0         |       0       |   0              |  |
    // |  ...             |   ...        |      ...      |   ...            |  |> 2^2-1
    // |  0               |    0         |       0       |   <unusable>     |  |
    // |  0               |    0         |       0       |   <unusable>     | /

    // generate constants and commits (witness).

    let query = |column, rotation| Expr::Var(PlonkVar::ColumnQuery { column, rotation });

    let (constants, degree) = constant_evaluator::generate(analyzed);
    let commits = commit_evaluator::generate(analyzed, degree, &constants, query_callback, verbose);

    let mut cd = CircuitData::from(constants, commits);

    // append to fixed columns, one that enables constrains that does not have rotations
    // and another that enables contraints that have a rotation ( not this is not activated )
    // in last row.

    let num_rows = cd.len();

    let q_enable_cur = query(
        cd.insert_constant("__enable_cur", itertools::repeat_n(BigInt::one(), num_rows)),
        0,
    );

    let q_enable_next = query(
        cd.insert_constant(
            "__enable_next",
            itertools::repeat_n(BigInt::one(), num_rows - 1).chain(std::iter::once(BigInt::zero())),
        ),
        0,
    );

    // build public input columns and constraints  -------------

    // collect all inputs-by-pc defined in queries

    let mut inputs_by_pc = HashMap::new();

    for (_, (_, def)) in &analyzed.definitions {
        let Some(FunctionValueDefinition::Query(Expression::Tuple(query))) = def else {
            continue;
        };
        for expr in query {
            let Expression::Tuple(query_args) = expr else {
                continue;
            };
            let Some(Expression::Number(pc)) = query_args.get(0) else {
                continue;
            };
            let Some(Expression::Tuple(expr_args)) = query_args.get(1) else {
                continue;
            };
            if Some(&Expression::String(String::from("input"))) != expr_args.get(0) {
                continue;
            }
            let Some(exprs) = expr_args.get(1) else {
                continue;
            };

            inputs_by_pc.insert(pc.clone(), exprs);
        }
    }

    let input_count_col_values: Vec<_> = (0..num_rows).map(BigInt::from).collect();
    let input_value_col_values: Vec<_> = inputs
        .iter()
        .cloned()
        .chain(std::iter::repeat(BigInt::zero()))
        .take(num_rows)
        .collect();

    let pc_column = cd.col("Assembly.pc");

    let input_index_col_values: Vec<BigInt> = (0..num_rows)
        .map(|row_no| {
            let pc = cd
                .commits
                .get(pc_column.index)
                .unwrap()
                .1
                .get(row_no)
                .unwrap();
            if let Some(expr) = inputs_by_pc.get(pc) {
                eval_expression_at_row(expr, row_no, &cd, inputs, int_to_field)
            } else {
                BigInt::zero()
            }
        })
        .collect();

    let input_count_col = cd.insert_constant("__input_count", input_count_col_values);
    let input_value_col = cd.insert_commit("__input_value", input_value_col_values);
    let input_index_col = cd.insert_commit("__input_index", input_index_col_values);

    // cool, now we have to prove that
    // a) the input index and the value used for input corresponds to a valid input
    //    ( input_index , Assembly.X_free_value ) exists in ( input_count, input_value )

    let mut lookups = vec![];
    let mut polys = vec![];

    let x_free_value_col = cd.col("Assembly.X_free_value");
    let x_read_free_col = cd.col("Assembly.p_X_read_free");
    let p_x_read_free = query(x_read_free_col, 0);

    lookups.push(Lookup {
        name: "fee_value is a valid input when reading inputs".to_string(),
        exps: (
            vec![
                p_x_read_free.clone() * query(input_index_col, 0),
                p_x_read_free * query(x_free_value_col, 0),
            ],
            vec![query(input_count_col, 0), query(input_value_col, 0)],
        ),
    });

    // b) that the column input_value_index is correctly computed  from query expressions, the constrain is as follows:
    //    sum( for each (pc, query_expr) if pc_col == pc then col(input_index) == query_expr  )

    let mut correct_input_value_per_pc_and_query = Expr::Const(BigUint::zero());

    for (pc, query_expr) in inputs_by_pc.iter() {
        // witness & constrain for is_zero_inv for col(pc)-pc --------------------------------------------------------------
        // -----------------------------------------------------------------------------------------------------------------

        let pc_diff_value_inv_col_values: Vec<_> = (0..num_rows)
            .map(|row_no| {
                let pc_row_value = cd
                    .commits
                    .get(pc_column.index)
                    .unwrap()
                    .1
                    .get(row_no)
                    .unwrap();
                let diff = pc_row_value - pc;
                if diff.is_zero() {
                    BigInt::zero()
                } else {
                    modinv(&diff, &field_mod.to_bigint().unwrap())
                }
            })
            .collect();

        let value_inv_col =
            cd.insert_commit("__pc_diff_value_inv_col_values", pc_diff_value_inv_col_values);
        let value_inv = query(value_inv_col, 0);

        // query(pc) == pc constrain
        let value = query(pc_column, 0) - Expr::Const(pc.to_biguint().unwrap());

        let is_zero_expression = Expr::Const(BigUint::one()) - value.clone() * value_inv;

        polys.push(Poly {
            name: "pc_diff_is_zero".to_string(),
            exp: q_enable_cur.clone() * value * is_zero_expression.clone(),
        });

        let query_expr =
            expression_2_expr(&cd, query_expr, int_to_field).0 - query(input_index_col, 0);

        correct_input_value_per_pc_and_query =
            correct_input_value_per_pc_and_query + is_zero_expression * query_expr;
    }

    polys.push(Poly {
        name: "correct_input_value_per_pc_and_query".to_string(),
        exp: q_enable_cur.clone() * correct_input_value_per_pc_and_query,
    });

    // build Plaf columns. -------------

    let exports = (0..max_public_inputs)
        .map(|offset| ColumnPublicValue {
            witness_column: input_value_col.index,
            offset,
        })
        .collect();

    let columns = Columns {
        fixed: cd
            .constants
            .iter()
            .map(|(name, _)| ColumnFixed::new(name.to_string()))
            .collect(),
        witness: cd
            .commits
            .iter()
            .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
            .collect(),
        public: vec![ColumnPublic::new(String::from("instance")).export(exports)],
    };

    // build Plaf info. -------------

    let info = Info {
        p: field_mod,
        num_rows: cd.len(),
        challenges: vec![],
    };

    // build Plaf polys. -------------

    for id in &analyzed.identities {
        if id.kind == IdentityKind::Polynomial {
            // polinomial identities.

            assert_eq!(id.right.expressions.len(), 0);
            assert_eq!(id.right.selector, None);
            assert_eq!(id.left.expressions.len(), 0);

            let (exp, rotation) =
                expression_2_expr(&cd, &id.left.selector.as_ref().unwrap(), int_to_field);

            // depending if this polinomial contains a rotation, enable for all rows or all unless last one.

            let exp = if rotation == 0 {
                Expr::Mul(vec![exp, q_enable_cur.clone()])
            } else {
                Expr::Mul(vec![exp, q_enable_next.clone()])
            };
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
                    expression_2_expr(&cd, &expr, int_to_field).0
                });

            let left = id
                .left
                .expressions
                .iter()
                .map(|expr| left_selector.clone() * expression_2_expr(&cd, expr, int_to_field).0)
                .collect();

            let right = id
                .right
                .expressions
                .iter()
                .map(|expr| expression_2_expr(&cd, expr, int_to_field).0)
                .collect();

            lookups.push(Lookup {
                name: "".to_string(),
                exps: (left, right),
            });
        } else {
            unimplemented!()
        }
    }

    // build Plaf fixeds. -------------

    let fixed: Vec<Vec<_>> = cd
        .constants
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(int_to_field(value)))
                .collect()
        })
        .collect();

    // build plaf. -------------

    let plaf = Plaf {
        info,
        columns,
        polys,
        lookups,
        copys: vec![],
        fixed,
    };

    // build witness. -------------

    let witness: Vec<Vec<_>> = cd
        .commits
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(value.to_biguint().unwrap()))
                .collect()
        })
        .collect();

    let witness_cols = cd
        .commits
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Fixed, n)));

    let wit = Witness {
        num_rows: cd.commits.len(),
        columns: witness_cols
            .map(|(name, _)| ColumnWitness::new(name, 0))
            .collect(),
        witness,
    };

    // return circuit description + witness. -------------

    PlafH2Circuit { plaf, wit }
}
