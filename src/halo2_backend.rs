#![allow(unused)]

use std::collections::HashMap;
use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;
use std::sync::Arc;

use halo2_proofs::halo2curves::FieldExt;
use itertools::Itertools;
use num_bigint::{BigInt, BigUint, Sign};
use polyexen::expr::{Column, ColumnKind, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{
    ColumnFixed, ColumnWitness, Columns, Info, Lookup, Plaf, PlafDisplayBaseTOML, Poly, Witness,
};
use prettytable::{Row, Table};

use crate::analyzer::{
    BinaryOperator, Expression, FunctionValueDefinition, Identity, IdentityKind,
};
use crate::commit_evaluator::WitnessColumn;
use crate::number::{abstract_to_degree, AbstractNumberType, DegreeType, FIELD_MOD};
use crate::parser::ast::PILFile;
use crate::{analyzer, asm_compiler, commit_evaluator, constant_evaluator, json_exporter};
use core::ops::Rem;
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr, plonk::Circuit};
use num_integer::Integer;
use num_traits::{One, Zero};

// Follow dependency installation instrucions from https://github.com/ed255/polyexen-demo

fn modinv(n: &BigInt, mut p: &BigInt) -> BigInt {
    let mut n = n.clone();
    
    if p.is_one() {
        return BigInt::one();
    }
    if n < BigInt::zero() {
        n = p - -n;
    }

    let (mut a, mut m, mut x, mut inv) = (n.clone(), p.clone(), BigInt::zero(), BigInt::one());

    while a > BigInt::one() {
        let (div, rem) = a.div_rem(&m);
        inv -= div * &x;
        a = rem;
        std::mem::swap(&mut a, &mut m);
        std::mem::swap(&mut x, &mut inv);
    }

    if inv < BigInt::zero() {
        inv += p
    }

    inv
}

fn print_table(data: &[(&str, Vec<BigInt>)]) {
    use prettytable::Cell as PCell;

    let mut table = Table::new();
    let header_n = std::iter::once(PCell::new(""));
    let header_rest = data.iter().map(|(name, _)| PCell::new(&name.to_string()));
    let headers = header_n.chain(header_rest).collect();
    table.add_row(Row::new(headers));

    let trim = |s: &str| {
        if s.len() > 5 {
            format!("{}…", &s[..5])
        } else {
            s.to_string()
        }
    };

    for row in 0..data.get(0).unwrap().1.len() {
        let value_n = std::iter::once(PCell::new(&row.to_string()));
        let value_rest = data
            .iter()
            .map(|(_, d)| PCell::new(&trim(&d.get(row).unwrap().to_string())));
        let values = value_n.chain(value_rest).collect();
        table.add_row(Row::new(values));
    }

    table.printstd();
}

fn expression_2_expr(
    cols: &HashMap<String, (ColumnKind, usize)>,
    expr: &Expression,
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> (Expr<PlonkVar>, i32) {
    match expr {
        Expression::Number(n) => (Expr::Const(int_to_field(&n)), 0),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);
            let (kind, index) = *cols.get(&polyref.name).unwrap();
            let rotation = if polyref.next { 1 } else { 0 };

            let plonkvar = PlonkVar::ColumnQuery {
                column: Column { kind, index },
                rotation,
            };
            (Expr::Var(plonkvar), rotation)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (lhe, lhe_rot) = expression_2_expr(cols, lhe, int_to_field);
            let (rhe, rhe_rot) = expression_2_expr(cols, rhe, int_to_field);
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
    cols: &HashMap<String, (ColumnKind, usize)>,
    constants: &Vec<(&str, Vec<AbstractNumberType>)>,
    commits: &Vec<(&str, Vec<AbstractNumberType>)>,
    inputs: &[BigInt],
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> AbstractNumberType {
    match expr {
        Expression::Number(n) => n.clone(),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);
            let (kind, index) = *cols.get(&polyref.name).unwrap();
            let rotation = if polyref.next { 1 } else { 0 };

            let value = match kind {
                ColumnKind::Fixed => constants.get(index).unwrap().1.get(at_row).unwrap(),
                ColumnKind::Witness => commits.get(index).unwrap().1.get(at_row).unwrap(),
                _ => unimplemented!(),
            };

            value.clone()
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let lhe =
                eval_expression_at_row(lhe, at_row, cols, constants, commits, inputs, int_to_field);
            let rhe =
                eval_expression_at_row(rhe, at_row, cols, constants, commits, inputs, int_to_field);
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
            let index = eval_expression_at_row(
                exprs.get(1).unwrap(),
                at_row,
                cols,
                constants,
                commits,
                inputs,
                int_to_field,
            );
            let index: u64 = index.try_into().unwrap();
            inputs[index as usize].clone()
        }
        _ => unimplemented!("{:?}", expr),
    }
}

fn analyzed_to_circuit(
    analyzed: &analyzer::Analyzed,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    inputs: &[BigInt],
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

    let (mut constants, degree) = constant_evaluator::generate(analyzed);
    let mut commits =
        commit_evaluator::generate(analyzed, degree, &constants, query_callback, verbose);
    assert_eq!(
        constants.get(0).unwrap().1.len(),
        commits.get(0).unwrap().1.len()
    );
    // print_table(&constants);
    // print_table(&commits);

    // get number of rows, we assume that data is defined by all rows and all columns.

    let num_rows = constants.get(0).unwrap().1.len();

    // append to fixed columns, one that enables constrains that does not have rotations
    // and another that enables contraints that have a rotation ( not this is not activated )
    // in last row.

    constants.push((
        "__enable_cur",
        itertools::repeat_n(BigInt::one(), num_rows).collect(),
    ));
    let q_enable_cur_col = constants.len() - 1;
    let q_enable_cur = Expr::Var(PlonkVar::ColumnQuery {
        column: Column {
            kind: ColumnKind::Fixed,
            index: q_enable_cur_col,
        },
        rotation: 0,
    });

    constants.push((
        "__enable_next",
        itertools::repeat_n(BigInt::one(), num_rows - 1)
            .chain(std::iter::once(BigInt::zero()))
            .collect(),
    ));
    let q_enable_next_col = constants.len() - 1;
    let q_enable_next = Expr::Var(PlonkVar::ColumnQuery {
        column: Column {
            kind: ColumnKind::Fixed,
            index: q_enable_next_col,
        },
        rotation: 0,
    });

    // build public input columns and constraints  -------------

    // get current columns names with their offset.

    let const_cols = constants
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Fixed, n)));

    let witness_cols = commits
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Witness, n)));

    let cols: HashMap<String, (ColumnKind, usize)> =
        const_cols.clone().chain(witness_cols.clone()).collect();

    // collect all inputs-by-pc defined in queries

    let mut inputs_by_pc = HashMap::new();

    for (def_name, (poly, def)) in &analyzed.definitions {
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

    // add new columns with inputs
    //    fixed   input_count
    //    witness input_value will contains a copy constrain of the inputs of the value
    //    witness x_free_value

    let input_count_col_values: Vec<_> = (0..num_rows).map(BigInt::from).collect();
    let input_value_col_values: Vec<_> = inputs
        .iter()
        .cloned()
        .chain(std::iter::repeat(BigInt::zero()))
        .take(num_rows)
        .collect();

    let (_, pc_column) = cols.get("Assembly.pc").unwrap();
    let input_index_col_values: Vec<BigInt> = (0..num_rows)
        .map(|row_no| {
            let pc = commits.get(*pc_column).unwrap().1.get(row_no).unwrap();
            if let Some(expr) = inputs_by_pc.get(pc) {
                eval_expression_at_row(
                    expr,
                    row_no,
                    &cols,
                    &constants,
                    &commits,
                    inputs,
                    int_to_field,
                )
            } else {
                BigInt::zero()
            }
        })
        .collect();

    constants.push(("input_count", input_count_col_values));
    let input_count_col = constants.len() - 1;

    commits.push(("input_value", input_value_col_values));
    let input_value_col = commits.len() - 1;

    commits.push(("input_index", input_index_col_values));
    let input_index_col = commits.len() - 1;

    // cool, now we have to prove that
    // a) the input index and the value used for input corresponds to a valid input
    //    ( input_index , Assembly.X_free_value ) exists in ( input_count, input_value )

    let mut lookups = vec![];
    let mut polys = vec![];

    let (_, x_free_value_col) = cols.get("Assembly.X_free_value").unwrap();

    lookups.push(Lookup {
        name: "fee_value is a valid input".to_string(),
        exps: (
            vec![
                Expr::Var(PlonkVar::ColumnQuery {
                    column: Column {
                        kind: ColumnKind::Witness,
                        index: input_index_col,
                    },
                    rotation: 0,
                }),
                Expr::Var(PlonkVar::ColumnQuery {
                    column: Column {
                        kind: ColumnKind::Witness,
                        index: *x_free_value_col,
                    },
                    rotation: 0,
                }),
            ],
            vec![
                Expr::Var(PlonkVar::ColumnQuery {
                    column: Column {
                        kind: ColumnKind::Fixed,
                        index: input_count_col,
                    },
                    rotation: 0,
                }),
                Expr::Var(PlonkVar::ColumnQuery {
                    column: Column {
                        kind: ColumnKind::Witness,
                        index: input_value_col,
                    },
                    rotation: 0,
                }),
            ],
        ),
    });

    // b) that the column input_value_index is correctly computed  from query expressions, the constrain is as follows:
    //    sum( for each (pc, query_expr) if pc_col == pc then query_expr is satisfied )

    let mut correct_input_value_per_pc_and_query = Expr::Const(BigUint::zero());
    
    println!("pcs => {:?}",inputs_by_pc.keys().collect::<Vec<_>>());

    //inputs_by_pc.retain(|k,_| k == &BigInt::from(0));
    println!("pcs => {:?}",inputs_by_pc.keys().collect::<Vec<_>>());

    for (n, (pc, query_expr)) in inputs_by_pc.iter().enumerate() {
        // witness for is_zero_inv
        let value_inv_col_values: Vec<_> = (0..num_rows)
            .map(|row_no| {
                let pc_row_value = commits.get(*pc_column).unwrap().1.get(row_no).unwrap();
                let diff = pc_row_value - pc;
                if diff.is_zero() {
                    BigInt::zero()
                } else {
                    modinv(&diff, &*FIELD_MOD)
                }
            })
            .collect();

        commits.push(("value_inv", value_inv_col_values));

        let value_inv_col = commits.len() - 1;

        let value_inv = Expr::Var(PlonkVar::ColumnQuery {
            column: Column {
                kind: ColumnKind::Witness,
                index: value_inv_col,
            },
            rotation: 0,
        });

        // query(pc) == pc constrain
        let value = Expr::Var(PlonkVar::ColumnQuery {
            column: Column {
                kind: ColumnKind::Witness,
                index: *pc_column,
            },
            rotation: 0,
        }) - Expr::Const(pc.to_biguint().unwrap());
        
        let is_zero_expression = Expr::Const(BigUint::one()) - value.clone() * value_inv;

         polys.push(Poly { name : "is_zero".to_string(), exp: q_enable_cur.clone() * value * is_zero_expression.clone()});

/* 

        EXP   pc_col  pc   IsZeroExpr             1-ZeroIf...  Expected EXP * ( 1-ZeroIf )
        ----- ------  ---  ---------------------  ------------ -------- --------------------
        0     3  ==   3    1                      1            ok       0
        2     3  ==   3    1                      1            fail     2
  
        0     3  !=   4    0                      0            ok       0 
        2     3  !=   4    0                      0            ok       0

        is_zero_expr * exp

*/  
        let (query_expr,_) = expression_2_expr(&cols, query_expr, int_to_field);
        correct_input_value_per_pc_and_query = correct_input_value_per_pc_and_query + (is_zero_expression * query_expr);

    }

    // polys.push(Poly { name : "correct_input_value_per_pc_and_query".to_string(), exp: q_enable_cur.clone() * correct_input_value_per_pc_and_query});
    // get columns names and offset. -------------

    let const_cols = constants
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Fixed, n)));

    let witness_cols = commits
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Witness, n)));

    let cols: HashMap<String, (ColumnKind, usize)> =
        const_cols.clone().chain(witness_cols.clone()).collect();

    // build Plaf columns. -------------

    let columns = Columns {
        fixed: constants
            .iter()
            .map(|(name, _)| ColumnFixed::new(name.to_string()))
            .collect(),
        witness: commits
            .iter()
            .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
            .collect(),
        public: vec![],
    };

    // build Plaf info. -------------

    let info = Info {
        p: polyexen::expr::get_field_p::<Fr>(),
        num_rows: constants.get(0).unwrap().1.len(),
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
                expression_2_expr(&cols, &id.left.selector.as_ref().unwrap(), int_to_field);

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

            assert_eq!(id.left.selector, None);
            assert_eq!(id.right.selector, None);

            let left = id
                .left
                .expressions
                .iter()
                .map(|expr| expression_2_expr(&cols, expr, int_to_field).0)
                .collect();

            let right = id
                .right
                .expressions
                .iter()
                .map(|expr| expression_2_expr(&cols, expr, int_to_field).0)
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

    let fixed: Vec<Vec<_>> = constants
        .iter()
        .map(|(_, row)| {
            row.into_iter()
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

    let witness: Vec<Vec<_>> = commits
        .iter()
        .map(|(_, row)| {
            row.into_iter()
                .map(|value| Some(value.to_biguint().unwrap()))
                .collect()
        })
        .collect();

    let wit = Witness {
        num_rows: commits.len(),
        columns: witness_cols
            .map(|(name, _)| ColumnWitness::new(name, 0))
            .collect(),
        witness,
    };

    // return circuit description + witness. -------------

    PlafH2Circuit { plaf, wit }
}

pub fn prove_asm(file_name: &str, inputs: Vec<AbstractNumberType>, verbose: bool) {
    // read and compile PIL.

    let contents = fs::read_to_string(file_name).unwrap();
    let pil = asm_compiler::compile(Some(file_name), &contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });
    let analyzed = &analyzer::analyze_string(&format!("{pil}"));

    // define how query information is retrieved.

    let query_callback = |query: &str| -> Option<AbstractNumberType> {
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        let mut it = items.iter();
        let _current_step = it.next().unwrap();
        let current_pc = it.next().unwrap();
        assert!(it.clone().len() % 3 == 0);
        for (pc_check, input, index) in it.tuples() {
            if pc_check == current_pc {
                assert_eq!(*input, "\"input\"");
                let index: usize = index.parse().unwrap();
                return inputs.get(index).cloned();
            }
        }
        None
    };

    let modulus = polyexen::expr::get_field_p::<Fr>();
    let int_to_field = |n: &BigInt| {
        let n = if let Some(n) = n.to_biguint() {
            n
        } else {
            &modulus - (-n).to_biguint().unwrap()
        };
        assert!(&n < &modulus);
        n
    };

    let circuit = analyzed_to_circuit(
        &analyzed,
        Some(query_callback),
        &inputs,
        verbose,
        &int_to_field,
    );
    let k = 1 + f32::log2(circuit.plaf.info.num_rows as f32).ceil() as u32;

    // println!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    let mock_prover = MockProver::<Fr>::run(k, &circuit, vec![]).unwrap();
    mock_prover.assert_satisfied();

    println!("cool, works");
}
