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
use crate::number::{abstract_to_degree, AbstractNumberType, DegreeType};
use crate::parser::ast::{PILFile};
use crate::{analyzer, asm_compiler, commit_evaluator, constant_evaluator, json_exporter};
use halo2_proofs::{dev::MockProver, halo2curves::bn256::Fr, plonk::Circuit};

// Follow dependency installation instrucions from https://github.com/ed255/polyexen-demo

fn print_table(data: &[(&str, Vec<BigInt>)]) {
    use prettytable::Cell as PCell;

    let mut table = Table::new();
    let header_n = std::iter::once(PCell::new("n"));
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

fn analyzed_to_circuit(
    analyzed: &analyzer::Analyzed,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
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
    let commits = commit_evaluator::generate(analyzed, degree, &constants, query_callback, verbose);
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
        itertools::repeat_n(BigInt::from(1), num_rows).collect(),
    ));
    let q_enable_cur_col = constants.len() - 1;

    constants.push((
        "__enable_next",
        itertools::repeat_n(BigInt::from(1), num_rows - 1)
            .chain(std::iter::once(BigInt::from(0)))
            .collect(),
    ));
    let q_enable_next_col = constants.len() - 1;

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

    // analyze inputs my pc -------------

    // collect all inputs by pc defined

    let mut inputs_by_pc = vec![]; 

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
            let (expr,_) = expression_2_expr(&cols, exprs, int_to_field);

            inputs_by_pc.push((pc.clone(),expr));
        }
    }

    // build an expression where, for each defined option, input_n == expression

        

    println!("====> {:#?}", inputs_by_pc);

    // build Plaf polys. -------------

    let mut polys = vec![];
    let mut lookups = vec![];
    let q_enable_cur = Expr::Var(PlonkVar::ColumnQuery {
        column: Column {
            kind: ColumnKind::Fixed,
            index: q_enable_cur_col,
        },
        rotation: 0,
    });
    let q_enable_next = Expr::Var(PlonkVar::ColumnQuery {
        column: Column {
            kind: ColumnKind::Fixed,
            index: q_enable_next_col,
        },
        rotation: 0,
    });
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

    let circuit = analyzed_to_circuit(&analyzed, Some(query_callback), verbose, &int_to_field);
    let k = 1 + f32::log2(circuit.plaf.info.num_rows as f32).ceil() as u32;

    println!("{}", PlafDisplayBaseTOML(&circuit.plaf));

    let mock_prover = MockProver::<Fr>::run(k, &circuit, vec![]).unwrap();
    mock_prover.assert_satisfied();

    println!("cool, works");
}
