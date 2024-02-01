use std::collections::HashMap;

use halo2_curves::bn256::Fr;
use halo2_curves::ff::FromUniformBytes;
use num_bigint::BigUint;
use polyexen::expr::{ColumnQuery, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{
    ColumnFixed, ColumnPublic, ColumnWitness, Columns, CopyC, Info, Lookup, Plaf, Poly, Shuffle,
    Witness,
};
use powdr_ast::parsed::SelectedExpressions;

use num_traits::{One, ToPrimitive};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperator, AlgebraicExpression as Expression, Analyzed, IdentityKind,
};
use powdr_number::{BigInt, FieldElement};

use super::circuit_data::CircuitData;

/// Converts an analyzed PIL and fixed to a Plaf circuit.
/// A Plaf circuit only contains the shape of the circuit.
pub(crate) fn analyzed_to_plaf<T: FieldElement>(
    analyzed: &Analyzed<T>,
    fixed: &[(String, Vec<T>)],
) -> Plaf {
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

    let mut cd = CircuitData::from(analyzed, fixed.to_owned());

    // append two fixed columns:
    // - one that enables constraints that do not have rotations (__enable_cur) in the actual circuit
    // - and another that enables constraints that have a rotation (__enable_next) in the actual circuit except in the last actual row

    let num_rows: usize = analyzed.degree() as usize;

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

    let wit_columns: Vec<_> = analyzed
        .committed_polys_in_source_order()
        .into_iter()
        .flat_map(|(p, _)| p.array_elements())
        .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
        .collect();

    // build Plaf columns -------------------------------------------------

    let columns = Columns {
        fixed: cd
            .fixed
            .iter()
            .map(|(name, _)| ColumnFixed::new(name.to_string()))
            .collect(),
        witness: wit_columns,
        public: vec![ColumnPublic::new("public".to_string())],
    };

    // build Plaf info. -------------------------------------------------------------------------

    let info = Info {
        p: T::modulus().to_arbitrary_integer(),
        num_rows: cd.len(),
        challenges: vec![],
    };

    // build Plaf polys. -------------------------------------------------------------------------

    let apply_selectors_to_set = |set: &SelectedExpressions<Expression<T>>| {
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

    let identities = analyzed.identities_with_inlined_intermediate_polynomials();
    for id in &identities {
        match id.kind {
            IdentityKind::Polynomial => {
                // polynomial identities.

                assert_eq!(id.right.expressions.len(), 0);
                assert_eq!(id.right.selector, None);
                assert_eq!(id.left.expressions.len(), 0);

                let exp = id.expression_for_poly_id();
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

    if lookups.is_empty() {
        // TODO something inside halo2 breaks (only in debug mode) if lookups is empty,
        // so just add an empty lookup.
        lookups.push(Lookup {
            name: "".to_string(),
            exps: (vec![], vec![]),
        });
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

    Plaf {
        info,
        columns,
        polys,
        metadata: Default::default(),
        lookups,
        shuffles,
        copys: copy_constraints(analyzed, &cd),
        fixed,
    }
}

fn copy_constraints<T: FieldElement>(pil: &Analyzed<T>, cd: &CircuitData<T>) -> Vec<CopyC> {
    // Enforce publics by copy-constraining to cells in the instance column.
    // For example, if we have the following public declarations:
    // - public out1 = A(2);
    // - public out2 = B(0);
    // This would lead to the corresponding values being copied into the public
    // column as follows:
    // | Row | A   | B   | public |
    // |-----|-----|-----|--------|
    // |  0  |  0  | *5* |  *2*   |
    // |  1  |  1  |  6  |  *5*   |
    // |  2  | *2* |  7  |        |
    // |  3  |  4  |  8  |        |
    let mut copies = vec![];
    for public_declaration in pil.public_declarations.values() {
        let witness_name = public_declaration.referenced_poly_name();
        let witness_col = cd.col(&witness_name);
        let witness_offset = public_declaration.index as usize;
        let public_col = cd.public_column;
        let public_offset = copies.len();

        // Add copy constraint
        copies.push(CopyC {
            columns: (public_col, witness_col),
            // We could also create one copy constraint per column pair wih several offsets.
            // I don't think there is a difference though...
            offsets: vec![(public_offset, witness_offset)],
        });
    }

    copies
}

/// Converts an analyzed PIL and fixed to a PlafH2Circuit.
/// A PlafH2Circuit contains the witness because Halo2 is like that.
/// Because of that we just build a witness with the correct length
/// but with 0s.
pub(crate) fn analyzed_to_circuit_with_zeroed_witness<T: FieldElement>(
    plaf: Plaf,
    analyzed: &Analyzed<T>,
) -> PlafH2Circuit {
    let num_rows = plaf.fixed.len();

    let wit_columns: Vec<_> = analyzed
        .committed_polys_in_source_order()
        .into_iter()
        .flat_map(|(p, _)| p.array_elements())
        .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
        .collect();

    // build zeroed witness. -------------------------------------------------------------------------

    let witness: Vec<Vec<_>> = wit_columns.iter().map(|_| vec![None; num_rows]).collect();

    let wit = Witness {
        num_rows: wit_columns.len(),
        columns: wit_columns,
        witness,
    };

    // return circuit description + empty witness
    PlafH2Circuit { plaf, wit }
}

/// Convert from Bn254Field to halo2_curves::bn256::Fr by converting to little endian bytes and back.
pub fn powdr_ff_to_fr<T: FieldElement>(x: T) -> Fr {
    let mut buffer = [0u8; 64];
    let bytes = x.to_bytes_le();
    buffer[..bytes.len()].copy_from_slice(&bytes);
    Fr::from_uniform_bytes(&buffer)
}

fn public_values<T: FieldElement>(pil: &Analyzed<T>, witness: &[(String, Vec<T>)]) -> Vec<Fr> {
    let witness_map: HashMap<String, Vec<T>> = witness.iter().cloned().collect();
    let eval_witness = |name: &String, row: usize| -> T { witness_map.get(name).unwrap()[row] };

    let mut publics = vec![];
    for public_declaration in pil.public_declarations.values() {
        let witness_name = public_declaration.referenced_poly_name();
        let witness_offset = public_declaration.index as usize;

        // Evaluate the given cell and add it to the publics.
        let value = eval_witness(&witness_name, witness_offset);
        let value = powdr_ff_to_fr(value);

        // Add to publics.
        publics.push(value);
    }

    publics
}

/// Converts an analyzed PIL, fixed and witness columns to a PlafH2Circuit and publics for the halo2 backend.
pub(crate) fn analyzed_to_circuit_with_witness<T: FieldElement>(
    analyzed: &Analyzed<T>,
    plaf: Plaf,
    witness: &[(String, Vec<T>)],
) -> (PlafH2Circuit, Vec<Vec<Fr>>) {
    let num_rows = plaf.fixed.len();

    assert!(
        analyzed.public_declarations.len() <= num_rows,
        "More publics than rows!"
    );

    // build witness. -------------------------------------------------------------------------

    let converted_witness: Vec<Vec<_>> = witness
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(value.to_arbitrary_integer()))
                .collect()
        })
        .collect();

    let wit = Witness {
        num_rows: witness.len(),
        columns: witness
            .iter()
            .map(|(name, _)| ColumnWitness::new(name.clone(), 0))
            .collect(),
        witness: converted_witness,
    };

    // return circuit description + witness. -------------

    (
        PlafH2Circuit { plaf, wit },
        vec![public_values(analyzed, witness)],
    )
}

fn expression_2_expr<T: FieldElement>(cd: &CircuitData<T>, expr: &Expression<T>) -> Expr<PlonkVar> {
    match expr {
        Expression::Number(n) => Expr::Const(n.to_arbitrary_integer()),
        Expression::Reference(polyref) => {
            let plonkvar = PlonkVar::Query(ColumnQuery {
                column: cd.col(&polyref.name),
                rotation: polyref.next as i32,
            });

            Expr::Var(plonkvar)
        }
        Expression::BinaryOperation(lhe, op, rhe_powdr) => {
            let lhe = expression_2_expr(cd, lhe);
            let rhe = expression_2_expr(cd, rhe_powdr);
            match op {
                AlgebraicBinaryOperator::Add => Expr::Sum(vec![lhe, rhe]),
                AlgebraicBinaryOperator::Sub => Expr::Sum(vec![lhe, Expr::Neg(Box::new(rhe))]),
                AlgebraicBinaryOperator::Mul => Expr::Mul(vec![lhe, rhe]),
                AlgebraicBinaryOperator::Pow => {
                    let Expression::Number(e) = rhe_powdr.as_ref() else {
                        panic!("Expected number in exponent.")
                    };
                    Expr::Pow(
                        Box::new(lhe),
                        e.to_arbitrary_integer()
                            .to_u32()
                            .unwrap_or_else(|| panic!("Exponent has to fit 32 bits.")),
                    )
                }
            }
        }

        _ => unimplemented!("{:?}", expr),
    }
}
