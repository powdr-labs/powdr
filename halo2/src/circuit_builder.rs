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

/// Name of the __enable column
const ENABLE_NAME: &str = "__enable";

/// Converts a column of size $2^k$ and type T to a column of size $2^{k+1}$ and type Option<BigUint>:
/// - The first $2^k$ elements are Some(value.to_arbitrary_integer())
/// - Value $2^k + 1$ is the first value again
/// - The rest of the values are `None``
fn convert_column<T: FieldElement>(column: &[T]) -> Vec<Option<BigUint>> {
    let original_size = column.len();
    assert_eq!(
        original_size & (original_size - 1),
        0,
        "Expected a power of 2 size. Got {}",
        original_size
    );

    column
        .iter()
        // Convert to Some(value.to_arbitrary_integer())
        .map(|value| Some(value.to_arbitrary_integer()))
        // Repeat the first value
        .chain(std::iter::once(Some(column[0].to_arbitrary_integer())))
        // Pad with None
        .chain(itertools::repeat_n(None, original_size - 1))
        .collect::<Vec<_>>()
}

/// Converts an analyzed PIL and fixed to a Plaf circuit.
/// A Plaf circuit only contains the shape of the circuit.
pub(crate) fn analyzed_to_plaf<T: FieldElement>(
    analyzed: &Analyzed<T>,
    fixed: &[(String, Vec<T>)],
) -> Plaf {
    // The structure of the table is as following
    //
    // | constant columns | __enable     |  witness columns | \
    // |  c[0]            |    1         |   w[0]           |  |
    // |  c[1]            |    1         |   w[1]           |  |>  N actual circuit rows
    // |  ...             |   ...        |   ...            |  |
    // |  c[N - 2]        |    1         |   w[N - 2]       |  |
    // |  c[N - 1]        |    1         |   w[N - 1]       | /
    // |  c[0]            |    0         |   w[0]           | \  <-- Row 0 is copy-constrained to row N
    // |  None            |    None      |   None           |  |
    // |  ...             |   ...        |   ...            |  |>  2**(ceil(log2(N)) padding rows to fit the halo2 unusable rows
    // |  None            |    None      |   None           |  |
    // |  None            |    None      |   None           |  | <-- Halo2 will put blinding factors in the last few rows
    // |  None            |    None      |   None           | /      of the witness columns.

    // generate fixed and witness (witness).

    let query = |column, rotation| Expr::Var(PlonkVar::Query(ColumnQuery { column, rotation }));

    let fixed_names = fixed
        .iter()
        .map(|(name, _)| name.clone())
        // Append __enable fixed column
        .chain(std::iter::once(ENABLE_NAME.to_string()))
        .collect::<Vec<_>>();

    let original_size: usize = analyzed.degree() as usize;
    let fixed = fixed
        .iter()
        .map(|(_, column)| convert_column(column))
        // Append __enable fixed column
        .chain(std::iter::once(
            itertools::repeat_n(Some(BigUint::from(1u8)), original_size)
                .chain(std::iter::once(Some(BigUint::from(0u8))))
                .chain(itertools::repeat_n(None, original_size - 1))
                .collect(),
        ))
        .collect::<Vec<_>>();

    let cd = CircuitData::from(analyzed, &fixed_names);

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
        fixed: fixed_names.into_iter().map(ColumnFixed::new).collect(),
        witness: wit_columns,
        public: vec![ColumnPublic::new("public".to_string())],
    };

    // build Plaf info. -------------------------------------------------------------------------

    let info = Info {
        p: T::modulus().to_arbitrary_integer(),
        num_rows: original_size,
        challenges: vec![],
    };

    // build Plaf polys. -------------------------------------------------------------------------

    let q_enable = query(cd.col(ENABLE_NAME), 0);
    let apply_selectors_to_set = |set: &SelectedExpressions<Expression<T>>| {
        let selector = set
            .selector
            .clone()
            .map_or(Expr::Const(BigUint::one()), |expr| {
                expression_2_expr(&cd, &expr)
            });

        let selector = Expr::Mul(vec![selector, q_enable.clone()]);

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
                let exp = expression_2_expr(&cd, exp);

                // depending whether this polynomial contains a rotation,
                // enable for all rows or all except the last one.

                let exp = Expr::Mul(vec![exp, q_enable.clone()]);
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

fn copy_constraints<T: FieldElement>(pil: &Analyzed<T>, cd: &CircuitData) -> Vec<CopyC> {
    let mut copies = vec![];

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

    // Also, copy row 0 to row N.
    for (poly_name, _) in pil
        .committed_polys_in_source_order()
        .into_iter()
        .flat_map(|(p, _)| p.array_elements())
    {
        let witness_col = cd.col(&poly_name);
        copies.push(CopyC {
            columns: (witness_col, witness_col),
            offsets: vec![(0, pil.degree() as usize)],
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
        .map(|(_, column)| convert_column(column))
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

fn expression_2_expr<T: FieldElement>(cd: &CircuitData, expr: &Expression<T>) -> Expr<PlonkVar> {
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
