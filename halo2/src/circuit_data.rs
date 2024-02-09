#![allow(unused)]

use std::collections::HashMap;

use num_bigint::BigInt;
use polyexen::expr::{Column, ColumnKind};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{AbstractNumberType, FieldElement};

pub(crate) struct CircuitData {
    pub(crate) public_column: Column,
    pub columns: HashMap<String, Column>,
    fixed_id_counter: usize,
}

impl CircuitData {
    pub fn from<T: FieldElement>(pil: &Analyzed<T>, fixed_names: &[String]) -> Self {
        let const_cols = fixed_names.iter().enumerate().map(|(index, name)| {
            (
                name.to_string(),
                Column {
                    kind: ColumnKind::Fixed,
                    index,
                },
            )
        });

        let witness_cols = pil
            .committed_polys_in_source_order()
            .into_iter()
            .flat_map(|(p, _)| p.array_elements())
            .enumerate()
            .map(|(index, (name, _))| {
                (
                    name.to_string(),
                    Column {
                        kind: ColumnKind::Witness,
                        index,
                    },
                )
            });

        let columns = const_cols.chain(witness_cols).collect();
        let public_column = Column {
            kind: ColumnKind::Public,
            index: 0,
        };

        Self {
            columns,
            public_column,
            fixed_id_counter: fixed_names.len(),
        }
    }

    pub fn col(&self, name: &str) -> Column {
        *self
            .columns
            .get(name)
            .unwrap_or_else(|| panic!("{name} column not found"))
    }
}
