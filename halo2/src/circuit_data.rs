#![allow(unused)]

use std::collections::HashMap;

use num_bigint::BigInt;
use polyexen::expr::{Column, ColumnKind};
use powdr_ast::analyzed::Analyzed;
use powdr_number::{AbstractNumberType, FieldElement};

pub(crate) struct CircuitData<T> {
    pub(crate) fixed: Vec<(String, Vec<T>)>,
    pub(crate) public_column: Column,
    pub columns: HashMap<String, Column>,
}

impl<'a, T: FieldElement> CircuitData<T> {
    pub fn from(pil: &Analyzed<T>, fixed: Vec<(String, Vec<T>)>) -> Self {
        let const_cols = fixed.iter().enumerate().map(|(index, (name, _))| {
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
            fixed,
            columns,
            public_column,
        }
    }

    pub fn col(&self, name: &str) -> Column {
        *self
            .columns
            .get(name)
            .unwrap_or_else(|| panic!("{name} column not found"))
    }

    pub fn len(&self) -> usize {
        self.fixed.get(0).unwrap().1.len()
    }

    pub fn insert_constant<IT: IntoIterator<Item = T>>(
        &mut self,
        name: &'a str,
        values: IT,
    ) -> Column {
        let values = values.into_iter().collect::<Vec<_>>();

        if !self.fixed.is_empty() {
            assert_eq!(values.len(), self.len());
        }

        self.fixed.push((name.to_string(), values));
        let column = Column {
            kind: ColumnKind::Fixed,
            index: self.fixed.len() - 1,
        };
        self.columns.insert(name.to_string(), column);
        column
    }
}
