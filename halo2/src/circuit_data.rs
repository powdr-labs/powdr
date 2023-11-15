#![allow(unused)]

use std::collections::HashMap;

use num_bigint::BigInt;
use number::{AbstractNumberType, FieldElement};
use polyexen::expr::{Column, ColumnKind};

pub(crate) struct CircuitData<'a, T> {
    pub(crate) fixed: Vec<(String, Vec<T>)>,
    pub(crate) witness: &'a [(String, Vec<T>)],
    columns: HashMap<String, Column>,
}

impl<'a, T: FieldElement> CircuitData<'a, T> {
    pub fn from(fixed: Vec<(String, Vec<T>)>, witness: &'a [(String, Vec<T>)]) -> Self {
        if !fixed.is_empty() && !witness.is_empty() {
            assert_eq!(
                fixed.get(0).unwrap().1.len(),
                witness.get(0).unwrap().1.len()
            );
        }

        let const_cols = fixed.iter().enumerate().map(|(index, (name, _))| {
            (
                name.to_string(),
                Column {
                    kind: ColumnKind::Fixed,
                    index,
                },
            )
        });

        let witness_cols = witness.iter().enumerate().map(|(index, (name, _))| {
            (
                name.to_string(),
                Column {
                    kind: ColumnKind::Witness,
                    index,
                },
            )
        });

        let columns = const_cols.chain(witness_cols).collect();

        Self {
            fixed,
            witness,
            columns,
        }
    }

    pub fn col(&self, name: &str) -> Column {
        *self
            .columns
            .get(name)
            .unwrap_or_else(|| panic!("{name} column not found"))
    }

    pub fn len(&self) -> usize {
        self.witness.get(0).unwrap().1.len()
    }

    pub fn insert_constant<IT: IntoIterator<Item = T>>(
        &mut self,
        name: &'a str,
        values: IT,
    ) -> Column {
        let values = values.into_iter().collect::<Vec<_>>();
        assert_eq!(values.len(), self.len());
        self.fixed.push((name.to_string(), values));
        let column = Column {
            kind: ColumnKind::Fixed,
            index: self.fixed.len() - 1,
        };
        self.columns.insert(name.to_string(), column);
        column
    }
}
