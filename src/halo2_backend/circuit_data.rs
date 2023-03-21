use std::collections::HashMap;

use crate::number::AbstractNumberType;
use num_bigint::BigInt;
use polyexen::expr::{Column, ColumnKind};
use prettytable::Cell as PCell;
use prettytable::{Row, Table};

pub(crate) struct CircuitData<'a> {
    pub(crate) constants: Vec<(&'a str, Vec<AbstractNumberType>)>,
    pub(crate) commits: Vec<(&'a str, Vec<AbstractNumberType>)>,
    columns: HashMap<String, Column>,
}

impl<'a> CircuitData<'a> {
    pub fn from(
        constants: Vec<(&'a str, Vec<AbstractNumberType>)>,
        commits: Vec<(&'a str, Vec<AbstractNumberType>)>,
    ) -> Self {
        assert_eq!(
            constants.get(0).unwrap().1.len(),
            commits.get(0).unwrap().1.len()
        );

        let const_cols = constants.iter().enumerate().map(|(index, (name, _))| {
            (
                name.to_string(),
                Column {
                    kind: ColumnKind::Fixed,
                    index,
                },
            )
        });

        let witness_cols = commits.iter().enumerate().map(|(index, (name, _))| {
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
            constants,
            commits,
            columns,
        }
    }
    pub fn col(&self, name: &str) -> Column {
        *self
            .columns
            .get(name)
            .unwrap_or_else(|| panic!("{name} column not found"))
    }
    pub fn val(&self, column: &Column, offset: usize) -> &BigInt {
        match column.kind {
            ColumnKind::Fixed => self
                .constants
                .get(column.index)
                .unwrap()
                .1
                .get(offset)
                .unwrap(),
            ColumnKind::Witness => self
                .commits
                .get(column.index)
                .unwrap()
                .1
                .get(offset)
                .unwrap(),
            _ => unimplemented!(),
        }
    }
    pub fn len(&self) -> usize {
        self.constants.get(0).unwrap().1.len()
    }
    pub fn insert_constant<IT: IntoIterator<Item = BigInt>>(
        &mut self,
        name: &'a str,
        values: IT,
    ) -> Column {
        let values = values.into_iter().collect::<Vec<_>>();
        assert_eq!(values.len(), self.len());
        self.constants.push((name, values));
        let column = Column {
            kind: ColumnKind::Fixed,
            index: self.constants.len() - 1,
        };
        self.columns.insert(name.to_string(), column);
        column
    }
    pub fn insert_commit<IT: IntoIterator<Item = BigInt>>(
        &mut self,
        name: &'a str,
        values: IT,
    ) -> Column {
        let values = values.into_iter().collect::<Vec<_>>();
        assert_eq!(values.len(), self.len());
        self.commits.push((name, values));
        let column = Column {
            kind: ColumnKind::Witness,
            index: self.commits.len() - 1,
        };
        self.columns.insert(name.to_string(), column);
        column
    }

    #[allow(unused)]
    pub fn printstd(&self, cols: Option<&[&str]>) {
        let mut data: HashMap<_, _> = self
            .constants
            .iter()
            .chain(self.commits.iter())
            .cloned()
            .collect();
        if let Some(cols) = cols {
            data.retain(|name, _| cols.contains(name));
        };

        let mut table = Table::new();
        let header_n = std::iter::once(PCell::new(""));
        let header_rest = data.keys().map(|name| PCell::new(name));
        let headers = header_n.chain(header_rest).collect();
        table.add_row(Row::new(headers));

        let trim = |s: &str| {
            if s.len() > 5 {
                format!("{}…", &s[..5])
            } else {
                s.to_string()
            }
        };

        for row in 0..self.len() {
            let value_n = std::iter::once(PCell::new(&row.to_string()));
            let value_rest = data
                .values()
                .map(|d| PCell::new(&trim(&d.get(row).unwrap().to_string())));
            let values = value_n.chain(value_rest).collect();
            table.add_row(Row::new(values));
        }

        table.printstd();
    }
}
