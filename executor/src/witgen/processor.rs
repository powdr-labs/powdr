use ast::analyzed::Identity;
use number::FieldElement;

use super::{
    identity_processor::IdentityProcessor,
    rows::{Row, RowFactory, RowPair, RowUpdater},
    EvalError, FixedData,
};

pub struct Processor<'a, T: FieldElement> {
    row_offset: u64,
    data: Vec<Row<'a, T>>,
    identity_processor: IdentityProcessor<'a, T>,
    identities: Vec<Identity<T>>,
    fixed_data: &'a FixedData<'a, T>,
    row_factory: RowFactory<'a, T>,
}

impl<'a, T: FieldElement> Processor<'a, T> {
    pub fn new(
        row_offset: u64,
        data: Vec<Row<'a, T>>,
        identity_processor: IdentityProcessor<'a, T>,
        identities: Vec<Identity<T>>,
        fixed_data: &'a FixedData<'a, T>,
        row_factory: RowFactory<'a, T>,
    ) -> Self {
        Self {
            row_offset,
            data,
            identity_processor,
            identities,
            fixed_data,
            row_factory,
        }
    }

    pub fn check_constraints(&mut self) -> bool {
        for i in 0..(self.data.len() - 1) {
            let row_pair = RowPair::new(
                &self.data[i],
                &self.data[i + 1],
                self.row_offset + i as u64,
                self.fixed_data,
                true,
            );
            for identity in &self.identities {
                if self
                    .identity_processor
                    .process_identity(identity, &row_pair)
                    .is_err()
                {
                    return false;
                }
            }
        }
        true
    }

    pub fn clear_row(&mut self, index: usize) {
        self.data[index] = self.row_factory.fresh_row();
    }

    pub fn solve(&mut self) -> Result<(), EvalError<T>> {
        for i in 0..(self.data.len() - 1) {
            self.iterate_on_row_pair(i)?;
        }
        Ok(())
    }

    pub fn finish(self) -> Vec<Row<'a, T>> {
        self.data
    }

    fn iterate_on_row_pair(&mut self, i: usize) -> Result<(), EvalError<T>> {
        loop {
            let mut progress = false;
            for identity in &self.identities {
                let row_pair = RowPair::new(
                    &self.data[i],
                    &self.data[i + 1],
                    self.row_offset + i as u64,
                    self.fixed_data,
                    false,
                );
                let updates = self
                    .identity_processor
                    .process_identity(identity, &row_pair)?;
                let (before, after) = self.data.split_at_mut(i + 1);
                let current = before.last_mut().unwrap();
                let next = after.first_mut().unwrap();
                let mut row_updater = RowUpdater::new(current, next, self.row_offset + i as u64);
                progress |= row_updater.apply_updates(&updates, || identity.to_string());
            }
            if !progress {
                break Ok(());
            }
        }
    }
}
