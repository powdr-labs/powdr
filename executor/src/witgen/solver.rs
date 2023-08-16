use ast::analyzed::Identity;
use number::FieldElement;

use super::{
    identity_processor::IdentityProcessor,
    rows::{Row, RowPair, RowUpdater},
    FixedData,
};

pub struct Solver<'a, T: FieldElement> {
    pub row_offset: u64,
    pub data: Vec<Row<'a, T>>,
    pub identity_processor: IdentityProcessor<'a, T>,
    pub identities: Vec<Identity<T>>,
    pub fixed_data: &'a FixedData<'a, T>,
}

impl<'a, T: FieldElement> Solver<'a, T> {
    pub fn solve(&mut self) {
        for i in 0..(self.data.len() - 1) {
            self.iterate_on_row_pair(i);
        }
    }

    fn iterate_on_row_pair(&mut self, i: usize) {
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
                    .process_identity(identity, &row_pair)
                    .unwrap();
                let (before, after) = self.data.split_at_mut(i + 1);
                let current = before.last_mut().unwrap();
                let next = after.first_mut().unwrap();
                let mut row_updater = RowUpdater::new(current, next, self.row_offset + i as u64);
                progress |= row_updater.apply_updates(&updates, || identity.to_string());
            }
            if !progress {
                break;
            }
        }
    }
}
