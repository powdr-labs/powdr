use std::collections::BTreeMap;

use number::FieldElement;

use super::affine_expression::AffineExpression;

#[derive(Clone, Debug)]
pub struct SequenceStep {
    pub row_delta: i64,
    pub identity: IdentityInSequence,
}

/// Goes through all rows of the block machine (plus the ones before and after)
/// forward, backward, and forward again.
/// In each row, iterates over all identities until no further progress is made.
pub struct DefaultSequenceIterator {
    identities_count: usize,
    row_deltas: Vec<i64>,
    outer_query_row: Option<i64>,

    /// Whether this is the first time the iterator is called.
    is_first: bool,
    /// Whether any progress was made in the current round.
    progress_in_current_round: bool,
    /// The current row delta index.
    cur_row_delta_index: usize,
    /// The current identity index.
    cur_identity_index: usize,
    /// The number of rounds for the current row delta.
    /// If this number gets too large, we will assume that we're in an infinite loop and exit.
    current_round_count: usize,

    /// The steps on which we made progress.
    progress_steps: Vec<SequenceStep>,
}

const MAX_ROUNDS_PER_ROW_DELTA: usize = 100;

impl DefaultSequenceIterator {
    pub fn new(block_size: usize, identities_count: usize, outer_query_row: Option<i64>) -> Self {
        assert!(block_size >= 1);
        let max_row = block_size as i64 - 1;
        DefaultSequenceIterator {
            identities_count,
            row_deltas: (-1..=max_row)
                .chain((-1..max_row).rev())
                .chain(0..=max_row)
                .collect(),
            outer_query_row,
            is_first: true,
            progress_in_current_round: false,
            cur_row_delta_index: 0,
            cur_identity_index: 0,
            current_round_count: 0,
            progress_steps: vec![],
        }
    }

    /// Update the state of the iterator.
    /// If we're not at the last identity in the current row, just moves to the next.
    /// Otherwise, starts with identity 0 and moves to the next row if no progress was made.
    fn update_state(&mut self) {
        if !self.is_first {
            if self.is_last_identity() {
                self.start_next_round();
            } else {
                // Stay at row delta, move to next identity.
                self.cur_identity_index += 1;
            }
        }
        self.is_first = false;
    }

    fn is_last_identity(&self) -> bool {
        let row_delta = self.row_deltas[self.cur_row_delta_index];
        let is_on_row_with_outer_query = self.outer_query_row == Some(row_delta);

        if is_on_row_with_outer_query {
            // In the last row, we want to process one more identity, the outer query.
            self.cur_identity_index == self.identities_count
        } else {
            self.cur_identity_index == self.identities_count - 1
        }
    }

    fn start_next_round(&mut self) {
        if !self.progress_in_current_round || self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
            // Move to next row delta, starting with identity 0.
            if self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
                panic!("In witness generation for block machine, we have been stuck in the same row for {MAX_ROUNDS_PER_ROW_DELTA} rounds. \
                            This is a bug in the witness generation algorithm.");
            }

            self.cur_row_delta_index += 1;
            self.current_round_count = 0;
        } else {
            // Stay and current row delta, starting with identity 0.
            self.current_round_count += 1;
        }
        self.cur_identity_index = 0;
        self.progress_in_current_round = false;
    }

    pub fn report_progress(&mut self, progress_in_last_step: bool) {
        assert!(!self.is_first, "Called report_progress() before next()");

        if progress_in_last_step {
            self.progress_steps.push(self.current_step());
        }
        self.progress_in_current_round |= progress_in_last_step;
    }

    pub fn next(&mut self) -> Option<SequenceStep> {
        self.update_state();

        if self.cur_row_delta_index == self.row_deltas.len() {
            // Done!
            return None;
        }

        Some(self.current_step())
    }

    fn current_step(&self) -> SequenceStep {
        SequenceStep {
            row_delta: self.row_deltas[self.cur_row_delta_index],
            identity: if self.cur_identity_index < self.identities_count {
                IdentityInSequence::Internal(self.cur_identity_index)
            } else {
                IdentityInSequence::OuterQuery
            },
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum IdentityInSequence {
    Internal(usize),
    OuterQuery,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct SequenceCacheKey {
    /// For each expression on the left-hand side of the lookup, whether it is a constant.
    known_columns: Vec<bool>,
}

impl<K, T> From<&[AffineExpression<K, T>]> for SequenceCacheKey
where
    K: Copy + Ord,
    T: FieldElement,
{
    fn from(value: &[AffineExpression<K, T>]) -> Self {
        SequenceCacheKey {
            known_columns: value.iter().map(|v| v.is_constant()).collect(),
        }
    }
}

pub enum ProcessingSequenceIterator {
    Default(DefaultSequenceIterator),
    Cached(<Vec<SequenceStep> as IntoIterator>::IntoIter),
}

impl ProcessingSequenceIterator {
    pub fn report_progress(&mut self, progress_in_last_step: bool) {
        match self {
            Self::Default(it) => it.report_progress(progress_in_last_step),
            Self::Cached(_) => {} // Progress is ignored
        }
    }

    pub fn has_steps(&self) -> bool {
        match self {
            Self::Default(_) => true,
            Self::Cached(it) => it.len() > 0,
        }
    }
}

impl Iterator for ProcessingSequenceIterator {
    type Item = SequenceStep;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Default(it) => it.next(),
            Self::Cached(it) => it.next(),
        }
    }
}

pub struct ProcessingSequenceCache {
    block_size: usize,
    identities_count: usize,
    cache: BTreeMap<SequenceCacheKey, Vec<SequenceStep>>,
}

impl ProcessingSequenceCache {
    pub fn new(block_size: usize, identities_count: usize) -> Self {
        ProcessingSequenceCache {
            block_size,
            identities_count,
            cache: Default::default(),
        }
    }

    pub fn get_processing_sequence<K, T>(
        &self,
        left: &[AffineExpression<K, T>],
    ) -> ProcessingSequenceIterator
    where
        K: Copy + Ord,
        T: FieldElement,
    {
        match self.cache.get(&left.into()) {
            Some(cached_sequence) => {
                log::trace!("Using cached sequence");
                ProcessingSequenceIterator::Cached(cached_sequence.clone().into_iter())
            }
            None => {
                log::trace!("Using default sequence");
                ProcessingSequenceIterator::Default(DefaultSequenceIterator::new(
                    self.block_size,
                    self.identities_count,
                    // Run the outer query on the last row of the block.
                    Some(self.block_size as i64 - 1),
                ))
            }
        }
    }

    pub fn report_incomplete<K, T>(&mut self, left: &[AffineExpression<K, T>])
    where
        K: Copy + Ord,
        T: FieldElement,
    {
        self.cache.entry(left.into()).or_insert(vec![]);
    }

    pub fn report_processing_sequence<K, T>(
        &mut self,
        left: &[AffineExpression<K, T>],
        sequence_iterator: ProcessingSequenceIterator,
    ) where
        K: Copy + Ord,
        T: FieldElement,
    {
        match sequence_iterator {
            ProcessingSequenceIterator::Default(it) => {
                self.cache.entry(left.into()).or_insert(it.progress_steps);
            }
            ProcessingSequenceIterator::Cached(_) => {} // Already cached, do nothing
        }
    }
}
