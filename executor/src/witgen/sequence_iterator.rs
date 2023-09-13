use std::collections::BTreeMap;

use number::FieldElement;

use super::affine_expression::AffineExpression;

#[derive(Clone, Copy, Debug)]
pub enum Action {
    /// Process identity of a given index
    Process(usize),
    /// Process the outer query
    OuterQuery,
}

#[derive(Clone, Debug)]
pub struct SequenceStep {
    pub row_delta: i64,
    pub identity: Action,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Direction {
    Forward,
    Backward,
}

/// Goes through all rows of the block machine (plus the ones before and after)
/// forward, backward, and forward again.
/// In each row, iterates over all identities until no further progress is made.
pub struct DefaultSequenceIterator {
    identities_count: usize,
    current_row_delta: i64,
    directions: Vec<Direction>,
    direction_index: usize,
    on_outer_query_row: bool,
    on_last_row: bool,

    /// Whether this is the first time the iterator is called.
    is_first: bool,
    /// Whether any progress was made in the current round.
    progress_in_current_round: bool,
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
    pub fn new(identities_count: usize, directions: Vec<Direction>) -> Self {
        assert!(!directions.is_empty());
        assert!(directions.last().unwrap() == &Direction::Forward);
        DefaultSequenceIterator {
            identities_count,
            directions,
            direction_index: 0,
            current_row_delta: -1,
            on_outer_query_row: false,
            on_last_row: false,
            is_first: true,
            progress_in_current_round: false,
            cur_identity_index: 0,
            current_round_count: 0,
            progress_steps: vec![],
        }
    }

    /// Update the state of the iterator.
    /// If we're not at the last identity in the current row, just moves to the next.
    /// Otherwise, starts with identity 0 and moves to the next row if no progress was made.
    fn update_state(&mut self) -> Result<(), ()> {
        if !self.is_first {
            if self.is_last_identity() {
                self.start_next_round()?;
            } else {
                // Stay at row delta, move to next identity.
                self.cur_identity_index += 1;
            }
        }
        self.is_first = false;
        Ok(())
    }

    fn is_last_identity(&self) -> bool {
        if self.on_outer_query_row {
            // In the last row, we want to process one more identity, the outer query.
            self.cur_identity_index == self.identities_count
        } else {
            self.cur_identity_index == self.identities_count - 1
        }
    }

    fn start_next_round(&mut self) -> Result<(), ()> {
        if !self.progress_in_current_round || self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
            // Move to next row delta, starting with identity 0.
            if self.current_round_count > MAX_ROUNDS_PER_ROW_DELTA {
                panic!("In witness generation for block machine, we have been stuck in the same row for {MAX_ROUNDS_PER_ROW_DELTA} rounds. \
                            This is a bug in the witness generation algorithm.");
            }

            let direction = &self.directions[self.direction_index];
            if direction == &Direction::Forward && self.on_last_row
                || direction == &Direction::Backward && self.current_row_delta == -1
            {
                // Move to next direction.
                self.direction_index += 1;

                if self.direction_index == self.directions.len() {
                    // Done!
                    return Err(());
                }
            }

            self.current_row_delta += match self.directions[self.direction_index] {
                Direction::Forward => 1,
                Direction::Backward => -1,
            };
            self.current_round_count = 0;
        } else {
            // Stay and current row delta, starting with identity 0.
            self.current_round_count += 1;
        }
        self.cur_identity_index = 0;
        self.progress_in_current_round = false;
        Ok(())
    }

    pub fn report_progress(
        &mut self,
        progress_in_last_step: bool,
        on_outer_query_row: bool,
        on_last_row: bool,
    ) {
        assert!(!self.is_first, "Called report_progress() before next()");

        if progress_in_last_step {
            self.progress_steps.push(self.current_step());
        }
        self.progress_in_current_round |= progress_in_last_step;
        self.on_outer_query_row = on_outer_query_row;
        self.on_last_row = on_last_row;
    }

    pub fn next(&mut self) -> Option<SequenceStep> {
        match self.update_state() {
            Ok(()) => Some(self.current_step()),
            // Done!
            Err(()) => None,
        }
    }

    fn current_step(&self) -> SequenceStep {
        SequenceStep {
            row_delta: self.current_row_delta,
            identity: if self.cur_identity_index < self.identities_count {
                Action::Process(self.cur_identity_index)
            } else {
                Action::OuterQuery
            },
        }
    }
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
    pub fn report_progress(
        &mut self,
        progress_in_last_step: bool,
        on_outer_query_row: bool,
        on_last_row: bool,
    ) {
        match self {
            Self::Default(it) => {
                it.report_progress(progress_in_last_step, on_outer_query_row, on_last_row)
            }
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
    identities_count: usize,
    cache: BTreeMap<SequenceCacheKey, Vec<SequenceStep>>,
}

impl ProcessingSequenceCache {
    pub fn new(identities_count: usize) -> Self {
        ProcessingSequenceCache {
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
                    self.identities_count,
                    vec![Direction::Forward, Direction::Backward, Direction::Forward],
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
