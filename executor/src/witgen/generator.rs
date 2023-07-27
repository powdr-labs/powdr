use ast::analyzed::{Identity, PolynomialReference};
use itertools::Itertools;
use number::{DegreeType, FieldElement};
use parser_util::lines::indent;
use std::collections::{BTreeMap, HashMap};
use std::time::Instant;

use crate::witgen::identity_processor::IdentityProcessor;
use crate::witgen::rows::{RowPair, UpdateStatus};

use super::query_processor::QueryProcessor;
use super::range_constraints::RangeConstraint;

use super::machines::{FixedLookup, Machine};
use super::rows::{Row, RowFactory};
use super::{EvalError, FixedData};

pub struct Generator<'a, T: FieldElement, QueryCallback: Send + Sync> {
    row_factory: RowFactory<'a, T>,
    identity_processor: IdentityProcessor<'a, T>,
    query_processor: QueryProcessor<'a, T, QueryCallback>,
    fixed_data: &'a FixedData<'a, T>,
    identities: &'a [&'a Identity<T>],
    previous: Row<'a, T>,
    /// Values of the witness polynomials
    current: Row<'a, T>,
    /// Values of the witness polynomials in the next row
    next: Row<'a, T>,
    current_row_index: DegreeType,
    failure_reasons: Vec<EvalError<T>>,
    last_report: DegreeType,
    last_report_time: Instant,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum SolvingStrategy {
    /// Only solve expressions that are affine in a single variable
    /// (and use range constraints).
    SingleVariableAffine,
    /// Assume that all unknown values are zero and check that this does not generate
    /// a conflict (but do not store the values as fixed zero to avoid relying on nondeterminism).
    AssumeZero,
}

impl<'a, T: FieldElement, QueryCallback> Generator<'a, T, QueryCallback>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        fixed_lookup: &'a mut FixedLookup<T>,
        identities: &'a [&'a Identity<T>],
        global_range_constraints: BTreeMap<&'a PolynomialReference, RangeConstraint<T>>,
        machines: Vec<Box<dyn Machine<T>>>,
        query_callback: Option<QueryCallback>,
    ) -> Self {
        let polys = fixed_data
            .witness_cols
            .iter()
            .map(|c| &c.poly)
            .collect::<Vec<_>>();
        let row_factory = RowFactory::new(polys, global_range_constraints);
        let query_processor = QueryProcessor::new(fixed_data, query_callback);
        let identity_processor = IdentityProcessor::new(fixed_data, fixed_lookup, machines);
        let default_row = row_factory.fresh_row();

        let mut generator = Generator {
            row_factory,
            query_processor,
            identity_processor,
            fixed_data,
            identities,
            previous: default_row.clone(),
            current: default_row.clone(),
            next: default_row,
            current_row_index: 0,
            failure_reasons: vec![],
            last_report: 0,
            last_report_time: Instant::now(),
        };
        generator.compute_next_row_or_initialize(fixed_data.degree - 1, true);
        generator
    }

    pub fn compute_next_row(&mut self, next_row: DegreeType) -> BTreeMap<usize, T> {
        self.compute_next_row_or_initialize(next_row, false)
    }

    pub fn compute_next_row_or_initialize(
        &mut self,
        next_row: DegreeType,
        initialize: bool,
    ) -> BTreeMap<usize, T> {
        if !initialize {
            self.set_next_row_and_log(next_row);
        } else {
            self.current_row_index = next_row;
        }

        let mut complete_identities = vec![false; self.identities.len()];

        log::trace!("Row: {}", next_row);

        let mut row_pair = RowPair::new(&mut self.current, &mut self.next, next_row);

        let mut identity_failed = false;

        let strategies = if initialize {
            vec![SolvingStrategy::SingleVariableAffine]
        } else {
            vec![
                SolvingStrategy::SingleVariableAffine,
                SolvingStrategy::AssumeZero,
            ]
        };

        for strategy in strategies {
            if identity_failed {
                break;
            }

            if strategy == SolvingStrategy::AssumeZero {
                row_pair.freeze();
            }

            log::trace!("  Strategy: {:?}", strategy);
            loop {
                identity_failed = false;
                self.failure_reasons.clear();

                let mut progress = false;

                for (identity, complete) in self
                    .identities
                    .iter()
                    .zip(complete_identities.iter_mut())
                    .filter(|(_, complete)| !**complete)
                {
                    let result: Result<UpdateStatus, EvalError<T>> = self
                        .identity_processor
                        .process_identity(identity, &mut row_pair)
                        .map_err(|err| {
                            let msg = match strategy {
                                SolvingStrategy::SingleVariableAffine => "Solving failed on",
                                SolvingStrategy::AssumeZero => {
                                    "Assuming zero for unknown columns failed in"
                                }
                            };
                            format!("{msg} {identity}:\n{}", indent(&format!("{err}"), "    "))
                                .into()
                        });

                    match &result {
                        Ok(e) => {
                            *complete = e.is_complete();
                            progress |= e.progress;
                        }
                        Err(e) => {
                            identity_failed = true;
                            self.failure_reasons.push(e.clone());
                        }
                    };
                }

                if strategy == SolvingStrategy::SingleVariableAffine {
                    progress |= self
                        .query_processor
                        .process_queries_on_current_row(&mut row_pair);
                }

                if !progress || identity_failed {
                    break;
                }
            }
        }
        if identity_failed {
            let poly_for_id = |poly_id: usize| {
                let column = self
                    .fixed_data
                    .witness_cols
                    .iter()
                    .find(|column| column.poly.poly_id() as usize == poly_id)
                    .unwrap();
                &column.poly
            };
            let list_undetermined = |row: &Row<T>| {
                row.iter_values()
                    .filter_map(|(poly_id, v)| {
                        if v.is_none() {
                            Some(poly_for_id(poly_id).to_string())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            };

            log::error!(
                "\nError: Row {next_row}: Identity check failed or unable to derive values for some witness columns.\nSet RUST_LOG=debug for more information.");
            log::debug!(
                "\nThe following columns were undetermined in the previous row and might have been needed to derive this row's values:\n{}",
                list_undetermined(&self.current)
            );
            log::debug!(
                "\nThe following columns are still undetermined in the current row:\n{}",
                list_undetermined(&self.next)
            );
            log::debug!(
                "\nReasons:\n{}\n",
                self.failure_reasons
                    .iter()
                    .map(|r| r.to_string())
                    .join("\n\n")
            );
            log::debug!(
                "Determined range constraints for this row:\n{}",
                self.current
                    .iter_range_constraints()
                    .filter_map(|(poly_id, cons)| {
                        cons.as_ref()
                            .map(|cons| format!("  {}: {cons}", poly_for_id(poly_id)))
                    })
                    .join("\n")
            );
            log::debug!(
                "Current values (known nonzero first, then zero, unknown omitted):\n{}",
                self.current.render_values(false)
            );
            panic!("Witness generation failed.");
        }

        log::trace!(
            "{}",
            self.current.render(&format!("===== Row {next_row}"), true)
        );

        self.shift_rows();

        self.previous.clone().into()
    }

    /// Shifts rows: fresh row -> next -> current -> previous
    fn shift_rows(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);
        std::mem::swap(&mut self.next, &mut self.current);
        self.next = self.row_factory.fresh_row();
    }

    /// Verifies the proposed values for the next row.
    /// TODO this is bad for machines because we might introduce rows in the machine that are then
    /// not used.
    pub fn propose_next_row(&mut self, next_row: DegreeType, values: &BTreeMap<usize, T>) -> bool {
        self.set_next_row_and_log(next_row);

        let mut proposed_row = self.row_factory.make_from_known_values(values);

        let constraints_valid = self.check_row_pair(&mut proposed_row, false)
            && self.check_row_pair(&mut proposed_row, true);

        if constraints_valid {
            self.current = proposed_row;
            self.shift_rows();
        }
        constraints_valid
    }

    fn check_row_pair(&mut self, proposed_row: &mut Row<'a, T>, previous: bool) -> bool {
        let mut row_pair = match previous {
            true => RowPair::new(&mut self.previous, proposed_row, self.current_row_index - 1),
            false => RowPair::new(proposed_row, &mut self.next, self.current_row_index),
        };
        row_pair.freeze();

        for identity in self.identities {
            // Split identities into whether or not they have a reference to the next row:
            // - Those that do should be checked on the previous and proposed row
            // - All others should be checked on the proposed row
            if identity.contains_next_ref() == previous
                && self
                    .identity_processor
                    .process_identity(identity, &mut row_pair)
                    .is_err()
            {
                log::debug!("Previous {:?}", self.previous);
                log::debug!("Proposed {:?}", proposed_row);
                log::debug!("Failed on identity: {}", identity);

                return false;
            }
        }
        true
    }

    pub fn machine_witness_col_values(&mut self) -> HashMap<usize, Vec<T>> {
        let mut result: HashMap<_, _> = Default::default();
        let name_to_id = self
            .identity_processor
            .fixed_data
            .witness_cols
            .iter()
            .map(|c| (c.poly.name.as_str(), c.poly.poly_id() as usize))
            .collect::<BTreeMap<_, _>>();
        for m in &mut self.identity_processor.machines {
            for (col_name, col) in m.witness_col_values(self.fixed_data) {
                result.insert(*name_to_id.get(col_name.as_str()).unwrap(), col);
            }
        }
        result
    }

    fn set_next_row_and_log(&mut self, next_row: DegreeType) {
        if next_row != self.fixed_data.degree - 1 && next_row >= self.last_report + 1000 {
            let duration = self.last_report_time.elapsed();
            self.last_report_time = Instant::now();

            log::info!(
                "{next_row} of {} rows ({} %, {} rows per second)",
                self.fixed_data.degree,
                next_row * 100 / self.fixed_data.degree,
                1_000_000_000 / duration.as_micros()
            );
            self.last_report = next_row;
        }
        self.current_row_index = next_row;
    }
}
