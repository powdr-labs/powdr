use std::{
    cell::RefCell,
    collections::BTreeMap,
    thread::LocalKey,
    time::{Duration, Instant},
};

use log::log_enabled;
use powdr_number::FieldElement;

use crate::witgen::FixedData;

pub static UNUSED_IDENTITY_ID: u64 = u64::MAX;
pub static IDENTITY_LOOKUP_CACHE: u64 = UNUSED_IDENTITY_ID - 1;
pub static PROCESS_PROVER_QUERIES_ID: u64 = UNUSED_IDENTITY_ID - 2;
pub static SNIPPET1: u64 = UNUSED_IDENTITY_ID - 3;
pub static SNIPPET2: u64 = UNUSED_IDENTITY_ID - 4;

#[derive(Debug)]
struct Event {
    is_start: bool,
    id: u64,
    time: Instant,
}

impl Event {
    fn start(id: u64) -> Self {
        Self {
            is_start: true,
            id,
            time: Instant::now(),
        }
    }

    fn end(id: u64) -> Self {
        Self {
            is_start: false,
            id,
            time: Instant::now(),
        }
    }
}

thread_local! {
    /// The event log is a list of events.
    static EVENT_LOG: RefCell<Vec<Event>> = const { RefCell::new(Vec::new()) };
    static EVENT_LOG_BY_IDENTITY: RefCell<Vec<Event>> = const { RefCell::new(Vec::new()) };
    /// Maps a machine name (assumed to be globally unique) to an ID.
    /// This is done so that we can use a usize in the event log.
    static NAME_TO_ID: RefCell<BTreeMap<String, u64>> = const { RefCell::new(BTreeMap::new()) };
}

/// Returns the ID for a given machine name, creating a new one if necessary.
fn id_from_name(name: &str) -> u64 {
    NAME_TO_ID.with(|name_to_id| {
        let mut name_to_id = name_to_id.borrow_mut();
        name_to_id.get(name).copied().unwrap_or_else(|| {
            let id = name_to_id.len() as u64;
            name_to_id.insert(name.to_string(), id);
            id
        })
    })
}

/// Adds the start of a computation to the event log.
pub fn record_start(name: &str) {
    let id = id_from_name(name);
    EVENT_LOG.with(|s| s.borrow_mut().push(Event::start(id)));
}

pub fn record_start_identity(identity: u64) {
    if log_enabled!(log::Level::Debug) {
        EVENT_LOG_BY_IDENTITY.with(|s| s.borrow_mut().push(Event::start(identity)));
    }
}

/// Adds the end of a computation to the event log.
pub fn record_end(name: &str) {
    let id = id_from_name(name);
    EVENT_LOG.with(|s| s.borrow_mut().push(Event::end(id)));
}

pub fn record_end_identity(identity: u64) {
    if log_enabled!(log::Level::Debug) {
        EVENT_LOG_BY_IDENTITY.with(|s| s.borrow_mut().push(Event::end(identity)));
    }
}

pub fn reset_and_print_profile_summary() {
    if !log_enabled!(log::Level::Debug) {
        return;
    }
    let id_to_name = NAME_TO_ID.with(|name_to_id| {
        let name_to_id = name_to_id.borrow();
        name_to_id
            .iter()
            .map(|(name, id)| (*id, name.clone()))
            .collect::<BTreeMap<_, _>>()
    });
    reset_and_print_profile_summary_impl("Per machine", &EVENT_LOG, id_to_name)
}

pub fn reset_and_print_profile_summary_identity<T: FieldElement>(fixed_data: &FixedData<T>) {
    if !log_enabled!(log::Level::Debug) {
        return;
    }
    let id_to_name = fixed_data
        .analyzed
        .identities
        .iter()
        .map(|identity| (identity.id, format!("{identity}")))
        .chain([(UNUSED_IDENTITY_ID, "other".to_string())])
        .chain([(IDENTITY_LOOKUP_CACHE, "lookup cache creation".to_string())])
        .chain([(
            PROCESS_PROVER_QUERIES_ID,
            "process prover queries".to_string(),
        )])
        .chain([(SNIPPET1, "snippet 1".to_string())])
        .chain([(SNIPPET2, "snippet 2".to_string())])
        .collect();

    reset_and_print_profile_summary_impl(
        "Identities processing",
        &EVENT_LOG_BY_IDENTITY,
        id_to_name,
    )
}

fn reset_and_print_profile_summary_impl(
    name: &str,
    event_log: &'static LocalKey<RefCell<Vec<Event>>>,
    id_to_name: BTreeMap<u64, String>,
) {
    event_log.with(|event_log| {
        // Taking the events out is actually important, because there might be
        // multiple (consecutive) runs of witgen in the same thread.
        let event_log = std::mem::take(&mut (*event_log.borrow_mut()));
        log::debug!("\n == {name} profile ({} events)", event_log.len());

        // Aggregate time spent in each machine.
        let mut time_by_machine = BTreeMap::new();
        let mut machine_invocations = BTreeMap::new();
        assert!(event_log[0].is_start);
        let mut current_time = event_log[0].time;
        let mut call_stack = vec![event_log[0].id];

        for (i, event) in event_log.iter().enumerate().skip(1) {
            // We expect one top-level call, so we should never have an empty call stack.
            let current_machine_id = *call_stack
                .last()
                .unwrap_or_else(|| panic!("Call stack is empty at index {i}, event: {event:?}"));

            // Finish the execution of the currently running machine.
            let duration = event.time.duration_since(current_time);
            *time_by_machine
                .entry(current_machine_id)
                .or_insert(Duration::default()) += duration;
            current_time = event.time;

            // Update the call stack.
            if event.is_start {
                assert!(current_machine_id != event.id, "Unexpected recursive call!");
                call_stack.push(event.id);
            } else {
                assert_eq!(current_machine_id, event.id, "Unexpected end of call!");
                machine_invocations
                    .entry(event.id)
                    .and_modify(|c| *c += 1)
                    .or_insert(1);
                call_stack.pop().unwrap();
            }
        }

        assert!(
            call_stack.is_empty(),
            "Call stack is not empty: {call_stack:?}"
        );

        // Sort by time, descending.
        let mut time_by_machine = time_by_machine.into_iter().collect::<Vec<_>>();
        time_by_machine.sort_by(|a, b| b.1.cmp(&a.1));

        let total_time = time_by_machine.iter().map(|(_, d)| *d).sum::<Duration>();
        assert_eq!(
            event_log
                .last()
                .unwrap()
                .time
                .duration_since(event_log[0].time),
            total_time
        );

        for (i, (id, duration)) in time_by_machine.iter().enumerate() {
            if i > 100 {
                log::debug!("... skipping {} more items", time_by_machine.len() - i);
                break;
            }
            let percentage = (duration.as_secs_f64() / total_time.as_secs_f64()) * 100.0;
            log::debug!(
                "  {percentage:>5.5}% ({duration:>8.5?}), {} runs: {}",
                machine_invocations.get(id).cloned().unwrap_or_default(),
                id_to_name[&id]
                    .to_string()
                    .chars()
                    .take(200)
                    .collect::<String>()
            );
        }
        log::debug!("  ---------------------------");
        log::debug!("    ==> Total: {:?}", total_time);
        log::debug!("\n");
    });
}
