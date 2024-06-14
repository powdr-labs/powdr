use std::{
    cell::RefCell,
    collections::BTreeMap,
    thread::LocalKey,
    time::{Duration, Instant},
};

use powdr_number::FieldElement;

use crate::witgen::{
    FixedData, IDENTITY_FINALIZE_ID, IDENTITY_LOOKUP_CACHE, IDENTITY_SNIPPET2_ID,
    IDENTITY_SNIPPET_ID, PROCESS_OUTER_QUERY_ID, PROCESS_PROVER_QUERIES_ID, UNUSED_IDENTITY_ID,
};

#[derive(PartialEq, Debug, Copy, Clone)]
enum Event {
    Start,
    End,
}

thread_local! {
    /// The event log is a list of (event, <ID>, time) tuples.
    static EVENT_LOG: RefCell<Vec<(Event, u64, Instant)>> = const { RefCell::new(Vec::new()) };
    static EVENT_LOG_BY_IDENTITY: RefCell<Vec<(Event, u64, Instant)>> = const { RefCell::new(Vec::new()) };
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
    EVENT_LOG.with(|s| s.borrow_mut().push((Event::Start, id, Instant::now())));
}

pub fn record_start_identity(identity: u64) {
    EVENT_LOG_BY_IDENTITY.with(|s| {
        s.borrow_mut()
            .push((Event::Start, identity, Instant::now()))
    });
}

/// Adds the end of a computation to the event log.
pub fn record_end(name: &str) {
    let id = id_from_name(name);
    EVENT_LOG.with(|s| s.borrow_mut().push((Event::End, id, Instant::now())));
}

pub fn record_end_identity(identity: u64) {
    EVENT_LOG_BY_IDENTITY.with(|s| s.borrow_mut().push((Event::End, identity, Instant::now())));
}
pub fn reset_and_print_profile_summary() {
    let id_to_name = NAME_TO_ID.with(|name_to_id| {
        let name_to_id = name_to_id.borrow();
        name_to_id
            .iter()
            .map(|(name, id)| (*id, name.clone()))
            .collect::<BTreeMap<_, _>>()
    });
    reset_and_print_profile_summary_impl(&EVENT_LOG, id_to_name)
}
pub fn reset_and_print_profile_summary_identity<T: FieldElement>(fixed_data: &FixedData<T>) {
    let id_to_name = fixed_data
        .analyzed
        .identities
        .iter()
        .map(|identity| (identity.id, format!("{identity}")))
        .chain([(UNUSED_IDENTITY_ID, "other".to_string())])
        .chain([(IDENTITY_LOOKUP_CACHE, "lookup cache creation".to_string())])
        .chain([(IDENTITY_SNIPPET_ID, "snippet1".to_string())])
        .chain([(IDENTITY_SNIPPET2_ID, "snippet2".to_string())])
        .chain([(IDENTITY_FINALIZE_ID, "finalize".to_string())])
        .chain([(PROCESS_OUTER_QUERY_ID, "process outer query".to_string())])
        .chain([(
            PROCESS_PROVER_QUERIES_ID,
            "process prover queries".to_string(),
        )])
        .collect();

    reset_and_print_profile_summary_impl(&EVENT_LOG_BY_IDENTITY, id_to_name)
}

fn reset_and_print_profile_summary_impl(
    event_log: &'static LocalKey<RefCell<Vec<(Event, u64, Instant)>>>,
    id_to_name: BTreeMap<u64, String>,
) {
    event_log.with(|event_log| {
        // Taking the events out is actually important, because there might be
        // multiple (consecutive) runs of witgen in the same thread.
        let event_log = std::mem::take(&mut (*event_log.borrow_mut()));
        log::debug!("\n == Witgen profile ({} events)", event_log.len());

        // Aggregate time spent in each machine.
        let mut time_by_machine = BTreeMap::new();
        let mut machine_invocations = BTreeMap::new();
        assert_eq!(event_log[0].0, Event::Start);
        let mut current_time = event_log[0].2;
        let mut call_stack = vec![event_log[0].1];

        for (i, &(event, id, time)) in event_log.iter().enumerate().skip(1) {
            // We expect one top-level call, so we should never have an empty call stack.
            let current_machine_id = *call_stack.last().unwrap_or_else(|| {
                panic!(
                    "Call stack is empty at index {i} (event: {event:?}, name: {id}, time: {time:?})"
                )
            });

            // Finish the execution of the currently running machine.
            let duration = time.duration_since(current_time);
            *time_by_machine
                .entry(current_machine_id)
                .or_insert(Duration::default()) += duration;
            current_time = time;

            // Update the call stack.
            match event {
                Event::Start => {
                    assert!(current_machine_id != id, "Unexpected recursive call!");
                    machine_invocations
                        .entry(id)
                        .and_modify(|c| *c += 1)
                        .or_insert(1);
                    call_stack.push(id);
                }
                Event::End => {
                    assert_eq!(current_machine_id, id, "Unexpected end of call!");
                    call_stack.pop().unwrap();
                }
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
            event_log.last().unwrap().2.duration_since(event_log[0].2),
            total_time
        );

        for (i, (id, duration)) in time_by_machine.iter().enumerate() {
            if i > 50 {
                break;
            }
            let percentage = (duration.as_secs_f64() / total_time.as_secs_f64()) * 100.0;
            log::debug!(
                "  {:>5.5}% ({:>8.5?}), {} runs: {}",
                percentage,
                duration,
                machine_invocations.get(id).cloned().unwrap_or_default(),
                id_to_name[&id].to_string().chars().take(200).collect::<String>()
            );
        }
        log::debug!("  ---------------------------");
        log::debug!("    ==> Total: {:?}", total_time);
        log::debug!("\n");
    });
}
