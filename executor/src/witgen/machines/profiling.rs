use std::{
    cell::RefCell,
    collections::BTreeMap,
    time::{Duration, Instant},
};

#[derive(PartialEq, Debug, Copy, Clone)]
enum Event {
    Start,
    End,
}

thread_local! {
    /// The event log is a list of (event, <ID>, time) tuples.
    static EVENT_LOG: RefCell<Vec<(Event, usize, Instant)>> = const { RefCell::new(Vec::new()) };
    /// Maps a machine name (assumed to be globally unique) to an ID.
    /// This is done so that we can use a usize in the event log.
    static NAME_TO_ID: RefCell<BTreeMap<String, usize>> = const { RefCell::new(BTreeMap::new()) };
}

/// Returns the ID for a given machine name, creating a new one if necessary.
fn id_from_name(name: &str) -> usize {
    NAME_TO_ID.with(|name_to_id| {
        let mut name_to_id = name_to_id.borrow_mut();
        name_to_id.get(name).copied().unwrap_or_else(|| {
            let id = name_to_id.len();
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

/// Adds the end of a computation to the event log.
pub fn record_end(name: &str) {
    let id = id_from_name(name);
    EVENT_LOG.with(|s| s.borrow_mut().push((Event::End, id, Instant::now())));
}

pub fn reset_and_print_profile_summary() {
    EVENT_LOG.with(|event_log| {
        let id_to_name = NAME_TO_ID.with(|name_to_id| {
            let name_to_id = name_to_id.borrow();
            name_to_id
                .iter()
                .map(|(name, id)| (*id, name.clone()))
                .collect::<BTreeMap<_, _>>()
        });

        // Taking the events out is actually important, because there might be
        // multiple (consecutive) runs of witgen in the same thread.
        let event_log = std::mem::take(&mut (*event_log.borrow_mut()));
        log::debug!("\n == Witgen profile ({} events)", event_log.len());

        // Aggregate time spent in each machine.
        let mut time_by_machine = BTreeMap::new();
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

        for (id, duration) in time_by_machine {
            let percentage = (duration.as_secs_f64() / total_time.as_secs_f64()) * 100.0;
            log::debug!(
                "  {:>5.1}% ({:>8.1?}): {}",
                percentage,
                duration,
                id_to_name[&id]
            );
        }
        log::debug!("  ---------------------------");
        log::debug!("    ==> Total: {:?}", total_time);
        log::debug!("\n");
    });
}
