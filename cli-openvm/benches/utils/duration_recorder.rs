use crate::utils::span_visitor::Visitor;
use indexmap::IndexMap;
use std::{
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use tracing::Subscriber;
use tracing_subscriber::layer::{Context, Layer};
use tracing_subscriber::registry::LookupSpan;

/// A layer that records span timings and parent relationships into maps.
#[derive(Clone)]
pub struct DurationRecorderLayer {
    span_times: Arc<Mutex<IndexMap<String, Vec<Duration>>>>,
    span_parents: Arc<Mutex<IndexMap<String, Option<String>>>>,
}

impl DurationRecorderLayer {
    /// Create a new collector with empty maps.
    pub fn new() -> Self {
        Self {
            span_times: Arc::new(Mutex::new(IndexMap::new())),
            span_parents: Arc::new(Mutex::new(IndexMap::new())),
        }
    }

    /// Clear all collected statistics.
    pub fn clear(&self) {
        self.span_times.lock().unwrap().clear();
        self.span_parents.lock().unwrap().clear();
    }

    /// Print the hierarchical timing breakdown, preserving insertion order with `IndexMap`.
    pub fn print_tree(&self) {
        let parents = self.span_parents.lock().unwrap();
        let times = self.span_times.lock().unwrap();

        // build parent -> children map
        let mut tree: IndexMap<String, Vec<String>> = IndexMap::new();
        // find roots (names without a parent)
        let mut roots = Vec::new();
        for (name, parent_opt) in parents.iter() {
            if let Some(parent) = parent_opt {
                tree.entry(parent.clone()).or_default().push(name.clone());
            } else {
                roots.push(name.clone());
            }
        }

        fn recurse(
            name: &str,
            level: usize,
            tree: &IndexMap<String, Vec<String>>,
            times: &IndexMap<String, Vec<Duration>>,
        ) {
            let durs = times.get(name).map(|v| v.as_slice()).unwrap_or(&[]);
            let count = durs.len() as u32;
            let total: Duration = durs.iter().copied().sum();
            let avg = if count > 0 {
                total / count
            } else {
                Duration::ZERO
            };
            tracing::debug!(
                "{:indent$}{} → ran {:3} times, avg = {:?}",
                "",
                name,
                count,
                avg,
                indent = level * 4
            );

            if let Some(children) = tree.get(name) {
                for child in children {
                    recurse(child, level + 1, tree, times);
                }
            }
        }

        tracing::debug!("\nSpan timing breakdown:");
        for root in roots {
            recurse(&root, 0, &tree, &times);
        }
    }
}

impl<S> Layer<S> for DurationRecorderLayer
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(
        &self,
        attrs: &tracing::span::Attributes<'_>,
        id: &tracing::Id,
        ctx: Context<'_, S>,
    ) {
        // pull the literal span name (e.g. "dummy trace")
        let base = attrs.metadata().name();
        // visit the fields and extract id
        let mut visitor = Visitor {
            id: None,
            air_name: None,
        };
        attrs.record(&mut visitor);

        // build the real key: e.g. "dummy trace id:1 air_name:alu"
        let key = format!("{base} {visitor}");

        // grab parent’s key from its extensions, if any
        let parent_key = ctx
            .current_span()
            .id()
            .and_then(|pid| ctx.span(pid))
            .and_then(|parent_span| parent_span.extensions().get::<String>().cloned());

        // record parent to child (by key)
        self.span_parents
            .lock()
            .unwrap()
            .insert(key.clone(), parent_key);

        // stash *this* span’s key so on_exit can reuse it
        if let Some(span) = ctx.span(id) {
            span.extensions_mut().insert(key);
        }
    }

    fn on_enter(&self, id: &tracing::Id, ctx: Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            span.extensions_mut().insert(Instant::now());
        }
    }

    fn on_exit(&self, id: &tracing::Id, ctx: Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            let exts = span.extensions();
            if let (Some(start), Some(key)) = (exts.get::<Instant>(), exts.get::<String>()) {
                let dur = start.elapsed();
                self.span_times
                    .lock()
                    .unwrap()
                    .entry(key.clone())
                    .or_default()
                    .push(dur);
            }
        }
    }
}
