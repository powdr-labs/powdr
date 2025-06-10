use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use criterion::{criterion_group, criterion_main, Criterion};
use once_cell::sync::Lazy;
use tracing::{
    field::{Field, Visit},
    Subscriber,
};
use tracing_subscriber::prelude::*;
use tracing_subscriber::{layer::Context, registry::LookupSpan, EnvFilter, Layer, Registry};

use openvm_sdk::StdIn;
use powdr_openvm::{compile_guest, execute_and_generate, GuestOptions, PgoConfig, PowdrConfig};

const GUEST_KECCAK: &str = "guest-keccak";
const GUEST_KECCAK_ITER: u32 = 1000;
const GUEST_KECCAK_APC: u64 = 1;
const GUEST_KECCAK_SKIP: u64 = 0;

/// Map from span-name to Vec<durations>.
static SPAN_TIMES: Lazy<Arc<Mutex<HashMap<String, Vec<Duration>>>>> =
    Lazy::new(|| Arc::new(Mutex::new(HashMap::new())));

/// Map from span-name to its parent span-name (if any).
static SPAN_PARENTS: Lazy<Arc<Mutex<HashMap<String, Option<String>>>>> =
    Lazy::new(|| Arc::new(Mutex::new(HashMap::new())));

struct Visitor {
    pub id: Option<usize>,
    pub air_name: Option<String>,
}

impl Visit for Visitor {
    // Called for fields recorded as string literals (`air_name = %...`).
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "air_name" {
            self.air_name = Some(value.to_string());
        }
    }

    // Called when the macro passes a Debug-able value (`id = id`).
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "id" {
            // Debug formatting of a usize will be just the digits, so we can parse.
            if let Ok(n) = format!("{:?}", value).parse::<usize>() {
                self.id = Some(n);
            }
        }
    }

    // In case it ever comes through as u64 or i64, handle those too
    fn record_u64(&mut self, field: &Field, value: u64) {
        if field.name() == "id" {
            self.id = Some(value as usize);
        }
    }
    fn record_i64(&mut self, field: &Field, value: i64) {
        if field.name() == "id" && value >= 0 {
            self.id = Some(value as usize);
        }
    }

    // We don’t care about the other types
    fn record_bool(&mut self, _field: &Field, _value: bool) {}
    fn record_f64(&mut self, _field: &Field, _value: f64) {}
}

/// A tracing layer that stamps each span with its enter-time, then on exit
/// records the elapsed time into SPAN_TIMES under that span's name.
struct DurationRecorder;

impl<S> Layer<S> for DurationRecorder
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

        // turn fields into strings or None
        let id_str = visitor.id.map(|i| format!("id:{}", i));
        let air_str = visitor.air_name.map(|a| format!("air:{}", a));

        // build the real key: e.g. "dummy trace alu"
        let key = match (air_str, id_str) {
            (Some(air), Some(id)) => format!("{} [{} {}]", base, air, id),
            (Some(air), None) => format!("{} [{}]", base, air),
            (None, Some(id)) => format!("{} [{}]", base, id),
            (None, None) => base.to_string(),
        };

        // grab parent’s key from its extensions, if any
        let parent_key = ctx
            .current_span()
            .id()
            .and_then(|pid| ctx.span(&pid))
            .and_then(|parent_span| parent_span.extensions().get::<String>().cloned());

        // record parent to child (by key)
        SPAN_PARENTS.lock().unwrap().insert(key.clone(), parent_key);

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
                SPAN_TIMES
                    .lock()
                    .unwrap()
                    .entry(key.clone())
                    .or_default()
                    .push(dur);
            }
        }
    }
}

// Install the global subscriber once
static INIT: Lazy<()> = Lazy::new(|| {
    // This reads RUST_LOG (or defaults to "debug" if unset)
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));

    Registry::default()
        .with(filter) // make sure DEBUG spans aren't dropped
        .with(DurationRecorder) // then our custom layer
        .init();
});

/// Build parent to children map and recursively print by name.
fn print_tree() {
    let parents = SPAN_PARENTS.lock().unwrap();
    let times = SPAN_TIMES.lock().unwrap();

    // Build name to set of child names
    let mut tree: HashMap<String, HashSet<String>> = HashMap::new();
    let mut has_parent = HashSet::new();

    for (name, parent_opt) in parents.iter() {
        if let Some(parent) = parent_opt {
            tree.entry(parent.clone()).or_default().insert(name.clone());
            has_parent.insert(name.clone());
        }
    }

    // Roots: those names that never appear as a child
    let mut roots: Vec<_> = parents
        .keys()
        .filter(|n| !has_parent.contains(*n))
        .cloned()
        .collect();
    roots.sort();

    fn recurse(
        name: &str,
        level: usize,
        tree: &HashMap<String, HashSet<String>>,
        times: &HashMap<String, Vec<Duration>>,
    ) {
        let durs = times.get(name).map(|v| v.as_slice()).unwrap_or(&[]);
        let count = durs.len() as u32;
        let total: Duration = durs.iter().copied().sum();
        let avg = if count > 0 {
            total / count
        } else {
            Duration::ZERO
        };

        println!(
            "{:indent$}{} → ran {} times, avg = {:?}",
            "",
            name,
            count,
            avg,
            indent = level * 2
        );

        if let Some(children) = tree.get(name) {
            let mut kids: Vec<_> = children.iter().cloned().collect();
            kids.sort();
            for child in kids {
                recurse(&child, level + 1, tree, times);
            }
        }
    }

    println!("\nSpan timing breakdown:");
    for root in roots {
        recurse(&root, 0, &tree, &times);
    }
}

fn keccak_benchmark(c: &mut Criterion) {
    Lazy::force(&INIT); // required for each function

    // To see the component run times (e.g. powdr chip vs non-powdr chip trace gen time, powdr chip dummy trace gen time for each dummy chip)
    // run with RUST_LOG=debug
    let mut group = c.benchmark_group("keccak-benchmark");

    // Compile the program to execute
    let guest_opts = GuestOptions::default();
    let powdr_config = PowdrConfig::new(GUEST_KECCAK_APC, GUEST_KECCAK_SKIP);
    let program = compile_guest(GUEST_KECCAK, guest_opts, powdr_config, PgoConfig::None).unwrap(); // we don't need PGO because it's guaranteed to run the biggest Keccak basic block without PGO
    let mut stdin = StdIn::default();
    stdin.write(&GUEST_KECCAK_ITER);

    // Default uses 100 samples, which is too many, but 10 samples is the required minimum
    group.sample_size(10);

    // Run benchmark
    group.bench_function("Execute 1000 Keccaks with 1 APC", |b| {
        b.iter(|| {
            execute_and_generate(program.clone(), stdin.clone()).unwrap();
        })
    });

    group.finish();

    // Print the tree of spans and their durations
    print_tree();
}

criterion_group!(execution_benchmarks, keccak_benchmark);
criterion_main!(execution_benchmarks);
