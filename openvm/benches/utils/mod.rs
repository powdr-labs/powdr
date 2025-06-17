pub mod duration_recorder;
pub mod span_visitor;

use duration_recorder::DurationRecorderLayer;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{EnvFilter, Registry};

/// A simple collector for running isolated benchmarks.
pub struct BenchmarkCollector {
    layer: DurationRecorderLayer,
}

impl BenchmarkCollector {
    /// Create a new, empty collector.
    pub fn new() -> Self {
        let layer = DurationRecorderLayer::default();
        let subscriber = Registry::default()
            .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug")))
            .with(layer.clone())
            .with(tracing_subscriber::fmt::layer());
        tracing::subscriber::set_global_default(subscriber)
            .expect("failed to set global tracing subscriber");

        Self { layer }
    }

    /// Print the timing tree for all collected spans and clear any previously collected data.
    pub fn finish(self) {
        self.layer.finish();
    }
}
