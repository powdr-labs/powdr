pub mod duration_recorder;
pub mod span_visitor;

use duration_recorder::DurationRecorderLayer;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{EnvFilter, Registry};

/// A simple collector for running isolated benchmarks.
pub struct BenchmarkCollector {
    layer: DurationRecorderLayer,
    is_installed: bool,
}

impl BenchmarkCollector {
    /// Create a new, empty collector.
    pub fn new() -> Self {
        Self {
            layer: DurationRecorderLayer::new(),
            is_installed: false,
        }
    }

    /// Install the subscriber globally. Call once before any spans are created.
    pub fn install(&mut self) {
        if !self.is_installed {
            let subscriber = Registry::default()
                .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug")))
                .with(self.layer.clone())
                .with(tracing_subscriber::fmt::layer());
            tracing::subscriber::set_global_default(subscriber)
                .expect("failed to set global tracing subscriber");
            self.is_installed = true;
        }
    }

    /// Clear any previously collected data.
    pub fn clear(&self) {
        self.layer.clear();
    }

    /// Print the timing tree for all collected spans.
    pub fn print_tree(&self) {
        self.layer.print_tree();
    }
}
