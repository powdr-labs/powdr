const DEFAULT_EXECUTION_COUNT_THRESHOLD: u64 = 100;
const DEFAULT_MAX_SEGMENTS: usize = 20;

pub struct OptimisticPrecompileConfig {
    /// For any program line that was not executed at least this many times in the traces,
    /// discard any empirical constraints associated with it.
    pub execution_count_threshold: u64,
    /// The maximum number of segments to keep in memory while detecting empirical constraints.
    /// A higher number here leads to more accurate percentile estimates, but uses more memory.
    pub max_segments: usize,
    /// Whether to restrict empirical constraints to those that are checkable at execution time.
    pub restricted_optimistic_precompiles: bool,
}

pub fn optimistic_precompile_config() -> OptimisticPrecompileConfig {
    let execution_count_threshold = std::env::var("POWDR_OP_EXECUTION_COUNT_THRESHOLD")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_EXECUTION_COUNT_THRESHOLD);
    let max_segments = std::env::var("POWDR_EMPIRICAL_CONSTRAINTS_MAX_SEGMENTS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_MAX_SEGMENTS);
    let restricted_optimistic_precompiles =
        std::env::var("POWDR_RESTRICTED_OPTIMISTIC_PRECOMPILES") == Ok("1".to_string());

    OptimisticPrecompileConfig {
        execution_count_threshold,
        max_segments,
        restricted_optimistic_precompiles,
    }
}
