use std::{
    io::BufWriter,
    path::{Path, PathBuf},
};

use crate::{
    adapter::{Adapter, AdapterApcOverPowdrField, AdapterBasicBlock, AdapterOptimisticConstraints},
    bus_map::BusMap,
    Apc, ColumnAllocator, SymbolicMachine,
};

/// Configuration for exporting the state of the autoprecompile
/// generation and optimization as json at different stages.
pub struct ExportOptions {
    pub path: Option<PathBuf>,
    pub level: ExportLevel,
    // TODO format json or cbor?
}

pub enum ExportLevel {
    /// Export the unoptimizend and optimizend autoprecompile.
    OnlyAPC,
    /// In addition to the above, also export the state at each
    /// optimization loop iteration.
    APCAndOptimizerLoop,
    /// In addition to the above, also export the state at each
    /// optimization step.
    APCAndOptimizerSteps,
}

impl ExportOptions {
    /// Constructs export options from environment variables.
    pub fn from_env_vars(export_path: Option<String>, export_level: Option<String>) -> Self {
        let path = export_path.map(PathBuf::from);
        let level = match export_level.as_deref() {
            Some("1") => ExportLevel::OnlyAPC,
            Some("2") => ExportLevel::APCAndOptimizerLoop,
            Some("3") => ExportLevel::APCAndOptimizerSteps,
            _ => ExportLevel::OnlyAPC,
        };
        ExportOptions { path, level }
    }

    pub fn export_requested(&self) -> bool {
        self.path.is_some()
    }
}

fn make_path(base_path: &Path, start_pc: u64, suffix: Option<&str>, extension: &str) -> PathBuf {
    let suffix = suffix.map(|s| format!("_{s}")).unwrap_or_default();
    Path::new(base_path)
        .join(format!("apc_candidate_{start_pc}{suffix}"))
        .with_extension(extension)
}

pub fn export_apc<A: Adapter>(
    apc: &AdapterApcOverPowdrField<A>,
    export_options: &ExportOptions,
    suffix: Option<&str>,
    bus_map: &BusMap<A::CustomBusTypes>,
) {
    let path = export_options.path.as_ref().unwrap();

    std::fs::create_dir_all(path).expect("Failed to create directory for APC candidates");

    let ser_path = make_path(path, apc.start_pc(), suffix, "json");
    let file_unopt =
        std::fs::File::create(&ser_path).expect("Failed to create file for {suffix} APC candidate");
    let writer_unopt = BufWriter::new(file_unopt);
    serde_json::to_writer(writer_unopt, &apc)
        .expect("Failed to write {suffix} APC candidate to file");

    // For debugging, also serialize a human-readable version of the final precompile
    let rendered = apc.machine.render(bus_map);
    let path = make_path(path, apc.start_pc(), None, "txt");
    std::fs::write(path, rendered).unwrap();
}

pub fn export_apc_from_machine<A: Adapter>(
    block: AdapterBasicBlock<A>,
    machine: SymbolicMachine<A::PowdrField>,
    column_allocator: &ColumnAllocator,
    bus_map: &BusMap<A::CustomBusTypes>,
    export_options: &ExportOptions,
    suffix: Option<&str>,
) {
    assert!(export_options.export_requested());
    let apc = Apc::new(
        block,
        machine,
        AdapterOptimisticConstraints::<A>::empty(),
        column_allocator,
    );
    export_apc::<A>(&apc, export_options, suffix, bus_map);
}
