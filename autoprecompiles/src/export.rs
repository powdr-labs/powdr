use std::{
    io::{BufWriter, Write},
    path::PathBuf,
};

use powdr_constraint_solver::constraint_system::ConstraintSystem;
use powdr_number::FieldElement;

use crate::{
    adapter::{Adapter, AdapterApcOverPowdrField, AdapterBasicBlock, AdapterOptimisticConstraints},
    bus_map::BusMap,
    expression::AlgebraicReference,
    symbolic_machine::constraint_system_to_symbolic_machine,
    Apc, ColumnAllocator, SymbolicMachine,
};

/// Configuration for exporting the state of the autoprecompile
/// generation and optimization as json at different stages.
#[derive(Default)]
pub struct ExportOptions {
    pub path: Option<PathBuf>,
    pub level: ExportLevel,
    sequence_number: usize,
}

#[derive(Default)]
pub enum ExportLevel {
    /// Export the unoptimizend and optimizend autoprecompile.
    #[default]
    OnlyAPC,
    /// In addition to the above, also export the state at each
    /// optimization loop iteration.
    APCAndOptimizerLoop,
    /// In addition to the above, also export the state at each
    /// optimization step.
    APCAndOptimizerSteps,
}

impl ExportOptions {
    /// Creates a new export options instance. Does not export anything unless
    /// a path is given. `path` is a path to a file name prefix.
    /// During export, a sequence number and an extension will be appended.
    pub fn new(path: Option<PathBuf>, start_pc: u64, level: ExportLevel) -> Self {
        ExportOptions {
            path: path.map(|p| p.join(format!("apc_candidate_{start_pc}"))),
            level,
            sequence_number: 0,
        }
    }
    /// Constructs export options from environment variables.
    pub fn from_env_vars(
        export_path: Option<String>,
        export_level: Option<String>,
        start_pc: u64,
    ) -> Self {
        let path = export_path.map(PathBuf::from);
        let level = match export_level.as_deref() {
            Some("1") => ExportLevel::OnlyAPC,
            Some("2") => ExportLevel::APCAndOptimizerLoop,
            Some("3") => ExportLevel::APCAndOptimizerSteps,
            _ => ExportLevel::OnlyAPC,
        };
        ExportOptions::new(path, start_pc, level)
    }

    pub fn export_requested(&self) -> bool {
        self.path.is_some()
    }

    pub fn export_apc<A: Adapter>(
        &mut self,
        apc: &AdapterApcOverPowdrField<A>,
        suffix: Option<&str>,
        bus_map: &BusMap<A::CustomBusTypes>,
    ) {
        let path = self.write_to_next_file(apc, suffix);

        // For debugging, also serialize a human-readable version of the final precompile
        let rendered = apc.machine.render(bus_map);
        let path = path.with_file_name(format!(
            "{}.txt",
            path.file_stem().unwrap().to_string_lossy()
        ));
        std::fs::write(path, rendered).unwrap();
    }

    pub fn export_apc_from_machine<A: Adapter>(
        &mut self,
        block: AdapterBasicBlock<A>,
        machine: SymbolicMachine<A::PowdrField>,
        column_allocator: &ColumnAllocator,
        bus_map: &BusMap<A::CustomBusTypes>,
        suffix: Option<&str>,
    ) {
        assert!(self.export_requested());
        let apc = Apc::new(
            block,
            machine,
            AdapterOptimisticConstraints::<A>::empty(),
            column_allocator,
        );
        self.export_apc::<A>(&apc, suffix, bus_map);
    }

    pub fn export_optimizer_outer(&mut self, data: &impl serde::Serialize, suffix: &str) {
        match self.level {
            ExportLevel::APCAndOptimizerLoop | ExportLevel::APCAndOptimizerSteps => {
                self.write_to_next_file(data, Some(suffix));
            }
            _ => {}
        }
    }

    pub fn export_optimizer_outer_constraint_system<T: FieldElement>(
        &mut self,
        constraint_system: &ConstraintSystem<T, AlgebraicReference>,
        suffix: &str,
    ) {
        match self.level {
            ExportLevel::APCAndOptimizerLoop | ExportLevel::APCAndOptimizerSteps => {
                let machine = constraint_system_to_symbolic_machine(constraint_system.clone());
                self.write_to_next_file(&machine, Some(suffix));
            }
            _ => {}
        }
    }

    pub fn export_optimizer_inner(&mut self, data: &impl serde::Serialize, suffix: &str) {
        if let ExportLevel::APCAndOptimizerSteps = self.level {
            self.write_to_next_file(data, Some(suffix));
        }
    }

    pub fn export_optimizer_inner_constraint_system<T, V>(
        &mut self,
        constraint_system: &ConstraintSystem<T, V>,
        suffix: &str,
    ) where
        T: FieldElement,
        V: Ord + Clone + serde::Serialize,
    {
        if let ExportLevel::APCAndOptimizerSteps = self.level {
            todo!()
            // let machine = constraint_system_to_symbolic_machine(constraint_system.clone());
            // self.write_to_next_file(&machine, Some(suffix));
        }
    }

    /// Path to the next file to export to. Uses an increasing sequence number
    /// and also adds the `info` into the file name.
    fn next_path(&mut self, info: Option<&str>) -> PathBuf {
        let seq = self.sequence_number;
        self.sequence_number += 1;
        let path = self.path.clone().unwrap();
        let file_stub = path.file_name().unwrap().to_string_lossy();
        path.with_file_name(format!(
            "{file_stub}_{seq:03}{}.json",
            info.map(|i| format!("_{i}")).unwrap_or_default(),
        ))
    }

    fn write_to_next_file(&mut self, data: &impl serde::Serialize, info: Option<&str>) -> PathBuf {
        let path = self.next_path(info);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        let file = std::fs::File::create(&path).unwrap();
        let mut writer_unopt = BufWriter::new(file);
        serde_json::to_writer(&mut writer_unopt, data).unwrap();
        writer_unopt.flush().unwrap();
        path
    }
}
