use std::{
    fmt::Display,
    io::{BufWriter, Write},
    path::PathBuf,
};

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::ConstraintSystem;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    adapter::{Adapter, AdapterApcOverPowdrField, AdapterOptimisticConstraints},
    blocks::{BasicBlock, Instruction, PcStep, SuperBlock},
    bus_map::BusMap,
    execution::ExecutionState,
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
    substituted_variables: Vec<String>,
}

#[derive(Default)]
pub enum ExportLevel {
    /// Export the unoptimized and optimized autoprecompile.
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
    pub fn new(path: Option<PathBuf>, start_pcs: &[u64], level: ExportLevel) -> Self {
        ExportOptions {
            path: path.map(|p| p.join(format!("apc_candidate_{}", start_pcs.iter().join("_")))),
            level,
            sequence_number: 0,
            substituted_variables: Vec::new(),
        }
    }
    /// Constructs export options from environment variables.
    pub fn from_env_vars(
        export_path: Option<String>,
        export_level: Option<String>,
        start_pcs: &[u64],
    ) -> Self {
        let path = export_path.map(PathBuf::from);
        let level = match export_level.as_deref() {
            Some("1") => ExportLevel::OnlyAPC,
            Some("2") => ExportLevel::APCAndOptimizerLoop,
            Some("3") => ExportLevel::APCAndOptimizerSteps,
            _ => ExportLevel::OnlyAPC,
        };
        ExportOptions::new(path, start_pcs, level)
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
        let apc = instructions_to_powdr_field::<A>(apc.clone());
        let path = self.write_to_next_file(&ApcWithBusMap { apc: &apc, bus_map }, suffix);

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
        block: SuperBlock<A::Instruction>,
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
            self.write_to_next_file(&constraint_system, Some(suffix));
        }
    }

    /// Registers a sequence of variables that have been substituted during optimization,
    /// so that they can be exported together with the final export.
    pub fn register_substituted_variables<Var, Expr>(
        &mut self,
        vars: impl IntoIterator<Item = (Var, Expr)>,
    ) where
        Var: serde::Serialize,
        Expr: serde::Serialize,
    {
        if self.export_requested() {
            self.substituted_variables.extend(
                vars.into_iter()
                    .map(|(v, e)| serde_json::to_string(&(v, e)).unwrap()),
            );
        }
    }

    /// Exports the registered substituted variables to a separate json file.
    pub fn export_substituted_variables(&mut self) {
        if self.export_requested() {
            let path = self.path.clone().unwrap();
            let file_stub = path.file_name().unwrap().to_string_lossy();
            let path = path.with_file_name(format!("{file_stub}_substitutions.json"));
            let mut writer = create_file_if_not_exists(&path);
            write!(&mut writer, "[{}]", self.substituted_variables.join(",")).unwrap();
            writer.flush().unwrap();
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
        self.write_to_file(data, path.clone());
        path
    }

    fn write_to_file(&mut self, data: &impl serde::Serialize, path: PathBuf) {
        let mut writer = create_file_if_not_exists(&path);
        serde_json::to_writer(&mut writer, data).unwrap();
        writer.flush().unwrap();
    }
}

fn create_file_if_not_exists(path: &PathBuf) -> BufWriter<std::fs::File> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    BufWriter::new(std::fs::File::create(path).unwrap())
}

/// Converts the APC to use an instruction type that stores field elements
/// using a powdr type, so that we do not need to export in Montgomery form.
#[allow(clippy::type_complexity)]
fn instructions_to_powdr_field<A: Adapter>(
    apc: AdapterApcOverPowdrField<A>,
) -> Apc<
    <A as Adapter>::PowdrField,
    SimpleInstruction<<A as Adapter>::PowdrField>,
    <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
    <<A as Adapter>::ExecutionState as ExecutionState>::Value,
> {
    let blocks: Vec<_> = apc
        .block
        .blocks()
        .map(|b| {
            BasicBlock {
                start_pc: b.start_pc,
                instructions: b
                    .instructions
                    .iter()
                    .map(|instr| {
                        SimpleInstruction(
                            // Extract the data by providing a dummy pc
                            // and removing it again.
                            instr
                                .pc_lookup_row(778)
                                .iter()
                                .skip(1)
                                .map(|x| A::from_field(x.clone()))
                                .collect(),
                        )
                    })
                    .collect(),
            }
        })
        .collect();

    let block = SuperBlock::from(blocks);

    Apc {
        block,
        machine: apc.machine,
        subs: apc.subs,
        optimistic_constraints: apc.optimistic_constraints,
    }
}

/// Dummy instruction type that is used to store the converted field type.
#[derive(Serialize, Deserialize, Clone)]
pub struct SimpleInstruction<T>(Vec<T>);

impl<T: Display> Display for SimpleInstruction<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().format(", "))
    }
}

impl<T: Display + Clone> Instruction<T> for SimpleInstruction<T> {
    fn pc_lookup_row(&self, _pc: u64) -> Vec<T> {
        self.0.clone()
    }
}

impl<T> PcStep for SimpleInstruction<T> {
    fn pc_step() -> u32 {
        unimplemented!()
    }
}

#[derive(Serialize, Deserialize)]
pub struct ApcWithBusMap<Apc, BusMap> {
    #[serde(flatten)]
    pub apc: Apc,
    pub bus_map: BusMap,
}
